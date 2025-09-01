library(sf)
library(dplyr)

# SF Fisheye Bezel Transformation - Robust Version
sf_fisheye_bezel <- function(sf_obj, cx = NULL, cy = NULL,
                             r_in = 50000, r_out = 100000,
                             zoom = 2.0, bulge = 0.35,
                             target_crs = NULL) {

  # Input validation
  stopifnot(r_out > r_in, zoom > 1)
  stopifnot(inherits(sf_obj, c("sf", "sfc")))

  # Clean and prepare the data
  cat("🔧 Preparing data...\n")

  # Remove empty geometries
  if (inherits(sf_obj, "sf")) {
    valid_geoms <- !st_is_empty(sf_obj)
    sf_obj <- sf_obj[valid_geoms, ]
    cat("   Removed", sum(!valid_geoms), "empty geometries\n")
  }

  # Ensure 2D coordinates only
  sf_obj <- st_zm(sf_obj, drop = TRUE, what = "ZM")
  cat("   Forced to 2D coordinates\n")

  # Handle CRS transformation
  original_crs <- st_crs(sf_obj)
  working_crs <- original_crs

  if (is.null(target_crs)) {
    if (st_is_longlat(sf_obj)) {
      # Auto-select appropriate projected CRS
      bbox <- st_bbox(sf_obj)
      lon_center <- mean(c(bbox["xmin"], bbox["xmax"]))
      lat_center <- mean(c(bbox["ymin"], bbox["ymax"]))

      # For Melbourne area, use GDA2020 MGA Zone 55
      if (lon_center > 140 && lon_center < 150 && lat_center > -40 && lat_center < -30) {
        working_crs <- "EPSG:7855"  # GDA2020 MGA Zone 55
      } else {
        # General UTM zone calculation
        utm_zone <- floor((lon_center + 180) / 6) + 1
        if (lat_center >= 0) {
          working_crs <- paste0("EPSG:", 32600 + utm_zone)  # Northern hemisphere
        } else {
          working_crs <- paste0("EPSG:", 32700 + utm_zone)  # Southern hemisphere
        }
      }

      sf_obj <- st_transform(sf_obj, working_crs)
      cat("   Transformed to projected CRS:", working_crs, "\n")
    }
  } else {
    sf_obj <- st_transform(sf_obj, target_crs)
    working_crs <- target_crs
  }

  # Calculate center point
  bbox <- st_bbox(sf_obj)
  if (is.null(cx)) cx <- mean(c(bbox["xmin"], bbox["xmax"]))
  if (is.null(cy)) cy <- mean(c(bbox["ymin"], bbox["ymax"]))

  cat("   Center point: (", round(cx), ",", round(cy), ")\n")
  cat("   Radii: inner =", r_in, "m, outer =", r_out, "m\n")

  # The fisheye transformation function
  fisheye_transform <- function(coords_matrix) {
    # Safety checks
    if (is.null(coords_matrix) || nrow(coords_matrix) == 0) {
      return(matrix(numeric(0), ncol = 2))
    }

    # Ensure we have exactly 2 columns
    if (ncol(coords_matrix) < 2) {
      stop("Coordinates must have at least 2 columns (x, y)")
    }

    # Take only first 2 columns (x, y)
    coords <- coords_matrix[, 1:2, drop = FALSE]

    # Calculate polar coordinates
    dx <- coords[, 1] - cx
    dy <- coords[, 2] - cy
    rho <- sqrt(dx^2 + dy^2) + 1e-9  # small epsilon to avoid division by zero
    theta <- atan2(dy, dx)

    # Normalize radial coordinate in bezel zone [0,1]
    u <- pmin(pmax((rho - r_in) / (r_out - r_in), 0), 1)

    # Smooth step function for C1 continuity
    smoothstep <- function(t) t * t * (3 - 2 * t)
    s <- smoothstep(u)

    # Base transformation: blend between zoom and original
    R_base <- (1 - s) * (rho / zoom) + s * rho

    # Optional bulge effect
    if (bulge > 0) {
      k <- max(1e-6, 1 - bulge)
      b <- u / (u + k * (1 - u))
      sb <- smoothstep(pmin(pmax(b, 0), 1))
      R_bulged <- (1 - sb) * (rho / zoom) + sb * rho

      alpha <- min(max(bulge, 0), 1)
      R <- (1 - alpha) * R_base + alpha * R_bulged
    } else {
      R <- R_base
    }

    # Apply zone-based transformation
    R_final <- ifelse(rho <= r_in, rho / zoom,       # Core: zoom
                      ifelse(rho >= r_out, rho, R))   # Bezel: blend, Outside: original

    # Convert back to Cartesian
    new_x <- cx + R_final * cos(theta)
    new_y <- cy + R_final * sin(theta)

    return(cbind(new_x, new_y))
  }

  # Apply transformation using st_transform with custom function
  result <- st_transform_custom(sf_obj, fisheye_transform)

  # Transform back to original CRS if different
  if (!identical(st_crs(result), original_crs)) {
    result <- st_transform(result, original_crs)
  }

  cat("✅ Transformation complete!\n")
  return(result)
}

# Custom transformation function that handles sf objects properly
st_transform_custom <- function(sf_obj, transform_fun) {

  # Ensure polygon closure
  ensure_closed <- function(coords) {
    if (nrow(coords) < 3) return(coords)

    # Force closure: last point = first point
    coords[nrow(coords), ] <- coords[1, ]
    return(coords)
  }

  # Process individual geometries
  transform_single_geom <- function(geom) {

    tryCatch({
      # Handle different geometry types
      geom_type <- st_geometry_type(geom)

      if (geom_type == "POINT") {
        coords <- st_coordinates(geom)[, 1:2, drop = FALSE]
        if (nrow(coords) == 0) return(st_point())

        new_coords <- transform_fun(coords)
        return(st_point(c(new_coords[1, 1], new_coords[1, 2])))

      } else if (geom_type == "LINESTRING") {
        coords <- st_coordinates(geom)[, 1:2, drop = FALSE]
        if (nrow(coords) == 0) return(st_linestring())

        new_coords <- transform_fun(coords)
        return(st_linestring(new_coords))

      } else if (geom_type == "POLYGON") {
        # Get coordinates with ring information
        coords_full <- st_coordinates(geom)
        if (nrow(coords_full) == 0) return(st_polygon())

        # Handle multiple rings (exterior + holes)
        if ("L1" %in% colnames(coords_full)) {
          # Split by ring
          ring_list <- split(as.data.frame(coords_full), coords_full[, "L1"])

          transformed_rings <- lapply(ring_list, function(ring_df) {
            ring_coords <- as.matrix(ring_df[, c("X", "Y")])
            new_coords <- transform_fun(ring_coords)
            ensure_closed(new_coords)
          })

          return(st_polygon(transformed_rings))

        } else {
          # Single ring polygon
          coords <- coords_full[, 1:2, drop = FALSE]
          new_coords <- transform_fun(coords)
          new_coords <- ensure_closed(new_coords)
          return(st_polygon(list(new_coords)))
        }

      } else if (geom_type == "MULTIPOLYGON") {
        # Handle multipolygons
        coords_full <- st_coordinates(geom)
        if (nrow(coords_full) == 0) return(st_multipolygon())

        # Split by polygon (L2) and ring (L1)
        poly_list <- split(as.data.frame(coords_full), coords_full[, "L2"])

        transformed_polys <- lapply(poly_list, function(poly_df) {
          if ("L1" %in% colnames(poly_df)) {
            ring_list <- split(poly_df, poly_df[, "L1"])
            transformed_rings <- lapply(ring_list, function(ring_df) {
              ring_coords <- as.matrix(ring_df[, c("X", "Y")])
              new_coords <- transform_fun(ring_coords)
              ensure_closed(new_coords)
            })
            return(transformed_rings)
          } else {
            coords <- as.matrix(poly_df[, c("X", "Y")])
            new_coords <- transform_fun(coords)
            return(list(ensure_closed(new_coords)))
          }
        })

        return(st_multipolygon(transformed_polys))

      } else {
        # For other geometry types, return empty
        return(st_polygon())
      }

    }, error = function(e) {
      warning("Failed to transform geometry: ", e$message)
      return(st_polygon())  # Return empty on error
    })
  }

  # Apply to all geometries
  if (inherits(sf_obj, "sf")) {
    geom_col <- attr(sf_obj, "sf_column")

    new_geometries <- lapply(sf_obj[[geom_col]], transform_single_geom)
    sf_obj[[geom_col]] <- st_sfc(new_geometries, crs = st_crs(sf_obj))

    return(sf_obj)

  } else if (inherits(sf_obj, "sfc")) {
    new_geometries <- lapply(sf_obj, transform_single_geom)
    return(st_sfc(new_geometries, crs = st_crs(sf_obj)))
  }

  return(sf_obj)
}

# Quick diagnostic function
diagnose_sf <- function(sf_obj) {
  cat("🔍 SF Object Diagnosis:\n")
  cat("   Geometry types:", paste(unique(st_geometry_type(sf_obj)), collapse = ", "), "\n")
  cat("   CRS:", as.character(st_crs(sf_obj)$input), "\n")
  cat("   Bounding box:\n")
  print(st_bbox(sf_obj))
  cat("   Empty geometries:", sum(st_is_empty(sf_obj)), "/", nrow(sf_obj), "\n")
  cat("   Valid geometries:", sum(st_is_valid(sf_obj)), "/", nrow(sf_obj), "\n")

  # Check dimensions
  sample_coords <- st_coordinates(sf_obj[1, ])
  cat("   Coordinate dimensions:", ncol(sample_coords), "\n")
  cat("   Sample coordinates (first 3 rows):\n")
  print(head(sample_coords, 3))
}

# Helper function to find center coordinates
find_melbourne_center <- function(vic_data, method = "cbd") {

  if (method == "cbd") {
    # Melbourne CBD coordinates
    if (st_is_longlat(vic_data)) {
      return(c(x = 144.9631, y = -37.8136))  # Flinders Street Station
    } else {
      # Transform CBD point to match data CRS
      cbd_point <- st_point(c(144.9631, -37.8136)) %>%
        st_sfc(crs = 4326) %>%
        st_transform(st_crs(vic_data))
      coords <- st_coordinates(cbd_point)
      return(c(x = coords[1], y = coords[2]))
    }

  } else if (method == "metro_centroid") {
    # Find Melbourne metro area and get its centroid
    melbourne_lgas <- c("MELBOURNE", "PORT PHILLIP", "YARRA", "STONNINGTON",
                        "GLEN EIRA", "BAYSIDE", "KINGSTON", "FRANKSTON",
                        "GREATER DANDENONG", "MONASH", "KNOX", "MAROONDAH",
                        "WHITEHORSE", "BOROONDARA", "MANNINGHAM", "DAREBIN",
                        "BANYULE", "NILLUMBIK", "WHITTLESEA", "MITCHELL",
                        "MORELAND", "MOONEE VALLEY", "MARIBYRNONG", "HOBSONS BAY",
                        "WYNDHAM", "MELTON", "BRIMBANK", "HUME", "CARDINIA",
                        "CASEY", "MORNINGTON PENINSULA")

    if ("LGA_NAME" %in% names(vic_data)) {
      metro_areas <- vic_data %>%
        filter(LGA_NAME %in% melbourne_lgas)

      if (nrow(metro_areas) > 0) {
        centroid <- st_centroid(st_union(metro_areas))
        coords <- st_coordinates(centroid)
        return(c(x = coords[1], y = coords[2]))
      }
    }

    # Fallback to CBD method
    return(find_melbourne_center(vic_data, "cbd"))

  } else if (method == "custom") {
    # Return NULL so user must specify cx, cy manually
    return(NULL)
  }
}

# Melbourne-focused fisheye function
melbourne_fisheye <- function(vic_data,
                              center_method = "cbd",
                              cx = NULL, cy = NULL,
                              focus_radius_km = 25,      # Inner radius in km
                              context_radius_km = 150,   # Outer radius in km
                              zoom = 2.5,
                              bulge = 0.4) {

  cat("🏙️  Creating Melbourne-focused fisheye map\n")

  # Clean data
  vic_clean <- vic_data %>%
    filter(!st_is_empty(geometry)) %>%
    st_make_valid() %>%
    st_zm(drop = TRUE, what = "ZM")

  # Determine center point
  if (is.null(cx) || is.null(cy)) {
    center_coords <- find_melbourne_center(vic_clean, center_method)
    if (is.null(center_coords)) {
      stop("Must provide cx and cy when using 'custom' center method")
    }
    cx <- center_coords["x"]
    cy <- center_coords["y"]
  }

  cat("   Focus center: (", round(cx, 4), ",", round(cy, 4), ")\n")

  # Convert km to appropriate units
  if (st_is_longlat(vic_clean)) {
    # Rough conversion: 1 degree ≈ 111 km at Melbourne latitude
    r_in <- focus_radius_km / 111
    r_out <- context_radius_km / 111
    cat("   Using geographic coordinates (degrees)\n")
  } else {
    # Assume projected CRS with meter units
    r_in <- focus_radius_km * 1000
    r_out <- context_radius_km * 1000
    cat("   Using projected coordinates (meters)\n")
  }

  cat("   Focus radius:", round(r_in), ", Context radius:", round(r_out), "\n")

  # Apply the fisheye transformation
  result <- sf_fisheye_bezel(vic_clean,
                             cx = cx, cy = cy,
                             r_in = r_in, r_out = r_out,
                             zoom = zoom, bulge = bulge)

  return(result)
}

# Usage examples
demo_melbourne_fisheye <- function(vic_data) {

  # Method 1: Focus on Melbourne CBD
  cat("\n🎯 Method 1: CBD Focus\n")
  fisheye_cbd <- melbourne_fisheye(vic_data,
                                   center_method = "cbd",
                                   focus_radius_km = 20,
                                   context_radius_km = 120,
                                   zoom = 3.0)

  # Method 2: Focus on Melbourne metro centroid
  cat("\n🎯 Method 2: Metro Centroid Focus\n")
  fisheye_metro <- melbourne_fisheye(vic_data,
                                     center_method = "metro_centroid",
                                     focus_radius_km = 30,
                                     context_radius_km = 100,
                                     zoom = 2.2)

  # Method 3: Custom focus point
  cat("\n🎯 Method 3: Custom Focus\n")
  # Example: Focus on Geelong (if using geographic coordinates)
  fisheye_custom <- melbourne_fisheye(vic_data,
                                      center_method = "custom",
                                      cx = 144.3600, cy = -38.1499,  # Geelong
                                      focus_radius_km = 15,
                                      context_radius_km = 80,
                                      zoom = 2.8)

  return(list(cbd = fisheye_cbd, metro = fisheye_metro, custom = fisheye_custom))
}
