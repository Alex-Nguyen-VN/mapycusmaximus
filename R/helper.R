
prepare_sf <- function(sf_obj) {
  # Remove empty geometries
  if (inherits(sf_obj, "sf")) {
    valid_geoms <- !st_is_empty(sf_obj)
    sf_obj <- sf_obj[valid_geoms, ]
  }
  # Ensure 2D coordinates only
  sf_obj <- st_zm(sf_obj, drop = TRUE, what = "ZM")
  original_crs <- st_crs(sf_obj)
  bbox <- st_bbox(sf_obj)
  coords <- st_coordinates(sf_obj)
  return(list(original_crs = original_crs, bbox = bbox, coords = coords))
}

prepare_sf(vic)

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

