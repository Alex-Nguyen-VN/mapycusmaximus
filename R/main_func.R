# SF Fisheye  Transformation - Robust Version
sf_fisheye <- function(sf_obj, cx = 0, cy = 0,
                             r_in = 0.34, r_out = 0.5,
                             zoom_factor = 1.5, squeeze_factor = 0.35,
                             revolution = 0.0, target_crs = NULL) {

  # Input validation
  stopifnot(r_out > r_in)
  stopifnot(inherits(sf_obj, c("sf", "sfc")))

  # Remove empty geometries
  if (inherits(sf_obj, "sf")) {
    valid_geoms <- !st_is_empty(sf_obj)
    sf_obj <- sf_obj[valid_geoms, ]
  }

  args <- list(cx = cx, cy = cy,
               r_in = r_in, r_out = r_out,
               zoom_factor = zoom_factor, squeeze_factor = squeeze_factor,
               revolution = revolution)

  # Ensure 2D coordinates only
  sf_obj <- st_zm(sf_obj, drop = TRUE, what = "ZM")

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
    }
  } else {
    sf_obj <- st_transform(sf_obj, target_crs)
    working_crs <- target_crs
  }

  # Calculate center point
  bbox <- st_bbox(sf_obj)
  if (is.null(cx)) cx <- mean(c(bbox["xmin"], bbox["xmax"]))
  if (is.null(cy)) cy <- mean(c(bbox["ymin"], bbox["ymax"]))

  # Apply transformation using st_transform with custom function
  result <- st_transform_custom(sf_obj, fisheye_fgc, args)

  # Transform back to original CRS if different
  if (!identical(st_crs(result), original_crs)) {
    result <- st_transform(result, original_crs)
  }

  return(result)
}

