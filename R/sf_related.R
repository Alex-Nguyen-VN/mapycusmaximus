#' Apply a custom coordinate transform to an sf/sfc object (POINT/LINESTRING/POLYGON/MULTIPOLYGON)
#'
#' @description
#' `st_transform_custom()` walks through each geometry in an `sf`/`sfc` object,
#' extracts its XY coordinates, applies a user-supplied transformation function
#' to those coordinates, and rebuilds the geometry. It preserves the input CRS
#' on the resulting `sfc` column. Polygon rings are re-closed after
#' transformation so the first and last vertex match.
#'
#' @param sf_obj An object of class [`sf`] or [`sfc`]. Supported geometry types:
#'   `POINT`, `LINESTRING`, `POLYGON`, and `MULTIPOLYGON`.
#' @param transform_fun A function that accepts a numeric matrix of coordinates
#'   with two columns `(X, Y)` and returns a transformed numeric matrix with the
#'   same number of rows and two columns. For example:
#'   `function(coords, ...) cbind(f(coords[,1], ...), g(coords[,2], ...))`.
#' @param args A named list of additional arguments to pass to `transform_fun`.
#'   These are appended after the `coords` matrix via `do.call()`, i.e.
#'   `do.call(transform_fun, c(list(coords), args))`.
#'
#' @details
#' For `POLYGON`/`MULTIPOLYGON`, the function uses the ring indices returned by
#' [`sf::st_coordinates()`] (`L1` for rings and `L2` for parts) to transform each
#' ring independently, and then ensures each ring is explicitly closed
#' (last vertex equals first vertex).
#'
#' Error handling is per-geometry: if a geometry fails to transform, a warning
#' is emitted and an empty geometry of the same "polygonal family" is returned
#' to keep list lengths consistent.
#'
#' The function **does not** modify or interpret the CRS numerically; it simply
#' preserves the CRS attribute on the output `sfc`. If your transformation
#' assumes metres (e.g., radial warps), ensure the input is in an appropriate
#' projected CRS before calling this function.
#'
#' @return
#' An object of the same top-level class as `sf_obj` (`sf` or `sfc`), with the
#' same column structure (if `sf`) and the same CRS as the input. Geometry
#' coordinates are replaced by the coordinates returned by `transform_fun`.
#'
#' @section Expected signature of `transform_fun`:
#' `transform_fun <- function(coords, ...) {  ## coords: n x 2 matrix (X, Y)
#'   ## return an n x 2 matrix with transformed (X, Y)
#' }`
#'
#' @examples
#' library(sf)
#'
#' # A simple coordinate transformer: scale and shift
#' scale_shift <- function(coords, sx = 1, sy = 1, dx = 0, dy = 0) {
#'   X <- coords[, 1] * sx + dx
#''  Y <- coords[, 2] * sy + dy
#'   cbind(X, Y)
#' }
#'
#' # POINT example
#' pt <- st_sfc(st_point(c(0, 0)), crs = 3857)
#' st_transform_custom(pt, transform_fun = scale_shift,
#'                     args = list(sx = 2, sy = 2, dx = 1000, dy = -500))
#'
#' # LINESTRING example
#' ln <- st_sfc(st_linestring(rbind(c(0, 0), c(1, 0), c(1, 1))), crs = 3857)
#' st_transform_custom(ln, transform_fun = scale_shift,
#'                     args = list(sx = 10, sy = 10))
#'
#' # POLYGON example (unit square)
#' poly <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1),
#'                                      c(0,1), c(0,0)))), crs = 3857)
#' st_transform_custom(poly, transform_fun = scale_shift,
#'                     args = list(sx = 2, sy = 0.5, dx = 5))
#'
#' # MULTIPOLYGON example (two disjoint squares)
#' mp <- st_sfc(st_multipolygon(list(
#'   list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))),
#'   list(rbind(c(2,2), c(3,2), c(3,3), c(2,3), c(2,2)))
#' )), crs = 3857)
#' st_transform_custom(mp, transform_fun = scale_shift,
#'                     args = list(dx = 100, dy = 100))
#'
#' # In an sf data frame
#' sf_df <- st_sf(id = 1:2, geometry = st_sfc(
#'   st_point(c(10, 10)),
#'   st_linestring(rbind(c(0,0), c(2,0), c(2,2)))
#' ), crs = 3857)
#'
#' st_transform_custom(sf_df, transform_fun = scale_shift,
#'                     args = list(sx = 3, sy = 3))
#'
#' @seealso
#' [sf::st_coordinates()], [sf::st_geometry_type()],
#' [sf::st_sfc()], [sf::st_crs()]
#'
#' @importFrom sf st_geometry_type st_coordinates st_point st_linestring
#' @importFrom sf st_polygon st_multipolygon st_sfc st_crs
#' @export

st_transform_custom <- function(sf_obj, transform_fun, args) {

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

        new_coords <- do.call(transform_fun, c(list(coords), args))
        return(st_point(c(new_coords[1, 1], new_coords[1, 2])))

      } else if (geom_type == "LINESTRING") {
        coords <- st_coordinates(geom)[, 1:2, drop = FALSE]
        if (nrow(coords) == 0) return(st_linestring())

        new_coords <- do.call(transform_fun, c(list(coords), args))
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
          new_coords <- do.call(transform_fun, c(list(coords), args))
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
              new_coords <- do.call(transform_fun, c(list(ring_coords), args))
              ensure_closed(new_coords)
            })
            return(transformed_rings)
          } else {
            coords <- as.matrix(poly_df[, c("X", "Y")])
            new_coords <- do.call(transform_fun, c(list(coords), args))
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



