# Global variables for ggplot2 aesthetics
utils::globalVariables(c("x", "y", "zone"))

#' Create a Regular Test Grid of Coordinates
#'
#' @description
#' Generates a 2D grid of equally spaced points, useful for testing
#' fisheye transformations and other spatial warping functions.
#'
#' @param range Numeric vector of length 2 giving the x and y limits
#'   of the grid (default = `c(-1, 1)`).
#' @param spacing Numeric. Distance between adjacent grid points
#'   along each axis (default = `0.1`).
#'
#' @return A numeric matrix with two columns (`x`, `y`) containing
#'   the coordinates of the grid points.
#'
#' @examples
#' # Create a grid from -1 to 1 with spacing 0.25
#' grid <- create_test_grid(range = c(-1, 1), spacing = 0.25)
#' head(grid)
#'
#' @seealso [plot_fisheye_fgc()], [fisheye_fgc()] 
#' @export

create_test_grid <- function(range = c(-1, 1), spacing = 0.1) {
  x <- seq(range[1], range[2], by = spacing)
  y <- seq(range[1], range[2], by = spacing)
  grid <- expand.grid(x = x, y = y)
  return(as.matrix(grid))
  }
  
#' Classify Coordinates into Focus, Glue, or Context Zones
#'
#' @description
#' Assigns each point to one of three zones based on its radial
#' distance from a specified center:
#' - **focus**: inside the inner radius `r_in`
#' - **glue**: between `r_in` and `r_out`
#' - **context**: outside `r_out`
#'
#' This is a helper for visualizing and analyzing fisheye
#' transformations using the Focus–Glue–Context (FGC) model.
#'
#' @param coords A numeric matrix or data frame with at least two
#'   columns representing `(x, y)` coordinates.
#' @param cx,cy Numeric. The x and y coordinates of the fisheye
#'   center (default = 0, 0).
#' @param r_in Numeric. Inner radius of the focus zone
#'   (default = 0.34).
#' @param r_out Numeric. Outer radius of the glue zone
#'   (default = 0.5).
#'
#' @return A character vector of the same length as `nrow(coords)`,
#'   with values `"focus"`, `"glue"`, or `"context"`.
#'
#' @examples
#' # Simple example
#' pts <- matrix(c(0, 0, 0.2, 0.2, 0.6, 0.6), ncol = 2, byrow = TRUE)
#' classify_zones(pts, r_in = 0.3, r_out = 0.5)
#' #> "focus"   "glue"    "context"
#'
#' @seealso [fisheye_fgc()], [plot_fisheye_fgc()]
#' @export
 

classify_zones <- function(coords, cx = 0, cy = 0, r_in = 0.34, r_out = 0.5) {
  dx <- coords[, 1] - cx
  dy <- coords[, 2] - cy
  radius <- sqrt(dx^2 + dy^2)
  
  ifelse(radius <= r_in, "focus",
  ifelse(radius <= r_out, "glue", "context"))
  }


#' Visualize Focus–Glue–Context (FGC) Fisheye Transformation
#'
#' @description
#' Creates a side-by-side scatterplot comparing the **original**
#' and **transformed** coordinates of a dataset under the
#' Focus–Glue–Context fisheye mapping. Points are colored
#' according to whether they fall in the *focus*, *glue*, or
#' *context* zones, and boundary circles are drawn for clarity.
#'
#' @param original_coords A matrix or data frame with at least two
#'   columns representing the original `(x, y)` coordinates.
#' @param transformed_coords A matrix or data frame with the
#'   transformed `(x, y)` coordinates (same number of rows as
#'   `original_coords`).
#' @param cx,cy Numeric. The x and y coordinates of the fisheye
#'   center (default = 0, 0).
#' @param r_in Numeric. Radius of the inner *focus* boundary
#'   (default = 0.34).
#' @param r_out Numeric. Radius of the outer *glue* boundary
#'   (default = 0.5).
#'
#' @return A `ggplot2` object showing original vs transformed
#'   coordinates, colored by zone, with boundary circles
#'   overlaid.
#'
#' @examples
#' library(ggplot2)
#'
#' # Generate test grid and apply fisheye
#' grid <- create_test_grid(range = c(-1, 1), spacing = 0.1)
#' warped <- fisheye_fgc(grid, r_in = 0.4, r_out = 0.7)
#'
#' # Visualize transformation
#' plot_fisheye_fgc(grid, warped, r_in = 0.4, r_out = 0.7)
#'
#' @seealso [create_test_grid()], [fisheye_fgc()]
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual facet_wrap coord_fixed theme_minimal theme labs geom_path element_blank
#' @export


plot_fisheye_fgc <- function(original_coords, transformed_coords, 
  cx = 0, cy = 0, r_in = 0.34, r_out = 0.5) {

# Create data frames for plotting
zones <- classify_zones(original_coords, cx, cy, r_in, r_out)

original_df <- data.frame(
x = original_coords[, 1],
y = original_coords[, 2],
zone = zones,
type = "Original"
)

transformed_df <- data.frame(
x = transformed_coords[, 1],
y = transformed_coords[, 2], 
zone = zones,
type = "Transformed"
)

combined_df <- rbind(original_df, transformed_df)

# Create the plot
p <- ggplot(combined_df, aes(x = x, y = y, color = zone)) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = c("focus" = "#c60000ff", 
     "glue" = "#141497ff", 
     "context" = "#FFCC00")) +
facet_wrap(~type) +
coord_fixed() +
theme_minimal() +
theme(
panel.grid.minor = element_blank(),
legend.title = element_blank()
) +
labs(title = "Fisheye FGC Transformation",
subtitle = paste("r_in =", r_in, ", r_out =", r_out))

# Add zone boundary circles
if (r_in > 0) {
circle_in <- data.frame(
x = cx + r_in * cos(seq(0, 2*pi, length.out = 100)),
y = cy + r_in * sin(seq(0, 2*pi, length.out = 100))
)
p <- p + geom_path(data = circle_in, aes(x = x, y = y), 
color = "red", linetype = "dashed", inherit.aes = FALSE)
}

circle_out <- data.frame(
x = cx + r_out * cos(seq(0, 2*pi, length.out = 100)),
y = cy + r_out * sin(seq(0, 2*pi, length.out = 100))
)
p <- p + geom_path(data = circle_out, aes(x = x, y = y), 
color = "blue", linetype = "dashed", inherit.aes = FALSE)

return(p)
}

#' @export
#' @rdname fisheye_fgc
print.mapycus_fgc <- function(x, ...) {
  dims <- dim(x)

  attributes(x) <- NULL
  dim(x) <- dims

  print(x, ...)
}

# Helper functions for converting sf objects to plain coordinate lists
# These are used internally by the Shiny app but could be exported if useful

#' Extract Polygon Coordinates from sf Objects
#'
#' @description
#' Converts `sf` polygon or multipolygon geometries into a list
#' structure containing coordinate arrays, suitable for serialization
#' to JSON or use in JavaScript visualizations. Preserves both
#' exterior rings and holes.
#'
#' @param sf_obj An `sf` or `sfc` object containing `POLYGON` or
#'   `MULTIPOLYGON` geometries.
#' @param id_col Character. Optional column name to use as polygon IDs.
#'   If `NULL`, IDs are generated as `"poly-1"`, `"poly-2"`, etc.
#'   (default = `NULL`).
#'
#' @return A list of lists, each containing:
#'   - `id`: Character identifier for the polygon
#'   - `rings`: List of coordinate rings, where each ring is a list
#'     of `[x, y]` coordinate pairs. The first ring is the exterior
#'     boundary; subsequent rings (if present) are holes.
#'
#' @details
#' This function is primarily used to prepare spatial data for
#' client-side rendering in web applications. Each polygon may contain
#' multiple rings (exterior + holes), and multipolygons are decomposed
#' into separate ring lists.
#'
#' The output format is compatible with JavaScript mapping libraries
#' and SVG path generation.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a simple polygon
#' poly <- st_polygon(list(
#'   matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
#' ))
#' sf_obj <- st_sf(id = "test", geometry = st_sfc(poly))
#'
#' # Extract coordinates
#' coords <- polygons_from_sf(sf_obj, id_col = "id")
#' str(coords)
#' }
#'
#' @seealso [lines_from_sf()], [points_from_sf()], [shiny_fisheye()]
#' @keywords internal
polygons_from_sf <- function(sf_obj, id_col = NULL) {
  geoms <- sf::st_geometry(sf_obj)
  res <- lapply(seq_along(geoms), function(i) {
    # Handle both POLYGON and MULTIPOLYGON
    geom <- geoms[[i]]
    geom_type <- sf::st_geometry_type(geom)
    
    rings <- list()
    
    if (geom_type == "POLYGON") {
      coords_list <- sf::st_coordinates(geom)
      # Group by ring (L2 column distinguishes exterior/holes)
      ring_ids <- unique(coords_list[, "L2"])
      rings <- lapply(ring_ids, function(rid) {
        ring_coords <- coords_list[coords_list[, "L2"] == rid, c("X", "Y"), drop = FALSE]
        lapply(seq_len(nrow(ring_coords)), function(j) as.numeric(ring_coords[j, ]))
      })
    } else if (geom_type == "MULTIPOLYGON") {
      coords_list <- sf::st_coordinates(geom)
      # Group by L3 (polygon part) and L2 (ring within polygon)
      poly_ids <- unique(coords_list[, "L3"])
      for (pid in poly_ids) {
        poly_coords <- coords_list[coords_list[, "L3"] == pid, , drop = FALSE]
        ring_ids <- unique(poly_coords[, "L2"])
        for (rid in ring_ids) {
          ring_coords <- poly_coords[poly_coords[, "L2"] == rid, c("X", "Y"), drop = FALSE]
          rings[[length(rings) + 1]] <- lapply(seq_len(nrow(ring_coords)), function(j) as.numeric(ring_coords[j, ]))
        }
      }
    }
    
    if (length(rings) == 0) return(NULL)
    
    list(
      id = if (!is.null(id_col) && id_col %in% names(sf_obj)) {
        as.character(sf_obj[[id_col]][i])
      } else {
        paste0("poly-", i)
      },
      rings = rings
    )
  })
  purrr::compact(res)
}


#' Extract Line Coordinates from sf Objects
#'
#' @description
#' Converts `sf` line or multiline geometries into a list structure
#' containing coordinate arrays, suitable for serialization to JSON
#' or use in JavaScript visualizations.
#'
#' @param sf_obj An `sf` or `sfc` object containing `LINESTRING` or
#'   `MULTILINESTRING` geometries.
#' @param id_col Character. Optional column name to use as line IDs.
#'   If `NULL`, IDs are generated as `"ln-1"`, `"ln-2"`, etc.
#'   (default = `NULL`).
#'
#' @return A list of lists, each containing:
#'   - `id`: Character identifier for the line
#'   - `coords`: List of `[x, y]` coordinate pairs representing the
#'     line vertices in sequence
#'
#' @details
#' This function prepares line geometries for client-side rendering.
#' Multilinestrings are handled by extracting all coordinate points
#' in order, which may or may not be appropriate depending on the
#' use case.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a simple linestring
#' line <- st_linestring(matrix(c(0,0, 1,1, 2,0), ncol = 2, byrow = TRUE))
#' sf_obj <- st_sf(id = "route1", geometry = st_sfc(line))
#'
#' # Extract coordinates
#' coords <- lines_from_sf(sf_obj, id_col = "id")
#' str(coords)
#' }
#'
#' @seealso [polygons_from_sf()], [points_from_sf()], [shiny_fisheye()]
#' @keywords internal
lines_from_sf <- function(sf_obj, id_col = NULL) {
  geoms <- sf::st_geometry(sf_obj)
  res <- lapply(seq_along(geoms), function(i) {
    coords <- sf::st_coordinates(geoms[[i]])
    if (nrow(coords) == 0) return(NULL)
    xy <- coords[, c("X", "Y"), drop = FALSE]
    list(
      id = if (!is.null(id_col) && id_col %in% names(sf_obj)) {
        as.character(sf_obj[[id_col]][i])
      } else {
        paste0("ln-", i)
      },
      coords = lapply(seq_len(nrow(xy)), function(j) as.numeric(xy[j, ]))
    )
  })
  purrr::compact(res)
}


#' Extract Point Coordinates from sf Objects
#'
#' @description
#' Converts `sf` point geometries into a list structure containing
#' coordinate pairs, suitable for serialization to JSON or use in
#' JavaScript visualizations.
#'
#' @param sf_obj An `sf` or `sfc` object containing `POINT` geometries.
#' @param id_col Character. Optional column name to use as point IDs.
#'   If `NULL`, IDs are generated as sequential integers.
#'   (default = `NULL`).
#'
#' @return A list of lists, each containing:
#'   - `id`: Character identifier for the point
#'   - `x`: Numeric x-coordinate
#'   - `y`: Numeric y-coordinate
#'
#' @details
#' This function prepares point geometries for client-side rendering
#' as circles or markers in SVG or Canvas visualizations.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create simple points
#' pts <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)))
#' sf_obj <- st_sf(id = c("A", "B"), geometry = pts)
#'
#' # Extract coordinates
#' coords <- points_from_sf(sf_obj, id_col = "id")
#' str(coords)
#' }
#'
#' @seealso [polygons_from_sf()], [lines_from_sf()], [shiny_fisheye()]
#' @keywords internal
points_from_sf <- function(sf_obj, id_col = NULL) {
  coords <- sf::st_coordinates(sf_obj)
  n <- nrow(coords)
  ids <- if (!is.null(id_col) && id_col %in% names(sf_obj)) {
    as.character(sf_obj[[id_col]])
  } else {
    as.character(seq_len(n))
  }
  lapply(seq_len(n), function(i) {
    list(
      id = ids[i],
      x = as.numeric(coords[i, 1]),
      y = as.numeric(coords[i, 2])
    )
  })
}
