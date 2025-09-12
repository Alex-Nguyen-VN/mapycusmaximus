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
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual facet_wrap coord_fixed theme_minimal theme labs geom_path
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
scale_color_manual(values = c("focus" = "#663399", 
     "glue" = "#339999", 
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
