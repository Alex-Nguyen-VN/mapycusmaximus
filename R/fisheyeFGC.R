#' Apply Focus–Glue–Context Fisheye Transformation
#'
#' @description
#' Transforms 2D coordinates using a **Focus–Glue–Context (FGC) fisheye transformation**.  
#' The function expands points inside a focus region, compresses points in a glue region,  
#' and leaves the surrounding context unchanged. Optionally, a rotational "revolution"  
#' can be added to the glue region to produce a swirling effect.
#'
#' @details
#' This function operates in three radial zones around a chosen center:
#' - **Focus zone (r <= r_in)**: expands distances from the center using `zoom_factor`,  
#'   but does not exceed the `r_in` boundary.
#' - **Glue zone (r_in < r <= r_out)**: compresses distances using a power-law defined  
#'   by `squeeze_factor`, then remaps them to smoothly connect focus and context zones.
#' - **Context zone (r > r_out)**: coordinates remain unchanged.
#'
#' Optionally, points in the glue zone can be rotated (`revolution`) to emphasize continuity.
#'
#' @param coords A matrix or data frame with at least two columns representing x and y coordinates.
#' @param cx,cy Numeric. The x and y coordinates of the fisheye center (default = 0, 0).
#' @param r_in Numeric. Radius of the focus zone (default = 0.34).
#' @param r_out Numeric. Radius of the glue zone boundary (default = 0.5).
#' @param zoom_factor Numeric. Expansion factor applied within the focus zone (default = 1.5).
#' @param squeeze_factor Numeric in (0,1]. Compression factor applied within the glue zone  
#'   (smaller values = stronger compression, default = 0.3).
#' @param revolution Numeric. Optional rotation factor applied in the glue zone. Positive values  
#'   rotate counter-clockwise, negative values clockwise (default = 0.0).
#'
#' @return A numeric matrix with two columns (`x_new`, `y_new`) of transformed coordinates.  
#' Additional attributes:
#' - `"zones"`: character vector classifying each point as `"focus"`, `"glue"`, or `"context"`.  
#' - `"original_radius"`: numeric vector of original radial distances.  
#' - `"new_radius"`: numeric vector of transformed radial distances.
#'
#' @examples
#' # Create a set of example coordinates
#' grid <- create_test_grid(range = c(-1, 1), spacing = 0.1)
#' 
#' # Apply FGC fisheye with expansion and compression
#' transformed <- fisheye_fgc(grid, r_in = 0.34, r_out = 0.5, zoom_factor = 1.3, squeeze_factor = 0.5)
#'
#' # Plot original vs transformed
#' plot_example_fgc(grid, transformed, r_in = 0.34, r_out = 0.5)
#' 
#' @export



fisheye_fgc <- function(coords, cx = 0, cy = 0,
  r_in = 0.34, r_out = 0.5,
  zoom_factor = 1.5,     # How much focus zone expands
  squeeze_factor = 0.3,  # How much glue zone compresses
  revolution = 0.0) {    # Optional: add rotation to glue zone
  
  coords <- as.matrix(coords[, 1:2, drop = FALSE])
  # Center coordinates
  dx <- coords[, 1] - cx
  dy <- coords[, 2] - cy
  # Polar coordinates
  radius <- sqrt(dx^2 + dy^2)
  angle <- atan2(dy, dx)

  # Create zone classification
  zone <- ifelse(radius <= r_in, "focus",
  ifelse(radius <= r_out, "glue", "context"))

  # Calculate new radius for each zone
  radius_new <- ifelse(
  # FOCUS ZONE: Can expand but limited to r_in boundary
  zone == "focus",
  {
  # Normalize to [0,1] within focus zone
  norm_r <- radius / r_in
  # Apply zoom but clamp to r_in boundary
  expanded_r <- norm_r * zoom_factor
  pmin(expanded_r, 1.0) * r_in  # Don't exceed r_in
  },

  ifelse(
  # GLUE ZONE: Squeeze and revolve around focus
  zone == "glue",
  {
  # Normalize position in glue zone [0,1]
  u <- (radius - r_in) / (r_out - r_in)

  # Create fisheye compression curve
  # This creates the "revolving" effect seen in your image
  u_compressed <- u^(1/squeeze_factor)  # Power function for compression

  # Map compressed u back to physical space
  # But make it "hug" the focus zone boundary
  compressed_width <- (r_out - r_in) * squeeze_factor
  r_in + u_compressed * compressed_width
  },

  # CONTEXT ZONE: No change
  radius
  )
  )

  # Optional: Add revolution (rotation) to glue zone
  angle_new <- ifelse(
  zone == "glue",
  {
  # Add rotation proportional to compression
  u <- (radius - r_in) / (r_out - r_in)
  rotation_amount <- revolution * u * (1 - u) * 4  # Bell curve rotation
  angle + rotation_amount
  },
  angle  # No rotation for focus and context
  )

  # Convert back to Cartesian
  x_new <- cx + radius_new * cos(angle_new)
  y_new <- cy + radius_new * sin(angle_new)

  # Return with zone information for visualization
  result <- cbind(x_new, y_new)
  attr(result, "zones") <- zone
  attr(result, "original_radius") <- radius
  attr(result, "new_radius") <- radius_new

  return(result)
}
