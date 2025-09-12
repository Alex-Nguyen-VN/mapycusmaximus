library(dplyr)

circle <- read.csv("data-raw/squares.csv")


ggplot(circle) +
  geom_point(aes(x = V1, y = V2)) +
  coord_fixed(ratio = 1)

# Assume center at (0,0)
circle <- circle |>
  mutate(
    radius = sqrt(V1^2 + V2^2),
    angle = atan2(V2, V1),
    X_new = sqrt(radius) * cos(angle),
    Y_new = sqrt(radius) * sin(angle))

ggplot(circle) +
  geom_point(aes(x = V1, y = V2), size = 0.5) +
  geom_point(aes(x = X_new, y = Y_new), color = "red", size = 0.5) +
  coord_fixed(ratio = 1) + 
  geom_point(data = circle |> slice(1), aes(x = X_new, y = Y_new), color = "blue", size = 8) + 
  geom_point(data = circle |> slice(1), aes(x = V1, y = V2), color = "blue", size = 8)

fisheye_simplified_test <- function(data, cx = 0, cy = 0, 
  r_inner = 0.3, r_outer = 1.0, scale_factor = 0.5) {

# Ensure coords is a matrix with 2 columns
coords <- as.matrix(data[, 1:2, drop = FALSE])

# Center the coordinates
dx <- coords[, 1] - cx
dy <- coords[, 2] - cy

# Convert to polar coordinates
radius <- sqrt(dx^2 + dy^2)
angle <- atan2(dy, dx)

# Apply piecewise radial transformation (based on the images)
radius_new <- ifelse(
radius <= r_inner,
# Inner zone: apply scale factor directly
radius * scale_factor,

ifelse(
radius <= r_outer,
# Transition zone: smooth interpolation
{
# Normalized position in transition zone [0,1]
t <- (radius - r_inner) / (r_outer - r_inner)
# Smooth step interpolation
s <- t * t * (3 - 2 * t)
# Blend between scaled inner and normal outer
(radius * scale_factor) * (1 - s) + radius * s
},

# Outer zone: no transformation
radius
)
)

# Convert back to Cartesian coordinates
x_new <- cx + radius_new * cos(angle)
y_new <- cy + radius_new * sin(angle)

# Return transformed coordinates
data[,1:2] <- c(x_new, y_new)
return(data)
}

magnifying_glass <- function(data, cx = 0, cy = 0,
  b1 = 30, b2 = 60, 
  ratio_n = 0.3, R0 = 1.0) {
  
  coords <- as.matrix(data[, 1:2, drop = FALSE])
  
  # Center coordinates
  dx <- coords[, 1] - cx
  dy <- coords[, 2] - cy
  
  # Convert to angular distance (assuming degrees)
  # For coordinate data, we treat radius as angular distance
  radius <- sqrt(dx^2 + dy^2)
  angle <- atan2(dy, dx)
  
  # Convert to radians for trigonometric calculations
  b1_rad <- b1 * pi / 180
  b2_rad <- b2 * pi / 180
  radius_rad <- radius * pi / 180
  
  # Formula (14): Calculate scale factor gs
  gs <- (1 - ratio_n) * (1 - cos(b1_rad)) / 
        (ratio_n * (cos(b1_rad) - cos(b2_rad)))
  
  # Formula (15): Calculate Cs  
  Cs <- (1 - cos(b1_rad))^0.5 / ratio_n
  
  # Formula (13): Calculate inner circle radius r
  r <- 2^0.5 * R0 * (1 - cos(b1_rad))^0.5
  
  # Apply the complete magnifying glass transformation
  radius_new <- ifelse(
    radius_rad <= b1_rad,
    # Inner zone: Formula (10) - Lambert Azimuthal Equal-Area
    R0 * (1 - cos(radius_rad))^0.5 * Cs,
    
    ifelse(
      radius_rad <= b2_rad,  
      # Transition zone: Formula (11)
      R0 * (1 - (1 - gs) * cos(b1_rad) * 
            gs^((radius_rad - b1_rad)/(b2_rad - b1_rad))) * Cs,
      
      # Outer zone: no transformation
      radius
    )
  )
  
  # Back to Cartesian coordinates
  x_new <- cx + radius_new * cos(angle)
  y_new <- cy + radius_new * sin(angle)
  
  data[,1:2] <- c(x_new, y_new)
  return(data)
  }
  

  magnifying_glass_auto <- function(data, cx = 0, cy = 0,
    inner_angle_deg = 30,   # b1 in degrees
    outer_angle_deg = 60,   # b2 in degrees  
    magnification = 2.0) {   # How much to magnify

coords <- as.matrix(data[, 1:2, drop = FALSE])

# Center coordinates
dx <- coords[, 1] - cx
dy <- coords[, 2] - cy

# Polar coordinates
radius <- sqrt(dx^2 + dy^2)
angle <- atan2(dy, dx)

# Auto-calculate parameters based on magnification desired
b1_rad <- inner_angle_deg * pi / 180
b2_rad <- outer_angle_deg * pi / 180

# Calculate ratio_n to achieve desired magnification
ratio_n <- 1 / magnification

# Calculate derived parameters
gs <- (1 - ratio_n) * (1 - cos(b1_rad)) / 
(ratio_n * (cos(b1_rad) - cos(b2_rad)))

Cs <- (1 - cos(b1_rad))^0.5 / ratio_n

# Normalize radius to the angular range [0, π]
max_radius <- max(radius, na.rm = TRUE)
if (max_radius > 0) {
radius_norm <- radius * pi / max_radius
} else {
radius_norm <- radius
}

# Apply transformation
radius_new <- ifelse(
radius_norm <= b1_rad,
# Inner: magnified zone  
radius * magnification * (1 - cos(radius_norm))^0.5 / 
(1 - cos(b1_rad))^0.5,

ifelse(
radius_norm <= b2_rad,
# Transition: smooth blend
{
t <- (radius_norm - b1_rad) / (b2_rad - b1_rad)
s <- t * t * (3 - 2 * t)  # smoothstep
inner_val <- radius * magnification * (1 - cos(radius_norm))^0.5 / 
(1 - cos(b1_rad))^0.5
outer_val <- radius
(1 - s) * inner_val + s * outer_val
},

# Outer: unchanged
radius
)
)

# Back to Cartesian
x_new <- cx + radius_new * cos(angle)
y_new <- cy + radius_new * sin(angle)

data[,1:2] <- c(x_new, y_new)
return(data)
}


fisheye_simplified <- function(data) {
  coords <- data[, 1:2, drop = FALSE]
  radius <- sqrt(coords[, 1]^2 + coords[, 2]^2)
  angle <- atan2(coords[, 2], coords[, 1])
  X_new <- sqrt(radius) * cos(angle)
  Y_new <- sqrt(radius) * sin(angle)
  data[,1:2] <- c(X_new, Y_new)
  return(data)
}

center_data <- function(data) {
  coords <- data[, 1:2, drop = FALSE]
  X_new <- coords[, 1] - mean(coords[, 1])
  Y_new <- coords[, 2] - mean(coords[, 2])
  data[,1:2] <- c(X_new, Y_new)
  return(data)
}
circle <- circle |>
  mutate(zone = case_when(
    abs(V1) < 0.34 & abs(V2) < 0.34 ~ "focus",
    abs(V1) < 0.50 & abs(V2) < 0.50 ~ "glue",
    TRUE ~ "context"
  ))

circle |>
  ggplot() + 
  geom_point(aes(x = V1, y = V2, color = zone)) + 
  scale_color_viridis_d(labels = c("focus", "glue", "context")) +
  coord_fixed(ratio = 1)

circle |> center_data() |> 
  magnifying_glass_auto(magnification = 1.3) |> 
  ggplot() + 
  geom_point(aes(x = V1, y = V2, color = zone)) + 
  scale_color_viridis_d(labels = c("focus", "glue", "context")) +
  coord_fixed(ratio = 1)


circle |> center_data() |> 
  magnifying_glass() |> 
  ggplot() + 
  geom_point(aes(x = V1, y = V2, color = zone)) + 
  scale_color_viridis_d(labels = c("focus", "glue", "context")) +
  coord_fixed(ratio = 1)
  # xlim(-1, 1) +
  # ylim(-1, 1)

circle |> center_data() |> 
  fisheye_simplified_test(r_inner = 0.34, r_outer = 0.50, scale_factor = 2) |> 
  ggplot() + 
  geom_point(aes(x = V1, y = V2, color = zone)) + 
  scale_color_viridis_d(labels = c("focus", "glue", "context")) +
  coord_fixed(ratio = 1) + 
  xlim(-1, 1) +
  ylim(-1, 1)


circle |> center_data() |> 
  fisheye_simplified() |> 
  ggplot() + 
  geom_point(aes(x = V1, y = V2, color = zone)) + 
  scale_color_viridis_d(labels = c("focus", "glue", "context")) +
  coord_fixed(ratio = 1)


circle |> center_data() |> 
  fisheye_simplified() |> 
  ggplot() + 
  geom_point(aes(x = V1, y = V2, color = zone)) + 
  scale_color_viridis_d(labels = c("focus", "glue", "context")) +
  coord_fixed(ratio = 1)

fgc_fisheye <- function(data, r_in, r_out, scale = 1, cx = 0, cy = 0) {
  stopifnot(r_out > r_in, scale > 0)
  xy  <- as.matrix(data[, 1:2, drop = FALSE])
  # shift to a center if needed
  xy0 <- sweep(xy, 2, c(cx, cy), "-")

  r  <- sqrt(rowSums(xy0^2))
  th <- atan2(xy0[, 2], xy0[, 1])

  inner <- r < r_in
  outer <- r > r_out
  ring  <- r >= r_in & r <= r_out

  X_new <- numeric(length(r))
  Y_new <- numeric(length(r))
  r_new <- numeric(length(r))

  # inner: unchanged
  r_new[inner] <- r[inner]
  X_new[inner] <- r_new[inner] * cos(th[inner])
  Y_new[inner] <- r_new[inner] * sin(th[inner])
  # outer: unchanged
  r_new[outer] <- r[outer]
  X_new[outer] <- r_new[outer] * cos(th[outer])
  Y_new[outer] <- r_new[outer] * sin(th[outer])

  if (any(ring)) {
    t <- (r[ring] - r_in) / (r_out - r_in)
    s <- t * t * (3 - 2 * t)        # smoothstep 0→1
    r_new[ring] <- (1 - s) * (r[ring]) + s * r[ring]
    X_new[ring] <- sqrt(r_new[ring]) * cos(th[ring])
    Y_new[ring] <- sqrt(r_new[ring]) * sin(th[ring])
  }
   
  # shift back
  data[, 1:2] <- cbind(X_new + cx, Y_new + cy)
  data
}

circle |> fgc_fisheye(r_in = 0.35, r_out = 0.50) |> 
  ggplot() + 
  geom_point(aes(x = V1, y = V2, color = zone)) + 
  scale_color_viridis_d() +
  coord_fixed(ratio = 1)


