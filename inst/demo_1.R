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

fisheye_simplified <- function(data) {
  coords <- data[, 1:2, drop = FALSE]
  radius <- sqrt(coords[, 1]^2 + coords[, 2]^2)
  angle <- atan2(coords[, 2], coords[, 1])
  X_new <- sqrt(radius) * cos(angle)
  Y_new <- sqrt(radius) * sin(angle)
  return(data.frame(X_new, Y_new))
}

center_data <- function(data) {
  coords <- data[, 1:2, drop = FALSE]
  X_new <- coords[, 1] - mean(coords[, 1])
  Y_new <- coords[, 2] - mean(coords[, 2])
  return(data.frame(X_new, Y_new))
}

circle |> center_data() |> 
  fisheye_simplified() |> 
  ggplot() + 
  geom_point(aes(x = X_new, y = Y_new)) + 
  coord_fixed(ratio = 1)


