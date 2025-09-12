library(tidyverse)
library(sf)
library(igraph)

vic <- read_sf(here::here("data/map/LGA_POLYGON.shp")) |>
  mutate(geometry = st_zm(geometry), drop = TRUE, what = "ZM") |>
  # convert to GDA2020 to match with census data
  mutate(geometry = st_transform(geometry, 7844))

  vic <- st_simplify(vic, dTolerance = 100) |>
    st_make_valid() |>
    st_zm(drop = TRUE, what = "ZM") |>
    mutate(LGA_NAME = toupper(LGA_NAME)) |>
    select(LGA_NAME, geometry)

plot(st_geometry(vic))

vic_crs <-  st_crs(vic)

get_center <- function(sf_obj) {
  st_coordinates(st_centroid(sf_obj))
}

vic_original <- st_coordinates(vic)
vic_coords <- data.frame(st_coordinates(vic))
new_coords <- vic_coords

vic |> filter(LGA_NAME == "MELBOURNE") |> 
  get_center() |> 
  as.data.frame() ->
  center_coords




new_coords <- new_coords |>
  mutate(radius = sqrt((X - center_coords$X)^2 + (Y - center_coords$Y)^2)) |>
  mutate(angle = atan2(Y - center_coords$Y, X - center_coords$X)) |>
  mutate(X_iden = center_coords$X + radius * cos(angle),
         Y_iden = center_coords$Y + radius * sin(angle),
        X_new = center_coords$X + sqrt(radius) * cos(angle),
      Y_new = center_coords$Y + sqrt(radius) * sin(angle)) 


new_coords |> ggplot() +
  geom_point(aes(X_iden, Y_iden), color = "blue", size = 0.1) +
  #geom_point(aes(X, Y), color = "grey", size = 0.1) +
  geom_point(aes(X_new, Y_new), color = "red", size = 0.1)



nested_list <- new_coords %>%
  group_by(L1, L2, L3) %>%
  summarise(coords = list(cbind(X_new, Y_new)), .groups = "drop") %>%
  group_by(L1, L2) %>%
  summarise(L3_list = list(setNames(coords, L3)), .groups = "drop") %>%
  group_by(L1) %>%
  summarise(L2_list = list(setNames(L3_list, L2)), .groups = "drop") %>%
  pull(L2_list) %>%
  setNames(unique(new_coords$L1))

new_geom <- map(nested_list, ~{
  # Extract rings for each polygon
  polygons <- map(.x, ~{
    rings <- map(.x, ~{
      coords <- as.matrix(.x)
      # Ensure ring is closed
      if(!identical(coords[1,], coords[nrow(coords),])) {
        coords <- rbind(coords, coords[1,])
      }
      coords
    })
    st_polygon(rings)
  })
  st_multipolygon(polygons)
})
new_geom <- st_sfc(new_geom)
new_geom <- st_set_crs(new_geom, st_crs(vic))
vic_bbox <- st_bbox(vic)
attr(vic_bbox, "class") = "bbox"
attr(new_geom, "bbox") = vic_bbox

# Get original and transformed bounding boxes
original_bbox <- st_bbox(vic)
transformed_coords_bbox <- c(
  xmin = min(new_coords$X_new),
  ymin = min(new_coords$Y_new),
  xmax = max(new_coords$X_new),
  ymax = max(new_coords$Y_new)
)

# Calculate scaling factors
scale_x <- (original_bbox["xmax"] - original_bbox["xmin"]) / 
           (transformed_coords_bbox["xmax"] - transformed_coords_bbox["xmin"])
scale_y <- (original_bbox["ymax"] - original_bbox["ymin"]) / 
           (transformed_coords_bbox["ymax"] - transformed_coords_bbox["ymin"])

# Apply scaling and translation to match original bbox
new_coords <- new_coords |>
  mutate(
    # First center at origin
    X_centered = X_new - mean(c(transformed_coords_bbox["xmin"], transformed_coords_bbox["xmax"])),
    Y_centered = Y_new - mean(c(transformed_coords_bbox["ymin"], transformed_coords_bbox["ymax"])),
    # Then scale
    X_scaled = X_centered * scale_x,
    Y_scaled = Y_centered * scale_y,
    # Finally translate to original center
    X_final = X_scaled + mean(c(original_bbox["xmin"], original_bbox["xmax"])),
    Y_final = Y_scaled + mean(c(original_bbox["ymin"], original_bbox["ymax"]))
  )

# Update your nested_list creation to use the scaled coordinates
nested_list <- new_coords %>%
  group_by(L1, L2, L3) %>%
  summarise(coords = list(cbind(X_final, Y_final)), .groups = "drop") %>%
  group_by(L1, L2) %>%
  summarise(L3_list = list(setNames(coords, L3)), .groups = "drop") %>%
  group_by(L1) %>%
  summarise(L2_list = list(setNames(L3_list, L2)), .groups = "drop") %>%
  pull(L2_list) %>%
  setNames(unique(new_coords$L1))

# Create new geometry (same code as before)
new_geom <- map(nested_list, ~{
  polygons <- map(.x, ~{
    rings <- map(.x, ~{
      coords <- as.matrix(.x)
      if(!identical(coords[1,], coords[nrow(coords),])) {
        coords <- rbind(coords, coords[1,])
      }
      coords
    })
    st_polygon(rings)
  })
  st_multipolygon(polygons)
})
new_geom <- st_sfc(new_geom)
new_geom <- st_set_crs(new_geom, st_crs(vic))

# Set the bbox to match original
attr(new_geom, "bbox") <- original_bbox

# Now plot - they should have the same overall size
ggplot() +
  geom_sf(aes(geometry = vic$geometry)) + 
  geom_sf(aes(geometry = new_geom), color = "red", fill = NA) +
  coord_sf()


ggplot() +
  geom_sf(aes(geometry = new_geom), color = "red", fill = NA) +
  coord_sf()

p = 3
r_out = 5
r_in = 2
u <- pmin(pmax((rho - r_in) / (r_out - r_in), 0), 1)

# Smooth step function for C1 continuity
smoothstep <- function(t) t * t * (3 - 2 * t)
s <- smoothstep(u)