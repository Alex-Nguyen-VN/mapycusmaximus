library(sf)
library(dplyr)

vic <- read_sf(here::here("data-raw/map/LGA_POLYGON.shp")) |>
  mutate(geometry = st_zm(geometry), drop = TRUE, what = "ZM") |>
  # convert to GDA2020 to match with census data
  mutate(geometry = st_transform(geometry, 7844))

  vic <- st_simplify(vic, dTolerance = 100) |>
    st_make_valid() |>
    mutate(LGA_NAME = toupper(LGA_NAME)) |>
    select(LGA_NAME, geometry)

save(vic, file = here::here("data/vic.rda"), compress = "xz")

library(sf)
library(dplyr)

# Function to create sf LINESTRING connections
make_connections <- function(df, 
                             long_src = "long_hosp", lat_src = "lat_hosp",
                             long_dst = "long_racf", lat_dst = "lat_racf",
                             crs = 4326) {
  
  # Ensure sf is loaded
  stopifnot(requireNamespace("sf"))
  
  # Build LINESTRING geometries row by row
  lines <- lapply(seq_len(nrow(df)), function(i) {
    coords <- matrix(c(df[[long_src]][i], df[[lat_src]][i],
                       df[[long_dst]][i], df[[lat_dst]][i]),
                     ncol = 2, byrow = TRUE)
    st_linestring(coords)
  })
  
  # Wrap into sf object
  sf_obj <- st_sf(df,
                  geometry = st_sfc(lines, crs = crs))
  
  return(sf_obj)
}

transfers <- read.csv("data-raw/transfers_coded.csv")

# Build connections
connections <- make_connections(transfers)

# Plot
plot(connections["weight"], lwd = 2)

# Suppose these are your fisheye parameters
cx <- 145.0
cy <- -37.8
r_in <- 0.34   # adjust to your CRS units (degrees if EPSG:4326, meters if projected)
r_out <- 0.5
# Define the center as sf point
center_pt <- st_sfc(st_point(c(cx, cy)), crs = st_crs(connections))

# Extract the source point from each LINESTRING (first coordinate)
connections_proj <- st_transform(connections, 3111)

# Sample first vertex (source point)
sources <- st_line_sample(connections_proj, sample = 0)
sources <- st_cast(sources, "POINT")

# Bring back to original CRS if needed
sources <- st_transform(sources, st_crs(connections))


# Compute distances
distances <- st_distance(sources, center_pt)

distances <- distances/max(distances)

# Filter connections inside radius
connections_focus <- connections[as.numeric(distances) <= r_in, ]

crs_proj <- 3111
vic_proj <- st_transform(vic, crs_proj)
connections_proj <- st_transform(connections, 3111)
center_proj <- st_transform(center_pt, 3111)
distances_m <- st_distance(st_line_sample(connections_proj, sample = 0), center_proj)
distances_norm <- as.numeric(distances_m) / max(as.numeric(distances_m))
connections_focus_proj <- connections_proj[as.numeric(distances_norm) <= r_in, ]

vic_fish   <- sf_fisheye(vic_proj, center = center_proj,
                         r_in = 0.34, r_out = 0.5, zoom_factor = 1)
conn_fish  <- sf_fisheye(connections_focus_proj, center = center_proj,
                         r_in = 0.428, r_out = 0.429, zoom_factor = 1)

save(vic_fish, file = here::here("data/vic_fish.rda"), compress = "xz")
save(conn_fish, file = here::here("data/conn_fish.rda"), compress = "xz")

# Define zoom sequence
zoom_seq <- seq(1, 10, by = 0.05)
i <- 0
# Output folder
dir.create("fisheye_frames", showWarnings = FALSE)

# Loop over zoom factors
for (z in zoom_seq) {
  i <- i + 1
  # Apply fisheye transformation
  vic_fish <- sf_fisheye(
    vic_proj,
    center = center_pt_proj,
    r_in = 0.34,
    r_out = 0.5,
    zoom_factor = z
  )
  
  conn_fish <- sf_fisheye(
    connections_focus_proj,
    center = center_pt_proj,
    r_in = 0.428,
    r_out = 0.429,
    zoom_factor = z
  )
  
  # Plot
  p <- ggplot() +
    geom_sf(data = conn_fish, aes(alpha = weight), color = "black") +
    geom_sf(data = vic_fish, fill = NA, color = "grey20") +
    coord_sf(crs = st_crs(crs_proj)) +
    labs(title = glue("Fisheye Zoom: {sprintf('%.1f', z)}x")) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  # Save each frame
  frame_file <- glue("fisheye_frames/frame_{sprintf('%04d', i)}.png")
  ggsave(frame_file, p, width = 8, height = 6, dpi = 200)
  
  message("Saved: ", frame_file)
}
library(magick)

# Read all frames
frames <- list.files("fisheye_frames", full.names = TRUE, pattern = "png") |> 
  lapply(image_read)

# Combine and animate
animation <- image_join(frames) |> 
  image_animate(fps = 25)  # frames per second

# Save the GIF
image_write(animation, "fisheye_zoom.gif")


# GGanimate approach

library(sf)
library(dplyr)
library(purrr)

fisheye_frames <- map_dfr(zoom_seq, function(z) {
  vic_fish  <- sf_fisheye(vic_proj, center = center_pt_proj,
                          r_in = 0.34, r_out = 0.5, zoom_factor = z)
  conn_fish <- sf_fisheye(connections_focus_proj, center = center_pt_proj,
                          r_in = 0.428, r_out = 0.429, zoom_factor = z)
  
  tibble(
    zoom_factor = z,
    vic = list(vic_fish),
    conn = list(conn_fish)
  )
})
fish_long <- map_dfr(1:nrow(fisheye_frames), function(i) {
  z <- fisheye_frames$zoom_factor[i]
  
  bind_rows(
    fisheye_frames$vic[[i]]  %>% mutate(type = "vic",  zoom_factor = z),
    fisheye_frames$conn[[i]] %>% mutate(type = "conn", zoom_factor = z)
  )
})
library(gganimate)
library(ggplot2)

p <- ggplot() +
  geom_sf(data = subset(fish_long, type == "vic"),
          fill = NA, color = "grey30") +
  geom_sf(data = subset(fish_long, type == "conn"),
          aes(alpha = weight), color = "black") +
  coord_sf(crs = st_crs(crs_proj)) +
  labs(title = "Fisheye Zoom: {current_frame}×") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  transition_manual(zoom_factor)
anim <- animate(
  p,
  fps = 25,       # 25 fps divides 100 evenly
  duration = 8,   # seconds total
  width = 800,
  height = 600,
  res = 150
)

anim_save("fisheye_zoom_gganimate.gif", animation = anim)
