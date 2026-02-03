library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

load("data/vic.rda")

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
data_name <- read.csv("data-raw/hospital_address.csv")
k <- 5
hosp_lookup <- data_name %>%
  filter(str_to_lower(category) == "hospital") %>%
  transmute(
    long_hosp = round(longitude, k),
    lat_hosp  = round(latitude,  k),
    hosp_name = formal_name
  ) %>%
  distinct()

racf_lookup <- data_name %>%
  filter(str_to_lower(category) == "racf") %>%
  transmute(
    long_racf = round(longitude, k),
    lat_racf  = round(latitude,  k),
    racf_name = formal_name
  ) %>%
  distinct()

# Build connections
connections <- make_connections(transfers)

connections <- connections %>%
  mutate(
    long_hosp = round(long_hosp, k),
    lat_hosp  = round(lat_hosp,  k),
    long_racf = round(long_racf, k),
    lat_racf  = round(lat_racf,  k)
  ) %>%
  left_join(hosp_lookup, by = c("long_hosp", "lat_hosp")) %>%
  left_join(racf_lookup, by = c("long_racf", "lat_racf"))

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
center_pt_proj <- st_transform(center_pt, 3111)
distances_m <- st_distance(st_line_sample(connections_proj, sample = 0), center_pt_proj)
distances_norm <- as.numeric(distances_m) / max(as.numeric(distances_m))
connections_focus_proj <- connections_proj[as.numeric(distances_norm) <= r_in, ]
connections_focus_proj_sum <- connections_focus_proj |>
  group_by(source) |>
  group_by(source) |> 
  count() |> 
  arrange(desc(n)) 


conn_fish_small <- connections_focus_proj |> 
  filter(source %in% connections_focus_proj_sum$source[1:20])


vic_fish   <- sf_fisheye(vic_proj, center = center_pt_proj,
                         r_in = 0.34, r_out = 0.5, zoom_factor = 1)
conn_fish  <- sf_fisheye(conn_fish_small, center = center_pt_proj,
                         r_in = 0.428, r_out = 0.429, zoom_factor = 1)

save(vic_fish, file = here::here("data/vic_fish.rda"), compress = "xz")
save(conn_fish, file = here::here("data/conn_fish.rda"), compress = "xz")

# GGanimate approach

library(sf)
library(dplyr)
library(purrr)



ggplot() +
  geom_sf(data = sf_fisheye(vic_proj, center = center_pt_proj,
                          r_in = 0.34, r_out = 0.4, zoom_factor = 3)) +
  geom_sf(data = sf_fisheye(conn_fish_small, center = center_pt_proj,
                          r_in = 1.07, r_out = 2, zoom_factor = 3))

zoom_seq <- seq(1, 20, by = 0.1)


fisheye_frames <- map_dfr(zoom_seq, function(z) {
  vic_fish  <- sf_fisheye(vic_proj, center = center_pt_proj,
                          r_in = 0.34, r_out = 0.5, zoom_factor = z)
  conn_fish <- sf_fisheye(conn_fish_small, center = center_pt_proj,
                          r_in = 1.07, r_out = 2, zoom_factor = z)
  
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

library(ggthemes)

sf_fisheye(vic_fish, center = center_proj,
            r_in = 0.24, r_out = 0.3, zoom_factor = 5) |>
   ggplot() +
   geom_sf() +
   coord_sf()

conn_fish |> 
  ggplot() +
  geom_sf(aes(label = source))




center_proj <- st_transform(center_proj, st_crs(vic))
center_bbox <- st_bbox(center_proj)

library(ggthemes)

ggplot(data = vic, fill = NA, color = "grey70") + 
  geom_sf() +
  geom_sf_label(aes(label = LGA_NAME)) +
  coord_sf(xlim = center_bbox[c("xmin", "xmax")], ylim = center_bbox[c("ymin", "ymax")]) +
  theme_map()


ggplot() +
  geom_sf(data = vic_fish, fill = NA, color = "grey80") +
  geom_sf(data = conn_fish, aes(alpha = weight), color = "black") +
  labs(title = "Transportation between Hospital and Age Care Facilities in VIC
during COVID - 19") +
  theme_map()


conn_fish |> group_by(source) |> count() |> arrange(desc(n)) -> conn_fish_summary


conn_small <- conn_fish |> 
  filter(source %in% conn_fish_summary$source[1:20])

center_zoom <- vic_fish |> 
  filter(LGA_NAME == "MELBOURNE")
center_point <- st_bbox(center_zoom)

ggplot() +
  geom_sf(data = vic_fish, fill = NA, color = "grey80") +
  geom_sf_label(data = vic_fish, aes(label = LGA_NAME)) +
  geom_sf(data = conn_small, aes(alpha = weight), color = "black") +
  coord_sf(xlim = center_point[c("xmin", "xmax")], ylim = center_point[c("ymin", "ymax")]) +
  labs(title = "Transportation between Hospital and Age Care Facilities in VIC
during COVID - 19") +
  theme_map()

