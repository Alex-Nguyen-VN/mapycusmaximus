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

save(vic, file = here::here("data/vic.rda"))