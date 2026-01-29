# General libraries needed
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggthemes)

# remotes::install_github("runapp-aus/strayr")
library(strayr)
vic_lga <- read_absmap("lga2022")
vic_lga <- vic_lga |>
  dplyr::filter(state_name_2021 == "Victoria") |>
  dplyr::filter(!(lga_name_2022 %in% c("Unincorporated Vic",
                                       "No usual address (Vic.)",
                                       "Migratory - Offshore - Shipping (Vic.)"))) |>
  rename(LGA_NAME = lga_name_2022) |>
  mutate(LGA_NAME = str_replace_all(LGA_NAME,
      "\\s*\\(.*\\)$", "")) |>  # drop trailing "(Vic)" etc.
  mutate(LGA_NAME = str_to_upper(LGA_NAME))


ggplot(vic_lga) + geom_sf(fill="grey85", color="white") + theme_map()
ggplot(dplyr::filter(vic_lga, LGA_NAME == "ALPINE")) +
  geom_sf(fill="grey85", color="white") + theme_map()

# This function will remove the internal polygons marking the ski fields
remove_holes <- function(map_data) {
  map_data <- st_make_valid(map_data)

  st_sfc(lapply(map_data, function(g) {
    if (inherits(g, "POLYGON")) {
      st_polygon(list(g[[1]]))   # keep only exterior ring
    } else if (inherits(g, "MULTIPOLYGON")) {
      st_multipolygon(lapply(g, function(p) list(p[[1]])))
    } else {
      g
    }
  }), crs = st_crs(map_data))
}

st_geometry(vic_lga) <- remove_holes(st_geometry(vic_lga))

ggplot(vic_lga) + geom_sf(fill="grey85", color="white") + theme_map()
ggplot(dplyr::filter(vic_lga, LGA_NAME == "ALPINE")) +
  geom_sf(fill="grey85", color="white") + theme_map()

vic <- vic_lga |>
  select(LGA_NAME)
save(vic, file="data/vic.rda")
