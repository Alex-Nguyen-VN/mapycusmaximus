#' Build FGC rings (inner/outer) as sf for plotting with geom_sf()
#' - Returns circles if preserve_aspect=TRUE, otherwise ellipses.
#' - Output is in the ORIGINAL CRS of sf_obj so it overlays your ggplot.
fisheye_rings_sf <- function(sf_obj,
  center = NULL, center_crs = NULL, normalized_center = FALSE,
  r_in = 0.34, r_out = 0.5,
  target_crs = NULL, preserve_aspect = TRUE,
  n = 360L,  # resolution for ellipse/polyline
  ring_as = c("line","polygon")) {

stopifnot(r_out > r_in)
ring_as <- match.arg(ring_as)

if (inherits(sf_obj, "sf")) sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
original_crs <- sf::st_crs(sf_obj)
sf_obj <- sf::st_zm(sf_obj, drop = TRUE, what = "ZM")

# ---- choose working projected CRS (same logic as sf_fisheye) ----
if (!is.null(target_crs)) {
sf_obj <- sf::st_transform(sf_obj, target_crs)
} else if (sf::st_is_longlat(sf_obj)) {
bb <- sf::st_bbox(sf_obj)
lon_center <- (bb["xmin"] + bb["xmax"])/2
lat_center <- (bb["ymin"] + bb["ymax"])/2
if (lon_center > 140 && lon_center < 150 && lat_center > -40 && lat_center < -30) {
sf_obj <- sf::st_transform(sf_obj, "EPSG:7855")
} else {
utm_zone <- floor((lon_center + 180)/6) + 1
epsg <- if (lat_center >= 0) paste0("EPSG:", 32600 + utm_zone) else paste0("EPSG:", 32700 + utm_zone)
sf_obj <- sf::st_transform(sf_obj, epsg)
}
}
working_crs <- sf::st_crs(sf_obj)

# ---- bbox scales & center (same as sf_fisheye) ----
bb <- sf::st_bbox(sf_obj)
cx0 <- (bb["xmin"] + bb["xmax"])/2
cy0 <- (bb["ymin"] + bb["ymax"])/2
sx <- (bb["xmax"] - bb["xmin"])/2; if (sx == 0) sx <- 1
sy <- (bb["ymax"] - bb["ymin"])/2; if (sy == 0) sy <- 1
if (preserve_aspect) {
s <- max(sx, sy)
}

# Use your internal resolver if you have it:
# cxy <- .resolve_center(center, center_crs, working_crs, bb, preserve_aspect, normalized_center)

# Minimal inline resolver (lon/lat heuristic + normalized support)
if (!is.null(center) && inherits(center, c("sf","sfc"))) {
g <- if (inherits(center, "sf")) sf::st_geometry(center) else center
if (!all(sf::st_geometry_type(g) %in% c("POINT","MULTIPOINT")) || length(g) > 1) {
g <- sf::st_centroid(sf::st_combine(g))
}
cxy <- as.numeric(sf::st_coordinates(sf::st_transform(g, working_crs))[1,1:2])
} else if (isTRUE(normalized_center)) {
stopifnot(is.numeric(center), length(center) == 2)
if (preserve_aspect) {
cxy <- c(cx0 + center[1]*s, cy0 + center[2]*s)
} else {
cxy <- c(cx0 + center[1]*sx, cy0 + center[2]*sy)
}
} else if (!is.null(center)) {
stopifnot(is.numeric(center), length(center) == 2)
if (!is.null(center_crs)) {
pt <- sf::st_sfc(sf::st_point(center), crs = center_crs)
cxy <- as.numeric(sf::st_coordinates(sf::st_transform(pt, working_crs))[1,1:2])
} else if (abs(center[1]) <= 180 && abs(center[2]) <= 90) { # looks like lon/lat
pt <- sf::st_sfc(sf::st_point(center), crs = 4326)
cxy <- as.numeric(sf::st_coordinates(sf::st_transform(pt, working_crs))[1,1:2])
} else {
cxy <- center
}
} else {
cxy <- c(cx0, cy0)
}

# ---- Build rings in WORKING CRS ----
if (preserve_aspect) {
rin  <- r_in  * s
rout <- r_out * s
p0   <- sf::st_sfc(sf::st_point(cxy), crs = working_crs)
ring_in_geom  <- sf::st_buffer(p0, dist = rin)
ring_out_geom <- sf::st_buffer(p0, dist = rout)
} else {
# Ellipses with radii (r*sx, r*sy)
make_ellipse <- function(rx, ry) {
t <- seq(0, 2*pi, length.out = n)
xy <- cbind(cxy[1] + rx*cos(t), cxy[2] + ry*sin(t))
sf::st_sfc(sf::st_polygon(list(rbind(xy, xy[1,]))), crs = working_crs)
}
ring_in_geom  <- make_ellipse(r_in * sx,  r_in * sy)
ring_out_geom <- make_ellipse(r_out * sx, r_out * sy)
}

if (ring_as == "line") {
ring_in_geom  <- sf::st_boundary(ring_in_geom)
ring_out_geom <- sf::st_boundary(ring_out_geom)
}

rings <- sf::st_as_sf(data.frame(
ring = c("r_in","r_out"),
geometry = sf::st_sfc(ring_in_geom[[1]], ring_out_geom[[1]], crs = working_crs)
))

# ---- back to ORIGINAL CRS so it overlays your ggplot ----
if (!identical(working_crs, original_crs)) {
rings <- sf::st_transform(rings, original_crs)
}
rings
}


# 1) Warp your map
fisheye_vic <- sf_fisheye(vic,
  center = c(144.9631, -37.8136), center_crs = "EPSG:4326",
  r_in = 0.34, r_out = 0.50, zoom_factor = 5, squeeze_factor = 0.3)

# 2) Build ring overlays that match the exact center/scales used
rings <- fisheye_rings_sf(vic,
  center = c(144.9631, -37.8136), center_crs = "EPSG:4326",
  r_in = 0.34, r_out = 0.50, preserve_aspect = TRUE, ring_as = "line")

# 3) Plot
library(ggplot2)
ggplot() +
  geom_sf(data = fisheye_vic, fill = NA, color = "grey50") +
  geom_sf(data = rings, aes(linetype = ring), color = "red", linewidth = 0.6) +
  coord_sf()
