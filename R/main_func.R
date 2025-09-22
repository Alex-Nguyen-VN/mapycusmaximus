sf_fisheye <- function(sf_obj, cx = NULL, cy = NULL,
  r_in = 0.34, r_out = 0.5,
  zoom_factor = 1.5, squeeze_factor = 0.35,
  method = "expand",
  revolution = 0.0, target_crs = NULL,
  preserve_aspect = TRUE) {

stopifnot(r_out > r_in)
stopifnot(inherits(sf_obj, c("sf", "sfc")))

# Drop empties
if (inherits(sf_obj, "sf")) {
sf_obj <- sf_obj[!st_is_empty(sf_obj), ]
}

# 2D only
sf_obj <- st_zm(sf_obj, drop = TRUE, what = "ZM")

# Project to a working CRS if in lon/lat or target_crs provided
original_crs <- st_crs(sf_obj)
working_crs  <- original_crs

if (!is.null(target_crs)) {
sf_obj <- st_transform(sf_obj, target_crs)
working_crs <- st_crs(sf_obj)
} else if (st_is_longlat(sf_obj)) {
bbox <- st_bbox(sf_obj)
lon_center <- (bbox["xmin"] + bbox["xmax"])/2
lat_center <- (bbox["ymin"] + bbox["ymax"])/2
if (lon_center > 140 && lon_center < 150 && lat_center > -40 && lat_center < -30) {
working_crs <- "EPSG:7855" # GDA2020 / MGA Zone 55
} else {
utm_zone <- floor((lon_center + 180) / 6) + 1
working_crs <- if (lat_center >= 0) paste0("EPSG:", 32600 + utm_zone) else paste0("EPSG:", 32700 + utm_zone)
}
sf_obj <- st_transform(sf_obj, working_crs)
}

# Compute default center from bbox if not supplied
bbox <- st_bbox(sf_obj)
if (is.null(cx)) cx <- (bbox["xmin"] + bbox["xmax"])/2
if (is.null(cy)) cy <- (bbox["ymin"] + bbox["ymax"])/2

# --- Build normalization <-> denormalization (affine) ---
# center to (0,0)
sx <- (bbox["xmax"] - bbox["xmin"])/2
sy <- (bbox["ymax"] - bbox["ymin"])/2
if (sx == 0) sx <- 1
if (sy == 0) sy <- 1

if (preserve_aspect) {
s <- max(sx, sy)  # uniform scale (preserve shapes)
norm_fun <- function(M) { cbind((M[,1] - cx)/s, (M[,2] - cy)/s) }
denorm_fun <- function(M) { cbind(cx + M[,1]*s, cy + M[,2]*s) }
} else {
norm_fun <- function(M) { cbind((M[,1] - cx)/sx, (M[,2] - cy)/sy) }
denorm_fun <- function(M) { cbind(cx + M[,1]*sx, cy + M[,2]*sy) }
}

# Wrap fisheye_fgc: normalize -> fisheye -> denormalize
# IMPORTANT: keep r_in/r_out in the normalized units you expect (e.g. 0.34, 0.5)
# so they’re interpreted in the [-1,1]-like space.
base_args <- list(
cx = 0, cy = 0,
r_in = r_in, r_out = r_out,
zoom_factor = zoom_factor, squeeze_factor = squeeze_factor,
method = method, revolution = revolution
)

wrapped_fisheye <- function(coords, ...) {
coords <- as.matrix(coords[, 1:2, drop = FALSE])
N <- norm_fun(coords)                              # normalize into unit-ish space
T <- do.call(fisheye_fgc, c(list(N), base_args))   # transform (fisheye_fgc unchanged)
D <- denorm_fun(T)                                 # map back to original metric space
D
}

# Apply
result <- st_transform_custom(sf_obj, transform_fun = wrapped_fisheye, args = list())

# Return to original CRS if changed
if (!identical(st_crs(result), original_crs)) {
result <- st_transform(result, original_crs)
}
result
}
