#' Radial fisheye warp for `sf`/`sfc` objects with auto-CRS handling
#'
#' @description
#' `sf_fisheye()` applies a **focus–glue–context** style fisheye transformation
#' to vector geometries. It:
#'
#' 1) optionally projects lon/lat data to a suitable projected CRS (or a user
#' supplied CRS),
#' 2) **normalizes** coordinates around a chosen center `(cx, cy)` into a
#' unit-ish square while optionally preserving aspect,
#' 3) applies a radial fisheye mapping (implemented in `fisheye_fgc()`),
#' 4) **denormalizes** back to the working metric space,
#' 5) and finally reprojects to the original CRS.
#'
#' This produces local enlargement inside a focus ring, smooth transition
#' through a glue ring, and minimal change outside the context ring.
#'
#' @param sf_obj An [`sf`][sf::sf] or [`sfc`][sf::st_sfc] object. Geometry
#'   types `POINT`, `LINESTRING`, `POLYGON`, and `MULTIPOLYGON` are supported.
#'   Empty geometries are dropped before transformation.
#' @param cx,cy Optional numeric coordinates of the fisheye **center** in the
#'   *working CRS*. If `NULL`, the function uses the midpoint of the object
#'   bounding box after any CRS transform. See Details.
#' @param r_in,r_out Numeric radii (in **normalized units**) defining the inner
#'   and outer ring of the fisheye. Must satisfy `r_out > r_in`. Inside `r_in`
#'   gets enlarged, between `r_in` and `r_out` blends smoothly to the outside,
#'   and beyond `r_out` remains mostly unchanged. Defaults are tuned for
#'   normalized space (roughly \[-1, 1\] per axis).
#' @param zoom_factor Numeric (> 1 to enlarge). Intensity of magnification in
#'   the focus region (passed to `fisheye_fgc()`).
#' @param squeeze_factor Numeric in \[0, 1\]. Controls how strongly the glue
#'   region compresses/squeezes as it blends focus to context (passed to
#'   `fisheye_fgc()`).
#' @param method Character scalar. The fisheye method name understood by
#'   `fisheye_fgc()` (default `"expand"`). See that function for options.
#' @param revolution Numeric (radians). Optional angular twist/rotation applied
#'   by `fisheye_fgc()` (default `0`).
#' @param target_crs Optional CRS (anything accepted by
#'   [`sf::st_crs()`]/[`sf::st_transform()`]) to use as the **working CRS**.
#'   If `NULL` and `sf_obj` is lon/lat, an appropriate projected CRS is chosen
#'   automatically (see Details). If `sf_obj` is already projected, it is used
#'   as the working CRS.
#' @param preserve_aspect Logical, default `TRUE`. If `TRUE`, normalization uses
#'   a **uniform** scale so shapes are not stretched; if `FALSE`, X and Y are
#'   scaled independently to the bbox half-width/half-height.
#'
#' @details
#' **CRS handling.** If `target_crs` is `NULL` and the input is geographic
#' (lon/lat), the function picks a projected CRS based on the data centroid:
#'
#' - For data roughly centered on Victoria, AU (140°E–150°E, 40°S–30°S),
#'   it uses **EPSG:7855** (GDA2020 / MGA Zone 55).
#' - Otherwise, it chooses UTM: **EPSG:326##** for the northern hemisphere or
#'   **EPSG:327##** for the southern hemisphere using the standard 6° zone rule.
#'
#' If you need a specific working CRS (e.g., equal-area), supply `target_crs`.
#' The original CRS of `sf_obj` is restored at the end.
#'
#' **Normalization.** Let the bbox half-width/half-height be `sx`, `sy`. When
#' `preserve_aspect = TRUE`, we use a uniform scale `s = max(sx, sy)` and map
#' \eqn{(x, y) \mapsto ((x - cx)/s, (y - cy)/s)} so that your chosen `r_in` and
#' `r_out` (e.g., 0.34 and 0.5) are interpreted in a roughly unit-radius space.
#' When `preserve_aspect = FALSE`, X and Y are scaled separately by `sx`, `sy`.
#'
#' **Implementation notes.** The actual coordinate warp is delegated to
#' `fisheye_fgc()` and applied per-geometry via `st_transform_custom()`. Polygon
#' rings are re-closed after transformation. Z/M dimensions are dropped.
#'
#' @return An object of the same top-level class as `sf_obj` (`sf` or `sfc`),
#'   with the original CRS restored. Geometry coordinates are replaced by their
#'   fisheye-warped counterparts.
#'
#' @examples
#' library(sf)
#'
#' # A toy square polygon in a projected CRS
#' poly <- st_sfc(st_polygon(list(rbind(
#'   c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
#' ))), crs = 3857)
#'
#' # Default center at bbox midpoint, gentle magnification
#' out1 <- sf_fisheye(poly, r_in = 0.3, r_out = 0.6,
#'                    zoom_factor = 1.5, squeeze_factor = 0.35)
#'
#' # Explicit center and stronger focus, preserving aspect
#' out2 <- sf_fisheye(poly, cx = 0.5, cy = 0.5,
#'                    r_in = 0.25, r_out = 0.55,
#'                    zoom_factor = 2.0, squeeze_factor = 0.25)
#'
#' # Lon/lat data (auto-project to UTM/MGA), then fisheye
#' pt_ll <- st_sfc(st_point(c(144.9631, -37.8136)), crs = 4326) # Melbourne CBD
#' out3  <- sf_fisheye(pt_ll, r_in = 0.2, r_out = 0.5)
#'
#' # If your method supports angular twist:
#' # out4 <- sf_fisheye(poly, revolution = pi/12, method = "expand")
#'
#' @seealso
#' [sf::st_transform()], [sf::st_is_longlat()], [sf::st_crs()],
#' [sf::st_coordinates()], [st_transform_custom()], [fisheye_fgc()]
#'
#' @importFrom sf st_is_empty st_zm st_crs st_bbox st_transform st_is_longlat
#' @export

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
