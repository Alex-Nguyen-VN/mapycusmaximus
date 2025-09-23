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

sf_fisheye <- function(sf_obj,
  center = NULL,          # accepts c(lon,lat), c(x,y in map units), normalized pair, or sf/sfc POINT
  center_crs = NULL,      # e.g. "EPSG:4326"; if NULL we auto-guess (lon/lat vs map units)
  normalized_center = FALSE,  # TRUE if 'center' is in [-1,1] normalized coords
  cx = NULL, cy = NULL,   # legacy map-unit center still supported; ignored if 'center' given
  r_in = 0.34, r_out = 0.5,
  zoom_factor = 1.5, squeeze_factor = 0.35,
  method = "expand",
  revolution = 0.0,
  target_crs = NULL,
  preserve_aspect = TRUE) {

stopifnot(r_out > r_in)
stopifnot(inherits(sf_obj, c("sf", "sfc")))
if (inherits(sf_obj, "sf")) sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]

sf_obj <- sf::st_zm(sf_obj, drop = TRUE, what = "ZM")

# --- choose working CRS (projected) ---
original_crs <- sf::st_crs(sf_obj)
if (!is.null(target_crs)) {
sf_obj <- sf::st_transform(sf_obj, target_crs)
} else if (sf::st_is_longlat(sf_obj)) {
bb <- sf::st_bbox(sf_obj)
lon_center <- (bb["xmin"] + bb["xmax"])/2
lat_center <- (bb["ymin"] + bb["ymax"])/2
if (lon_center > 140 && lon_center < 150 && lat_center > -40 && lat_center < -30) {
sf_obj <- sf::st_transform(sf_obj, "EPSG:7855")  # GDA2020 / MGA Zone 55
} else {
utm_zone <- floor((lon_center + 180)/6) + 1
epsg <- if (lat_center >= 0) paste0("EPSG:", 32600 + utm_zone) else paste0("EPSG:", 32700 + utm_zone)
sf_obj <- sf::st_transform(sf_obj, epsg)
}
}
working_crs <- sf::st_crs(sf_obj)

# --- bbox + scale ---
bb <- sf::st_bbox(sf_obj)
sx <- (bb["xmax"] - bb["xmin"])/2; if (sx == 0) sx <- 1
sy <- (bb["ymax"] - bb["ymin"])/2; if (sy == 0) sy <- 1
if (preserve_aspect) {
s <- max(sx, sy)
norm_fun   <- function(M) cbind((M[,1] - 0)/s, (M[,2] - 0)/s) # center handled separately
denorm_fun <- function(M, cxy) cbind(cxy[1] + M[,1]*s, cxy[2] + M[,2]*s)
} else {
norm_fun   <- function(M, cxy) cbind((M[,1] - cxy[1])/sx, (M[,2] - cxy[2])/sy)
denorm_fun <- function(M, cxy) cbind(cxy[1] + M[,1]*sx,  cxy[2] + M[,2]*sy)
}

# --- resolve center precedence ---
# 1) center (flexible) > 2) cx,cy (map units) > 3) bbox center
if (!is.null(center)) {
cxy <- .resolve_center(center, center_crs, working_crs, bb, preserve_aspect, normalized_center)
} else {
if (is.null(cx) || is.null(cy)) {
cxy <- c((bb["xmin"] + bb["xmax"])/2, (bb["ymin"] + bb["ymax"])/2)
} else {
cxy <- c(cx, cy)
}
}

# --- fisheye wrapper: normalize around cxy -> fisheye -> denormalize
base_args <- list(cx = 0, cy = 0, r_in = r_in, r_out = r_out,
zoom_factor = zoom_factor, squeeze_factor = squeeze_factor,
method = method, revolution = revolution)

if (preserve_aspect) {
wrapped_fisheye <- function(coords, ...) {
M <- as.matrix(coords[,1:2, drop = TRUE])
N <- cbind((M[,1] - cxy[1])/s, (M[,2] - cxy[2])/s)
T <- do.call(fisheye_fgc, c(list(N), base_args))
denorm_fun(T, cxy)
}
} else {
wrapped_fisheye <- function(coords, ...) {
M <- as.matrix(coords[,1:2, drop = TRUE])
N <- norm_fun(M, cxy)
T <- do.call(fisheye_fgc, c(list(N), base_args))
denorm_fun(T, cxy)
}
}

out <- st_transform_custom(sf_obj, transform_fun = wrapped_fisheye, args = list())
if (!identical(sf::st_crs(out), original_crs)) out <- sf::st_transform(out, original_crs)
out
}


# Helper: resolve user-supplied center into the working (projected) CRS
# Helper: resolve user-supplied center into the working (projected) CRS
.resolve_center <- function(center, center_crs, working_crs,
  bbox, preserve_aspect, normalized_center) {
# Fallback = bbox center
cx0 <- (bbox["xmin"] + bbox["xmax"])/2
cy0 <- (bbox["ymin"] + bbox["ymax"])/2
if (is.null(center)) return(c(cx0, cy0))

# --- Case 1: center is sf/sfc of ANY geometry type ---
if (inherits(center, c("sf", "sfc"))) {
g <- if (inherits(center, "sf")) sf::st_geometry(center) else center
if (length(g) == 0) return(c(cx0, cy0))
if (is.na(sf::st_crs(g))) {
stop("The supplied sf/sfc 'center' has no CRS. Please set it or use center_crs.")
}

# If it's not a single POINT, reduce to a single centroid
if (!all(sf::st_geometry_type(g) %in% c("POINT", "MULTIPOINT"))) {
g <- sf::st_centroid(sf::st_combine(g))
} else if (length(g) > 1) {
g <- sf::st_centroid(sf::st_combine(g))
}

g_w <- sf::st_transform(g, working_crs)
xy  <- as.numeric(sf::st_coordinates(g_w)[1, 1:2])
return(xy)
}

# --- Case 2: normalized [-1,1] pair relative to bbox center/scale ---
if (isTRUE(normalized_center)) {
stopifnot(is.numeric(center), length(center) == 2)
sx <- (bbox["xmax"] - bbox["xmin"])/2; if (sx == 0) sx <- 1
sy <- (bbox["ymax"] - bbox["ymin"])/2; if (sy == 0) sy <- 1
if (preserve_aspect) {
s  <- max(sx, sy)
return(c(cx0 + center[1]*s, cy0 + center[2]*s))
} else {
return(c(cx0 + center[1]*sx, cx0 + center[2]*sy))
}
}

# --- Case 3: numeric pair; use declared CRS or auto-guess lon/lat vs map-units ---
stopifnot(is.numeric(center), length(center) == 2)

if (!is.null(center_crs)) {
pt   <- sf::st_sfc(sf::st_point(center), crs = center_crs)
pt_w <- sf::st_transform(pt, working_crs)
return(as.numeric(sf::st_coordinates(pt_w)[1, 1:2]))
}

looks_lonlat <- (abs(center[1]) <= 180 && abs(center[2]) <= 90)
if (looks_lonlat) {
pt   <- sf::st_sfc(sf::st_point(center), crs = 4326)
pt_w <- sf::st_transform(pt, working_crs)
return(as.numeric(sf::st_coordinates(pt_w)[1, 1:2]))
}

# Assume it's already in working CRS units
center
}
