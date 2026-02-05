#' Radial fisheye warp for `sf`/`sfc` objects (auto-CRS + flexible centers)
#'
#' @description
#' `sf_fisheye()` applies a **focus–glue–context** fisheye to vector data:
#' it (1) ensures a sensible projected working CRS, (2) **normalizes**
#' coordinates around a chosen center, (3) calls `fisheye_fgc()` to warp radii,
#' (4) **denormalizes** back to map units, and (5) restores the original CRS.
#' Inside the focus ring (`r_in`) features enlarge; across the glue ring
#' (`r_out`) they transition smoothly; outside, they stay nearly unchanged.
#'
#' @details
#' **CRS handling.** If `target_crs` is `NULL` and the input is geographic
#' (lon/lat), a projected **working CRS** is chosen from the layer’s centroid:
#' \itemize{
#'   \item Victoria, AU region (approximate 140–150°E, 40–30°S): **EPSG:7855** (GDA2020 / MGA55).
#'   \item Otherwise UTM: **EPSG:326##** (north) or **EPSG:327##** (south).
#' }
#' You may override with `target_crs`. The original CRS is restored on return.
#'
#' **Center selection.** The fisheye center can be supplied in multiple ways:
#' \itemize{
#'   \item `center = c(lon, lat)`, with `center_crs = "EPSG:4326"` (recommended
#'         for WGS84) or another CRS string/object.
#'   \item `center = c(x, y)` already in **working CRS** map units (meters).
#'   \item `center` as any `sf`/`sfc` geometry (POINT/LINE/POLYGON/etc.): its
#'         **centroid of the combined geometry** is used, then transformed to the
#'         working CRS.
#'   \item `center = c(cx, cy)` as **normalized** coordinates in \eqn{[-1,1]}
#'         when `normalized_center = TRUE` (relative to the bbox midpoint and
#'         scale used for normalization).
#'   \item Legacy `cx, cy` (map units) are still accepted and used only when
#'         `center` is not supplied.
#' }
#'
#' **Normalization.** Let bbox half-width/height be `sx`, `sy`. With
#' `preserve_aspect = TRUE` (default), a uniform scale `s = max(sx, sy)` maps
#' \eqn{(x, y) \mapsto ((x - cx)/s,\, (y - cy)/s)}, so `r_in`/`r_out` (e.g.,
#' 0.34/0.5) are interpreted in a unit-like space. If `preserve_aspect = FALSE`,
#' X and Y are independently scaled by `sx` and `sy`.
#'
#' **Implementation notes.** Geometry coordinates are transformed by
#' `st_transform_custom()` which safely re-closes polygon rings and drops Z/M.
#' The radial warp itself is delegated to `fisheye_fgc()` (which is not modified).
#'
#' @param sf_obj An [`sf`][sf::sf] or [`sfc`][sf::st_sfc] object. Supports
#'   `POINT`, `LINESTRING`, `POLYGON`, and `MULTIPOLYGON`. Empty geometries
#'   are removed before processing.
#' @param center Flexible center specification (see **Center selection**):
#'   \itemize{
#'     \item numeric length-2 pair interpreted via `center_crs` or by lon/lat
#'           heuristic, or as map units if not lon/lat;
#'     \item any `sf`/`sfc` geometry, from which a centroid is derived;
#'     \item normalized \eqn{[-1,1]} pair when `normalized_center = TRUE`.
#'   }
#' @param center_crs Optional CRS for a numeric `center` (e.g., `"EPSG:4326"`).
#'   Ignored if `center` is an `sf`/`sfc` object (its own CRS is used).
#' @param normalized_center Logical. If `TRUE`, `center` is treated as a
#'   normalized \eqn{[-1,1]} coordinate around the bbox midpoint.
#' @param cx,cy Optional center in **working CRS** map units (legacy path,
#'   ignored when `center` is provided).
#' @param r_in,r_out Numeric radii (in **normalized units**) defining focus and
#'   glue boundaries; must satisfy `r_out > r_in`.
#' @param zoom_factor Numeric (> 1 to enlarge). Focus magnification passed to
#'   `fisheye_fgc()`.
#' @param squeeze_factor Numeric in \[0, 1\]. Glue-zone compression strength
#'   passed to `fisheye_fgc()`.
#' @param method Character; name understood by `fisheye_fgc()` (default `"expand"`).
#' @param revolution Numeric (radians); optional angular twist for glue zone,
#'   passed to `fisheye_fgc()`.
#' @param target_crs Optional working CRS (anything accepted by
#'   [sf::st_crs()] / [sf::st_transform()]). If `NULL`, a projected CRS is
#'   auto-selected when the input is lon/lat; otherwise the input CRS is used.
#' @param preserve_aspect Logical. If `TRUE` (default), use uniform scaling; if
#'   `FALSE`, scale axes independently (may stretch shapes).
#'
#' @return An object of the same top-level class as `sf_obj` (`sf` or `sfc`),
#'   with geometry coordinates warped by the fisheye and the **original CRS**
#'   restored. 
#' 
#' @details
#' The transformation may introduce self-intersections or other topology
#' issues due to geometric warping. 
#'
#' @examples
#' library(sf)
#'
#' # Toy polygon in a projected CRS
#' poly <- st_sfc(st_polygon(list(rbind(
#'   c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
#' ))), crs = 3857)
#'
#' # Default center (bbox midpoint), gentle magnification
#' out1 <- sf_fisheye(poly, r_in = 0.3, r_out = 0.6,
#'                    zoom_factor = 1.5, squeeze_factor = 0.35)
#'
#' # Explicit map-unit center, stronger focus
#' out2 <- sf_fisheye(poly, cx = 0.5, cy = 0.5,
#'                    r_in = 0.25, r_out = 0.55,
#'                    zoom_factor = 2.0, squeeze_factor = 0.25)
#'
#' # Lon/lat point (auto-project to UTM/MGA), then fisheye around CBD (WGS84)
#' pt_ll <- st_sfc(st_point(c(144.9631, -37.8136)), crs = 4326)  # Melbourne CBD
#' out3  <- sf_fisheye(pt_ll, r_in = 0.2, r_out = 0.5)
#'
#' # Center supplied as an sf polygon: centroid is used as the warp center
#' out4 <- sf_fisheye(poly, center = poly)
#'
#' @seealso
#' [sf::st_transform()], [sf::st_is_longlat()], [sf::st_crs()],
#' [sf::st_coordinates()], `st_transform_custom()`, `fisheye_fgc()`

#'
#' @importFrom sf st_is_empty st_zm st_crs st_bbox st_transform st_is_longlat st_set_geometry st_geometry
#' @export


sf_fisheye <- function(
  sf_obj,
  center = NULL,              # accepts c(lon,lat), c(x,y in map units), normalized pair, or sf/sfc POINT
  center_crs = NULL,          # e.g. "EPSG:4326"; if NULL we auto-guess (lon/lat vs map units)
  normalized_center = FALSE,  # TRUE if 'center' is in [-1,1] normalized coords
  cx = NULL, cy = NULL,       # legacy map-unit center still supported; ignored if 'center' given
  r_in = 0.34, r_out = 0.5,
  zoom_factor = 1.5,
  squeeze_factor = 0.35,
  method = "expand",
  revolution = 0.0,
  target_crs = NULL,
  preserve_aspect = TRUE
) {
  stopifnot(r_out > r_in)
  stopifnot(inherits(sf_obj, c("sf", "sfc")))

  if (inherits(sf_obj, "sf")) {
    sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  }

  sf_obj <- sf::st_zm(sf_obj, drop = TRUE, what = "ZM")

  # --- choose working CRS (projected) ---
  original_crs <- sf::st_crs(sf_obj)

  if (!is.null(target_crs)) {
    sf_obj <- sf::st_transform(sf_obj, target_crs)

  } else if (sf::st_is_longlat(sf_obj)) {
    bb <- sf::st_bbox(sf_obj)
    lon_center <- (bb["xmin"] + bb["xmax"]) / 2
    lat_center <- (bb["ymin"] + bb["ymax"]) / 2

    if (lon_center > 140 && lon_center < 150 && lat_center > -40 && lat_center < -30) {
      sf_obj <- sf::st_transform(sf_obj, "EPSG:7855")  # GDA2020 / MGA Zone 55
    } else {
      utm_zone <- floor((lon_center + 180) / 6) + 1
      epsg <- if (lat_center >= 0) {
        paste0("EPSG:", 32600 + utm_zone)
      } else {
        paste0("EPSG:", 32700 + utm_zone)
      }
      sf_obj <- sf::st_transform(sf_obj, epsg)
    }
  }

  working_crs <- sf::st_crs(sf_obj)

  # --- bbox + scale ---
  bb <- sf::st_bbox(sf_obj)
  sx <- (bb["xmax"] - bb["xmin"]) / 2; if (sx == 0) sx <- 1
  sy <- (bb["ymax"] - bb["ymin"]) / 2; if (sy == 0) sy <- 1
  s  <- max(sx, sy)

  norm_fun <- if (preserve_aspect) {
    function(M, cxy) cbind((M[, 1] - cxy[1]) / s,  (M[, 2] - cxy[2]) / s)
  } else {
    function(M, cxy) cbind((M[, 1] - cxy[1]) / sx, (M[, 2] - cxy[2]) / sy)
  }

  denorm_fun <- if (preserve_aspect) {
    function(M, cxy) cbind(cxy[1] + M[, 1] * s,  cxy[2] + M[, 2] * s)
  } else {
    function(M, cxy) cbind(cxy[1] + M[, 1] * sx, cxy[2] + M[, 2] * sy)
  }

  # --- resolve center precedence ---
  if (!is.null(center)) {
    cxy <- mapycusmaximus::.resolve_center(center, center_crs, working_crs, bb,
                           preserve_aspect, normalized_center)
  } else {
    if (is.null(cx) || is.null(cy)) {
      cxy <- c((bb["xmin"] + bb["xmax"]) / 2, (bb["ymin"] + bb["ymax"]) / 2)
    } else {
      cxy <- c(cx, cy)
    }
  }

  # --- fisheye wrapper ---
  base_args <- list(
    cx = 0, cy = 0,
    r_in = r_in, r_out = r_out,
    zoom_factor = zoom_factor,
    squeeze_factor = squeeze_factor,
    method = method,
    revolution = revolution
  )

  wrapped_fisheye <- function(coords, ...) {
    M <- as.matrix(coords[, 1:2, drop = FALSE])
    N <- norm_fun(M, cxy)
    T <- do.call(fisheye_fgc, c(list(N), base_args))
    denorm_fun(T, cxy)
  }

  out <- st_transform_custom(sf_obj, transform_fun = wrapped_fisheye, args = list())

  # restore original CRS if needed
  if (!identical(sf::st_crs(out), original_crs)) {
    out <- sf::st_transform(out, original_crs)
  }

  return(out)
}

#' Resolve a user-supplied center into the working CRS (internal)
#'
#' @description
#' Converts a flexible \emph{center} specification into a 2D coordinate
#' (x, y) expressed in the map's **working projected CRS**. This helper is
#' meant for internal use inside \code{sf_fisheye()}.
#'
#' @details
#' The \code{center} argument can be:
#' \itemize{
#'   \item An \strong{sf/sfc} geometry of any type (POINT/LINESTRING/POLYGON, multi*, collections).
#'         If not a single POINT, the function combines and takes the centroid
#'         (\code{st_centroid(st_combine(g))}). The geometry is then transformed
#'         to \code{working_crs}.
#'   \item A \strong{normalized} pair \code{c(cx, cy)} in \eqn{[-1,1]} if
#'         \code{normalized_center = TRUE}. It is mapped to map units using the
#'         bounding box center and scale (uniform if \code{preserve_aspect = TRUE}).
#'   \item A \strong{numeric} pair in a known CRS via \code{center_crs}
#'         (e.g., \code{"EPSG:4326"}), which is transformed into \code{working_crs}.
#'   \item A \strong{numeric} pair without CRS. A heuristic is applied:
#'         if values look like lon/lat (\eqn{|lon|\le 180}, \eqn{|lat|\le 90}),
#'         they are treated as WGS84 and transformed to \code{working_crs};
#'         otherwise they are assumed to already be in \code{working_crs}.
#' }
#'
#' If \code{center} is \code{NULL} or empty, the bbox midpoint
#' \code{((xmin+xmax)/2, (ymin+ymax)/2)} is returned.
#'
#' @param center An \code{sf}/\code{sfc} object, or a numeric length-2 vector.
#'   See \emph{Details} for accepted forms and interpretation.
#' @param center_crs Optional CRS for a numeric \code{center}; e.g. \code{"EPSG:4326"}.
#'   Ignored if \code{center} is an \code{sf}/\code{sfc} object (its own CRS is used).
#' @param working_crs The target (projected) CRS into which the center should be expressed;
#'   typically \code{st_crs(sf_obj)} after reprojecting to a working CRS.
#' @param bbox A bbox vector as returned by \code{sf::st_bbox()}, used for defaulting to
#'   the map midpoint and for normalized-center scaling.
#' @param preserve_aspect Logical. If \code{TRUE}, normalized centers use a uniform scale
#'   based on \code{max(x-span, y-span)}; otherwise per-axis scaling is used.
#' @param normalized_center Logical. If \code{TRUE}, \code{center} is assumed to be in
#'   \eqn{[-1,1]} relative coordinates around the bbox midpoint.
#'
#' @return A numeric length-2 vector \code{c(x, y)} in \code{working_crs} map units.
#'
#' @keywords internal
#' @noRd


.resolve_center <- function(
  center,
  center_crs,
  working_crs,
  bbox,
  preserve_aspect,
  normalized_center
) {
  # Fallback = bbox center
  cx0 <- (bbox["xmin"] + bbox["xmax"]) / 2
  cy0 <- (bbox["ymin"] + bbox["ymax"]) / 2
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

    sx <- (bbox["xmax"] - bbox["xmin"]) / 2; if (sx == 0) sx <- 1
    sy <- (bbox["ymax"] - bbox["ymin"]) / 2; if (sy == 0) sy <- 1

    if (preserve_aspect) {
      s <- max(sx, sy)
      return(c(cx0 + center[1] * s, cy0 + center[2] * s))
    } else {
      return(c(cx0 + center[1] * sx, cy0 + center[2] * sy))
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
