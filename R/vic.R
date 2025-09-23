#' Victoria Local Government Areas (`sf`)
#'
#' @description
#' An example polygon layer of Victoria's LGAs for demos and tests.
#' Built from `data-raw/map/LGA_POLYGON.shp`, Z/M dropped, transformed to a
#' projected CRS, simplified, validated, and reduced to `LGA_NAME` + geometry.
#'
#' @format An [`sf`][sf::sf] object with:
#' \describe{
#'   \item{LGA_NAME}{Character, LGA name (upper case).}
#'   \item{geometry}{`MULTIPOLYGON` / `POLYGON` in a projected CRS.}
#' }
#'
#' @details
#' The CRS stored in the object is whatever `st_crs(vic)` reports at build time.
#' In `data-raw/gen-data.R` we:
#' \enumerate{
#'   \item drop Z/M (\code{st_zm()}),
#'   \item transform to a projected CRS (\code{st_transform()}),
#'   \item simplify (\code{st_simplify(dTolerance = 100)}),
#'   \item repair geometries (\code{st_make_valid()}),
#'   \item upper-case names and select columns.
#' }
#'
#' @source Prepared in \code{data-raw/gen-data.R}. Update this if you include an
#' external data source.
#'
#' @examples
#' library(sf)
#' plot(sf::st_geometry(vic), col = "grey90", border = "grey50")
"vic"
