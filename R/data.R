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

#' Fisheye-Distorted Hospital–RACF Connections (`sf`)
#'
#' @description
#' An example `LINESTRING` layer showing hospital–RACF transfer routes
#' after applying a **Focus–Glue–Context (FGC) fisheye warp**.
#' It demonstrates how line geometries can be spatially distorted in sync
#' with polygon layers to visualize flow patterns within the magnified focus zone.
#'
#' @format An [`sf`][sf::sf] object with:
#' \describe{
#'   \item{weight}{Numeric, representing transfer magnitude or connection strength.}
#'   \item{geometry}{`LINESTRING` geometries in projected CRS (EPSG:3111).}
#' }
#'
#' @details
#' Built from hospital–RACF coordinate pairs in `data-raw/transfers_coded.csv`
#' using:
#' \enumerate{
#'   \item connection creation via `make_connections()` to form `LINESTRING`s,
#'   \item projection to VicGrid94 (`EPSG:3111`),
#'   \item distance-based filtering to keep only sources within \code{r_in = 0.34}
#'         of the focus point (`cx = 145.0`, `cy = -37.8`),
#'   \item fisheye transformation using [`sf_fisheye()`][mapycusmaximus::sf_fisheye]
#'         with \code{r_in = 0.428}, \code{r_out = 0.429}, and \code{zoom_factor = 1}.
#' }
#' The resulting object aligns spatially with `vic_fish`, allowing
#' co-visualization of regional flow intensity within the distorted focus region.
#'
#' @source Prepared in \code{data-raw/gen-data.R} from
#' `transfers_coded.csv` and the `make_connections()` function.
#'
#' @seealso [`sf_fisheye()`], [`vic_fish`]
#'
#' @examples
#' library(sf)
#' plot(st_geometry(vic_fish), col = "grey95", border = "grey70")
#' plot(st_geometry(conn_fish), add = TRUE, col = "black", lwd = 1)
"conn_fish"
