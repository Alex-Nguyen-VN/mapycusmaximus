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

#' Fisheye-Distorted Victoria Map and Connection Lines (`sf`)
#'
#' @description
#' Example spatial layers illustrating the **Focus–Glue–Context (FGC) fisheye transformation**  
#' applied to Victoria’s LGA polygons and hospital–RACF transfer connections.
#' These are used in package demos and tests to showcase local magnification
#' around a geographic focus point while preserving global context.
#'
#' @format
#' - `vic_fish`: An [`sf`][sf::sf] **polygon layer** of Victoria’s LGAs after fisheye warping.  
#' - `conn_fish`: An [`sf`][sf::sf] **linestring layer** connecting hospital and RACF pairs,  
#'   subset to features inside the fisheye focus region and warped with the same parameters.
#'
#' Each object includes:
#' \describe{
#'   \item{geometry}{`MULTIPOLYGON` or `LINESTRING` geometries in projected CRS (EPSG:3111).}
#'   \item{Optional attributes}{such as `LGA_NAME` (for polygons) or transfer metadata (`weight`, `src`, `dst`).}
#' }
#'
#' @details
#' Both layers were built in `data-raw/gen-data.R` using:
#' \enumerate{
#'   \item hospital–RACF coordinates from `data-raw/transfers_coded.csv`,
#'   \item connection geometries created via `make_connections()`,
#'   \item filtering to connections within `r_in = 0.34` of the focus center,
#'   \item projection to GDA94 / Vicgrid94 (`EPSG:3111`),
#'   \item fisheye transformation using [`sf_fisheye()`][mapycusmaximus::sf_fisheye],
#'         with parameters \code{r_in = 0.34}, \code{r_out = 0.5},
#'         and a moderate zoom factor (\code{zoom_factor = 1}).
#' }
#'
#' The resulting objects represent a **smooth local zoom** around the chosen
#' center point (`cx = 145.0`, `cy = -37.8`), demonstrating how spatial geometries
#' are reshaped by the fisheye warp while maintaining topological integrity.
#'
#' @source Prepared in \code{data-raw/gen-data.R} from the
#' Victorian LGA boundaries (`vic`) and hospital–RACF transfer data (`transfers_coded.csv`).
#'
#' @seealso
#' - [`sf_fisheye()`] for the transformation logic  
#' - [`vic`] for the unwarped baseline polygon layer
#'
#' @examples
#' library(sf)
#' plot(st_geometry(vic_fish), col = "grey90", border = "grey50")
#' plot(st_geometry(conn_fish), add = TRUE, col = "black", lwd = 1)
"vic_fish"
"conn_fish"
