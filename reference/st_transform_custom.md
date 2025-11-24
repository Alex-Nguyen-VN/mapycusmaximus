# Apply a custom coordinate transform to an sf/sfc object (POINT/LINESTRING/POLYGON/MULTIPOLYGON)

`st_transform_custom()` walks through each geometry in an `sf`/`sfc`
object, extracts its XY coordinates, applies a user-supplied
transformation function to those coordinates, and rebuilds the geometry.
It preserves the input CRS on the resulting `sfc` column. Polygon rings
are re-closed after transformation so the first and last vertex match.

## Usage

``` r
st_transform_custom(sf_obj, transform_fun, args)
```

## Arguments

- sf_obj:

  An object of class `sf` or `sfc`. Supported geometry types: `POINT`,
  `LINESTRING`, `POLYGON`, and `MULTIPOLYGON`.

- transform_fun:

  A function that accepts a numeric matrix of coordinates with two
  columns `(X, Y)` and returns a transformed numeric matrix with the
  same number of rows and two columns. For example:
  `function(coords, ...) cbind(f(coords[,1], ...), g(coords[,2], ...))`.

- args:

  A named list of additional arguments to pass to `transform_fun`. These
  are appended after the `coords` matrix via
  [`do.call()`](https://rdrr.io/r/base/do.call.html), i.e.
  `do.call(transform_fun, c(list(coords), args))`.

## Value

An object of the same top-level class as `sf_obj` (`sf` or `sfc`), with
the same column structure (if `sf`) and the same CRS as the input.
Geometry coordinates are replaced by the coordinates returned by
`transform_fun`.

## Details

For `POLYGON`/`MULTIPOLYGON`, the function uses the ring indices
returned by
[`sf::st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.html)
(`L1` for rings and `L2` for parts) to transform each ring
independently, and then ensures each ring is explicitly closed (last
vertex equals first vertex).

Error handling is per-geometry: if a geometry fails to transform, a
warning is emitted and an empty geometry of the same "polygonal family"
is returned to keep list lengths consistent.

The function **does not** modify or interpret the CRS numerically; it
simply preserves the CRS attribute on the output `sfc`. If your
transformation assumes metres (e.g., radial warps), ensure the input is
in an appropriate projected CRS before calling this function.

## Expected signature of `transform_fun`

    transform_fun <- function(coords, ...) {  ## coords: n x 2 matrix (X, Y)
      ## return an n x 2 matrix with transformed (X, Y)}

## See also

[`sf::st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.html),
[`sf::st_geometry_type()`](https://r-spatial.github.io/sf/reference/st_geometry_type.html),
[`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html),
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)

## Examples

``` r
library(sf)

# A simple coordinate transformer: scale and shift
scale_shift <- function(coords, sx = 1, sy = 1, dx = 0, dy = 0) {
 X <- coords[, 1] * sx + dx
 Y <- coords[, 2] * sy + dy
  cbind(X, Y)
}

# POINT example
pt <- st_sfc(st_point(c(0, 0)), crs = 3857)
st_transform_custom(pt, transform_fun = scale_shift,
                    args = list(sx = 2, sy = 2, dx = 1000, dy = -500))
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1000 ymin: -500 xmax: 1000 ymax: -500
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> POINT (1000 -500)

# LINESTRING example
ln <- st_sfc(st_linestring(rbind(c(0, 0), c(1, 0), c(1, 1))), crs = 3857)
st_transform_custom(ln, transform_fun = scale_shift,
                    args = list(sx = 10, sy = 10))
#> Geometry set for 1 feature 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 10 ymax: 10
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> LINESTRING (0 0, 10 0, 10 10)

# POLYGON example (unit square)
poly <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1),
                                     c(0,1), c(0,0)))), crs = 3857)
st_transform_custom(poly, transform_fun = scale_shift,
                    args = list(sx = 2, sy = 0.5, dx = 5))
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))

# MULTIPOLYGON example (two disjoint squares)
mp <- st_sfc(st_multipolygon(list(
  list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))),
  list(rbind(c(2,2), c(3,2), c(3,3), c(2,3), c(2,2)))
)), crs = 3857)
st_transform_custom(mp, transform_fun = scale_shift,
                    args = list(dx = 100, dy = 100))
#> Geometry set for 1 feature 
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: 100 xmax: 103 ymax: 103
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> MULTIPOLYGON (((100 100, 101 100, 101 101, 100 ...

# In an sf data frame
sf_df <- st_sf(id = 1:2, geometry = st_sfc(
  st_point(c(10, 10)),
  st_linestring(rbind(c(0,0), c(2,0), c(2,2)))
), crs = 3857)

st_transform_custom(sf_df, transform_fun = scale_shift,
                    args = list(sx = 3, sy = 3))
#> Simple feature collection with 2 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 30 ymax: 30
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   id                   geometry
#> 1  1              POINT (30 30)
#> 2  2 LINESTRING (0 0, 6 0, 6 6)
```
