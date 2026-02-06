# Extract Polygon Coordinates from sf Objects

Converts `sf` polygon or multipolygon geometries into a list structure
containing coordinate arrays, suitable for serialization to JSON or use
in JavaScript visualizations. Preserves both exterior rings and holes.

## Usage

``` r
polygons_from_sf(sf_obj, id_col = NULL)
```

## Arguments

- sf_obj:

  An `sf` or `sfc` object containing `POLYGON` or `MULTIPOLYGON`
  geometries.

- id_col:

  Character. Optional column name to use as polygon IDs. If `NULL`, IDs
  are generated as `"poly-1"`, `"poly-2"`, etc. (default = `NULL`).

## Value

A list of lists, each containing:

- `id`: Character identifier for the polygon

- `rings`: List of coordinate rings, where each ring is a list of
  `[x, y]` coordinate pairs. The first ring is the exterior boundary;
  subsequent rings (if present) are holes.

## Details

This function is primarily used to prepare spatial data for client-side
rendering in web applications. Each polygon may contain multiple rings
(exterior + holes), and multipolygons are decomposed into separate ring
lists.

The output format is compatible with JavaScript mapping libraries and
SVG path generation.

## See also

[`lines_from_sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/lines_from_sf.md),
[`points_from_sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/points_from_sf.md),
[`shiny_fisheye()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/shiny_fisheye.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create a simple polygon
poly <- st_polygon(list(
  matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
))
sf_obj <- st_sf(id = "test", geometry = st_sfc(poly))

# Extract coordinates
coords <- polygons_from_sf(sf_obj, id_col = "id")
str(coords)
} # }
```
