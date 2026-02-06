# Extract Point Coordinates from sf Objects

Converts `sf` point geometries into a list structure containing
coordinate pairs, suitable for serialization to JSON or use in
JavaScript visualizations.

## Usage

``` r
points_from_sf(sf_obj, id_col = NULL)
```

## Arguments

- sf_obj:

  An `sf` or `sfc` object containing `POINT` geometries.

- id_col:

  Character. Optional column name to use as point IDs. If `NULL`, IDs
  are generated as sequential integers. (default = `NULL`).

## Value

A list of lists, each containing:

- `id`: Character identifier for the point

- `x`: Numeric x-coordinate

- `y`: Numeric y-coordinate

## Details

This function prepares point geometries for client-side rendering as
circles or markers in SVG or Canvas visualizations.

## See also

[`polygons_from_sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/polygons_from_sf.md),
[`lines_from_sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/lines_from_sf.md),
[`shiny_fisheye()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/shiny_fisheye.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create simple points
pts <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)))
sf_obj <- st_sf(id = c("A", "B"), geometry = pts)

# Extract coordinates
coords <- points_from_sf(sf_obj, id_col = "id")
str(coords)
} # }
```
