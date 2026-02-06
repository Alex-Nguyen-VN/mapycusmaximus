# Extract Line Coordinates from sf Objects

Converts `sf` line or multiline geometries into a list structure
containing coordinate arrays, suitable for serialization to JSON or use
in JavaScript visualizations.

## Usage

``` r
lines_from_sf(sf_obj, id_col = NULL)
```

## Arguments

- sf_obj:

  An `sf` or `sfc` object containing `LINESTRING` or `MULTILINESTRING`
  geometries.

- id_col:

  Character. Optional column name to use as line IDs. If `NULL`, IDs are
  generated as `"ln-1"`, `"ln-2"`, etc. (default = `NULL`).

## Value

A list of lists, each containing:

- `id`: Character identifier for the line

- `coords`: List of `[x, y]` coordinate pairs representing the line
  vertices in sequence

## Details

This function prepares line geometries for client-side rendering.
Multilinestrings are handled by extracting all coordinate points in
order, which may or may not be appropriate depending on the use case.

## See also

[`polygons_from_sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/polygons_from_sf.md),
[`points_from_sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/points_from_sf.md),
[`shiny_fisheye()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/shiny_fisheye.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create a simple linestring
line <- st_linestring(matrix(c(0,0, 1,1, 2,0), ncol = 2, byrow = TRUE))
sf_obj <- st_sf(id = "route1", geometry = st_sfc(line))

# Extract coordinates
coords <- lines_from_sf(sf_obj, id_col = "id")
str(coords)
} # }
```
