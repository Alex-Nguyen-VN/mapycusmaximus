# Victoria Local Government Areas (`sf`)

An example polygon layer of Victoria's LGAs for demos and tests. Built
from `data-raw/map/LGA_POLYGON.shp`, Z/M dropped, transformed to a
projected CRS, simplified, validated, and reduced to `LGA_NAME` +
geometry.

## Usage

``` r
vic
```

## Format

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object with:

- LGA_NAME:

  Character, LGA name (upper case).

- geometry:

  `MULTIPOLYGON` / `POLYGON` in a projected CRS.

## Source

Prepared in `data-raw/gen-data.R`. Update this if you include an
external data source.

## Details

The CRS stored in the object is whatever `st_crs(vic)` reports at build
time. In `data-raw/gen-data.R` we:

1.  drop Z/M
    ([`st_zm()`](https://r-spatial.github.io/sf/reference/st_zm.html)),

2.  transform to a projected CRS
    ([`st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)),

3.  simplify (`st_simplify(dTolerance = 100)`),

4.  repair geometries
    ([`st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)),

5.  upper-case names and select columns.

## Examples

``` r
library(sf)
plot(sf::st_geometry(vic), col = "grey90", border = "grey50")
```
