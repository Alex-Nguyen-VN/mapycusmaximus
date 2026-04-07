# Fisheye-Distorted Victoria LGAs (`sf`)

An example polygon layer of Victoria’s Local Government Areas (LGAs)
after applying a **Focus–Glue–Context (FGC) fisheye transformation**.
This dataset illustrates how local detail can be magnified around a
chosen focus point while maintaining geographic context across the
state.

## Usage

``` r
vic_fish
```

## Format

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object with:

- LGA_NAME:

  Character, name of the LGA (upper case).

- geometry:

  `MULTIPOLYGON` / `POLYGON` geometries in projected CRS (EPSG:3111).

## Source

Prepared in `data-raw/gen-data.R` using the original `vic` polygon
layer.

## Details

Built from the base layer `vic` using:

1.  projection to VicGrid94 (`st_transform(vic, 3111)`),

2.  defining a focus center near Melbourne (`cx = 145.0`, `cy = -37.8`),

3.  applying
    [`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)
    with `r_in = 0.34`, `r_out = 0.5`, and `zoom_factor = 1`,

4.  preserving topology with
    [`st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
    where needed.

The result is a smoothly warped map emphasizing the metropolitan focus
zone.

## See also

[`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md),
[`conn_fish`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/conn_fish.md)

## Examples

``` r
library(sf)
plot(st_geometry(vic_fish), col = "grey90", border = "grey50")
```
