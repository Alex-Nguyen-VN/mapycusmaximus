# Fisheye-Distorted Hospital–RACF Connections (`sf`)

An example `LINESTRING` layer showing hospital–RACF transfer routes
after applying a **Focus–Glue–Context (FGC) fisheye warp**. It
demonstrates how line geometries can be spatially distorted in sync with
polygon layers to visualize flow patterns within the magnified focus
zone.

## Usage

``` r
conn_fish
```

## Format

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object with:

- weight:

  Numeric, representing transfer magnitude or connection strength.

- geometry:

  `LINESTRING` geometries in projected CRS (EPSG:3111).

## Source

Prepared in `data-raw/gen-data.R` from `transfers_coded.csv` and the
`make_connections()` function.

## Details

Built from hospital–RACF coordinate pairs in
`data-raw/transfers_coded.csv` using:

1.  connection creation via `make_connections()` to form `LINESTRING`s,

2.  projection to VicGrid94 (`EPSG:3111`),

3.  distance-based filtering to keep only sources within `r_in = 0.34`
    of the focus point (`cx = 145.0`, `cy = -37.8`),

4.  fisheye transformation using
    [`sf_fisheye()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/sf_fisheye.md)
    with `r_in = 0.428`, `r_out = 0.429`, and `zoom_factor = 1`.

The resulting object aligns spatially with `vic_fish`, allowing
co-visualization of regional flow intensity within the distorted focus
region.

## See also

[`sf_fisheye()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/sf_fisheye.md),
[`vic_fish`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/vic_fish.md)

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
plot(st_geometry(vic_fish), col = "grey95", border = "grey70")
plot(st_geometry(conn_fish), add = TRUE, col = "black", lwd = 1)
```
