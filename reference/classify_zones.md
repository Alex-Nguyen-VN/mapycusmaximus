# Classify Coordinates into Focus, Glue, or Context Zones

Assigns each point to one of three zones based on its radial distance
from a specified center:

- **focus**: inside the inner radius `r_in`

- **glue**: between `r_in` and `r_out`

- **context**: outside `r_out`

This is a helper for visualizing and analyzing fisheye transformations
using the Focus–Glue–Context (FGC) model.

## Usage

``` r
classify_zones(coords, cx = 0, cy = 0, r_in = 0.34, r_out = 0.5)
```

## Arguments

- coords:

  A numeric matrix or data frame with at least two columns representing
  `(x, y)` coordinates.

- cx, cy:

  Numeric. The x and y coordinates of the fisheye center (default = 0,
  0).

- r_in:

  Numeric. Inner radius of the focus zone (default = 0.34).

- r_out:

  Numeric. Outer radius of the glue zone (default = 0.5).

## Value

A character vector of the same length as `nrow(coords)`, with values
`"focus"`, `"glue"`, or `"context"`.

## See also

[`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md),
[`plot_fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/plot_fisheye_fgc.md)

## Examples

``` r
# Simple example
pts <- matrix(c(0, 0, 0.2, 0.2, 0.6, 0.6), ncol = 2, byrow = TRUE)
classify_zones(pts, r_in = 0.3, r_out = 0.5)
#> [1] "focus"   "focus"   "context"
#> "focus"   "glue"    "context"
```
