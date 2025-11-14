# Create a Regular Test Grid of Coordinates

Generates a 2D grid of equally spaced points, useful for testing fisheye
transformations and other spatial warping functions.

## Usage

``` r
create_test_grid(range = c(-1, 1), spacing = 0.1)
```

## Arguments

- range:

  Numeric vector of length 2 giving the x and y limits of the grid
  (default = `c(-1, 1)`).

- spacing:

  Numeric. Distance between adjacent grid points along each axis
  (default = `0.1`).

## Value

A numeric matrix with two columns (`x`, `y`) containing the coordinates
of the grid points.

## See also

[`plot_fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/plot_fisheye_fgc.md),
[`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)

## Examples

``` r
# Create a grid from -1 to 1 with spacing 0.25
grid <- create_test_grid(range = c(-1, 1), spacing = 0.25)
head(grid)
#>          x  y
#> [1,] -1.00 -1
#> [2,] -0.75 -1
#> [3,] -0.50 -1
#> [4,] -0.25 -1
#> [5,]  0.00 -1
#> [6,]  0.25 -1
```
