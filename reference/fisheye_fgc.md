# Apply Focus-Glue-Context Fisheye transformations

Transforms 2D coordinates using a **Focus–Glue–Context (FGC) fisheye
transformation**. The function expands points inside a focus region,
compresses points in a glue region, and leaves the surrounding context
unchanged. Optionally, a rotational "revolution" can be added to the
glue region to produce a swirling effect.

## Usage

``` r
fisheye_fgc(data, ...)
```

## Arguments

- data:

  Data on which to perform a focus-glue-context transformation.

- ...:

  Additional arguments passed to specific methods. See
  [`fisheye_fgc.matrix()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.matrix.md)
  and
  [`fisheye_fgc.sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.sf.md)
  for details for the methods provided by this package.

## Details

This is a generic S3 function to perform the focus-glue-context
transformation on different types of data.

This function operates in three radial zones around a chosen center:

- **Focus zone (r \<= r_in)**: expands distances from the center using
  `zoom_factor`, but does not exceed the `r_in` boundary.

- **Glue zone (r_in \< r \<= r_out)**: compresses distances using a
  power-law defined by `squeeze_factor`, then remaps them to smoothly
  connect focus and context zones.

- **Context zone (r \> r_out)**: coordinates remain unchanged.

Optionally, points in the glue zone can be rotated (`revolution`) to
emphasize continuity.

For the specifics of methods for spatial objects, see
[`fisheye_fgc.sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.sf.md).
For the underlying mathematical transformation, see
[`fisheye_fgc.matrix()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.matrix.md).

## See also

[`fisheye_fgc.sf()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.sf.md)

[`fisheye_fgc.matrix()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.matrix.md)
