# Radial fisheye warp for `sf`/`sfc` objects (auto-CRS + flexible centers)

`sf_fisheye()` applies a **focus–glue–context** fisheye to vector data:
it (1) ensures a sensible projected working CRS, (2) **normalizes**
coordinates around a chosen center, (3) calls
[`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)
to warp radii, (4) **denormalizes** back to map units, and (5) restores
the original CRS. Inside the focus ring (`r_in`) features enlarge;
across the glue ring (`r_out`) they transition smoothly; outside, they
stay nearly unchanged.

## Usage

``` r
sf_fisheye(
  sf_obj,
  center = NULL,
  center_crs = NULL,
  normalized_center = FALSE,
  cx = NULL,
  cy = NULL,
  r_in = 0.34,
  r_out = 0.5,
  zoom_factor = 1.5,
  squeeze_factor = 0.35,
  method = "expand",
  revolution = 0,
  target_crs = NULL,
  preserve_aspect = TRUE
)
```

## Arguments

- sf_obj:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sfc`](https://r-spatial.github.io/sf/reference/sfc.html) object.
  Supports `POINT`, `LINESTRING`, `POLYGON`, and `MULTIPOLYGON`. Empty
  geometries are removed before processing.

- center:

  Flexible center specification (see **Center selection**):

  - numeric length-2 pair interpreted via `center_crs` or by lon/lat
    heuristic, or as map units if not lon/lat;

  - any `sf`/`sfc` geometry, from which a centroid is derived;

  - normalized \\\[-1,1\]\\ pair when `normalized_center = TRUE`.

- center_crs:

  Optional CRS for a numeric `center` (e.g., `"EPSG:4326"`). Ignored if
  `center` is an `sf`/`sfc` object (its own CRS is used).

- normalized_center:

  Logical. If `TRUE`, `center` is treated as a normalized \\\[-1,1\]\\
  coordinate around the bbox midpoint.

- cx, cy:

  Optional center in **working CRS** map units (legacy path, ignored
  when `center` is provided).

- r_in, r_out:

  Numeric radii (in **normalized units**) defining focus and glue
  boundaries; must satisfy `r_out > r_in`.

- zoom_factor:

  Numeric (\> 1 to enlarge). Focus magnification passed to
  [`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md).

- squeeze_factor:

  Numeric in \[0, 1\]. Glue-zone compression strength passed to
  [`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md).

- method:

  Character; name understood by
  [`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)
  (default `"expand"`).

- revolution:

  Numeric (radians); optional angular twist for glue zone, passed to
  [`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md).

- target_crs:

  Optional working CRS (anything accepted by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  /
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)).
  If `NULL`, a projected CRS is auto-selected when the input is lon/lat;
  otherwise the input CRS is used.

- preserve_aspect:

  Logical. If `TRUE` (default), use uniform scaling; if `FALSE`, scale
  axes independently (may stretch shapes).

## Value

An object of the same top-level class as `sf_obj` (`sf` or `sfc`), with
geometry coordinates warped by the fisheye and the **original CRS**
restored.

## Details

**CRS handling.** If `target_crs` is `NULL` and the input is geographic
(lon/lat), a projected **working CRS** is chosen from the layer’s
centroid:

- Victoria, AU region (≈ 140–150°E, 40–30°S): **EPSG:7855** (GDA2020 /
  MGA55).

- Otherwise UTM: **EPSG:326##** (north) or **EPSG:327##** (south).

You may override with `target_crs`. The original CRS is restored on
return.

**Center selection.** The fisheye center can be supplied in multiple
ways:

- `center = c(lon, lat)`, with `center_crs = "EPSG:4326"` (recommended
  for WGS84) or another CRS string/object.

- `center = c(x, y)` already in **working CRS** map units (meters).

- `center` as any `sf`/`sfc` geometry (POINT/LINE/POLYGON/etc.): its
  **centroid of the combined geometry** is used, then transformed to the
  working CRS.

- `center = c(cx, cy)` as **normalized** coordinates in \\\[-1,1\]\\
  when `normalized_center = TRUE` (relative to the bbox midpoint and
  scale used for normalization).

- Legacy `cx, cy` (map units) are still accepted and used only when
  `center` is not supplied.

**Normalization.** Let bbox half-width/height be `sx`, `sy`. With
`preserve_aspect = TRUE` (default), a uniform scale `s = max(sx, sy)`
maps \\(x, y) \mapsto ((x - cx)/s,\\ (y - cy)/s)\\, so `r_in`/`r_out`
(e.g., 0.34/0.5) are interpreted in a unit-like space. If
`preserve_aspect = FALSE`, X and Y are independently scaled by `sx` and
`sy`.

**Implementation notes.** Geometry coordinates are transformed by
[`st_transform_custom()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/st_transform_custom.md)
which safely re-closes polygon rings and drops Z/M. The radial warp
itself is delegated to
[`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)
(which is not modified).

The transformation may introduce self-intersections or other topology
issues due to geometric warping. To ensure the output is suitable for
plotting and spatial operations, the geometry is repaired using
[`lwgeom::lwgeom_make_valid()`](https://r-spatial.github.io/lwgeom/reference/lwgeom_make_valid.html).
Users should be aware that:

- geometry types may be promoted (e.g., POLYGON → MULTIPOLYGON),

- tiny sliver polygons may be removed,

- invalid rings or bow-tie shapes will be corrected,

- the repair step requires the `{lwgeom}` package.

## See also

[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html),
[`sf::st_is_longlat()`](https://r-spatial.github.io/sf/reference/st_is_longlat.html),
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html),
[`sf::st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.html),
[`st_transform_custom()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/st_transform_custom.md),
[`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)
[`lwgeom::lwgeom_make_valid()`](https://r-spatial.github.io/lwgeom/reference/lwgeom_make_valid.html),
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)

## Examples

``` r
library(sf)

# Toy polygon in a projected CRS
poly <- st_sfc(st_polygon(list(rbind(
  c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
))), crs = 3857)

# Default center (bbox midpoint), gentle magnification
out1 <- sf_fisheye(poly, r_in = 0.3, r_out = 0.6,
                   zoom_factor = 1.5, squeeze_factor = 0.35)
#> Error in UseMethod("st_geometry<-"): no applicable method for 'st_geometry<-' applied to an object of class "c('sfc_POLYGON', 'sfc')"

# Explicit map-unit center, stronger focus
out2 <- sf_fisheye(poly, cx = 0.5, cy = 0.5,
                   r_in = 0.25, r_out = 0.55,
                   zoom_factor = 2.0, squeeze_factor = 0.25)
#> Error in UseMethod("st_geometry<-"): no applicable method for 'st_geometry<-' applied to an object of class "c('sfc_POLYGON', 'sfc')"

# Lon/lat point (auto-project to UTM/MGA), then fisheye around CBD (WGS84)
pt_ll <- st_sfc(st_point(c(144.9631, -37.8136)), crs = 4326)  # Melbourne CBD
out3  <- sf_fisheye(pt_ll, r_in = 0.2, r_out = 0.5)
#> Error in UseMethod("st_geometry<-"): no applicable method for 'st_geometry<-' applied to an object of class "c('sfc_POINT', 'sfc')"

# Center supplied as an sf polygon: centroid is used as the warp center
# out4 <- sf_fisheye(vic_polygon_sf, center = vic_polygon_sf)
```
