# fisheye

<!-- badges: start -->

[![R-CMD-check](https://github.com/Alex-Nguyen-VN/mapycusmaximus/workflows/R-CMD-check/badge.svg)](https://github.com/Alex-Nguyen-VN/mapycusmaximus/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

> **Focus–Glue–Context (FGC) Fisheye Transformations for R**

I am `mapycusmaximus` — the Focus + Glue + Context package. Mapper of the vast frontiers, master of radial transformations, and loyal servant to the truth of clear data visualization. Creator of focus where there was distortion. Defender of detail where there was crowding. And I will have my clarity — in this map or the next.

---

## ✨ Features

* 🎯 **FGC transformation**: Focus enlargement, glue compression, context preservation
* 🗺️ **`sf` integration**: Works directly on spatial geometries with automatic CRS handling
* 📍 **Flexible center specification**: Choose centers in lon/lat, projected map units, normalized coords, or even from other `sf` objects
* 🔄 **Customizable parameters**: Control zoom, squeeze, and optional angular twist
* 📊 **Visualization helpers**: Plot original vs transformed coordinates for quick inspection
* ⚡ **Efficient implementation**: Vectorized transformations, polygon rings re-closed safely

---

## 📦 Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Alex-Nguyen-VN/mapycusmaximus")
```

---

## 🚀 Quick Start

### Basic coordinate transformation

```r
library(fisheye)

grid <- create_test_grid(range = c(-1, 1), spacing = 0.1)

transformed <- fisheye_fgc(
  coords = grid,
  r_in = 0.34, r_out = 0.5,
  zoom_factor = 1.5,
  squeeze_factor = 0.3
)

plot_fisheye_fgc(grid, transformed, r_in = 0.34, r_out = 0.5)
```

### Spatial data (`sf`) integration

```r
library(sf)
library(fisheye)

poly <- st_sfc(st_polygon(list(rbind(
  c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
))), crs = 3857)

fisheye_poly <- sf_fisheye(
  poly,
  r_in = 0.3, r_out = 0.6,
  zoom_factor = 2.0,
  squeeze_factor = 0.25
)

library(ggplot2)
ggplot() +
  geom_sf(data = poly, fill = NA, color = "grey") +
  geom_sf(data = fisheye_poly, fill = NA, color = "red")
```

---

## 🎯 The Focus–Glue–Context model

The FGC fisheye divides space into three radial zones:

* **Focus zone** (`r ≤ r_in`): Magnified by `zoom_factor`, but clamped to the inner radius
* **Glue zone** (`r_in < r ≤ r_out`): Smooth transition with compression controlled by `squeeze_factor`
* **Context zone** (`r > r_out`): Remains unchanged

| Parameter        | Meaning                                  | Default    | Range    |
| ---------------- | ---------------------------------------- | ---------- | -------- |
| `r_in`           | Focus radius (normalized units)          | 0.34       | > 0      |
| `r_out`          | Glue radius (normalized units)           | 0.50       | > `r_in` |
| `zoom_factor`    | Focus magnification                      | 1.5        | > 1      |
| `squeeze_factor` | Glue compression                         | 0.3        | (0, 1]   |
| `method`         | Glue strategy (`"expand"` / `"outward"`) | `"expand"` | string   |
| `revolution`     | Angular twist (radians)                  | 0.0        | any      |

---

## 🗺️ Flexible centers & CRS handling

`sf_fisheye()` automatically handles coordinate systems:

* Geographic data → projected to suitable UTM/MGA zone (auto-picked from centroid)
* Victoria, AU → **EPSG:7855** (GDA2020 / MGA Zone 55)
* Already projected → left unchanged
* Manual override → `target_crs`

**Center specification options:**

* `center = c(lon, lat)` with `center_crs = "EPSG:4326"`
* `center = c(x, y)` already in map units
* `center = c(nx, ny)` in normalized space (`[-1,1]`), with `normalized_center = TRUE`
* `center = sf_object` — centroid of any geometry is used (polygon, line, point collection, etc.)
* Legacy: `cx, cy` numeric in map units

---

## 🔧 Advanced usage

### Use another `sf` object as center

```r
melb_poly <- suburbs[suburbs$name == "Melbourne", ]
fisheye_vic <- sf_fisheye(vic, center = melb_poly)
```

### Normalized center input

```r
# Center at +0.2, -0.1 relative to bbox, normalized space
fisheye_vic <- sf_fisheye(vic, center = c(0.2, -0.1), normalized_center = TRUE)
```

### Geographic lon/lat input

```r
fisheye_cbd <- sf_fisheye(
  vic,
  center = c(144.9631, -37.8136),  # Melbourne CBD
  center_crs = "EPSG:4326",
  r_in = 0.2, r_out = 0.5
)
```

### Custom coordinate transformation

```r
shift_and_scale <- function(coords, scale = 2, shift_x = 100) {
  cbind(coords[,1] * scale + shift_x, coords[,2] * scale)
}

transformed <- st_transform_custom(
  sf_obj = vic,
  transform_fun = shift_and_scale,
  args = list(scale = 1.5, shift_x = 500)
)
```

### Geographic Data with Auto-Projection

```r
# Automatically projects to appropriate CRS, applies fisheye, returns to WGS84
sf_fisheye(vic, cx = 321300, cy = 5812000, r_in = 0.15, r_out = 0.2, zoom_factor = 3) |> ggplot() + geom_sf()
```

### Zone Classification and Analysis

```r
# Classify points by transformation zone
coords <- matrix(runif(200, -1, 1), ncol = 2)
zones <- classify_zones(coords, r_in = 0.3, r_out = 0.6)

table(zones)
#> zones
#> context   focus    glue 
#>      74      5      21
```



---

## Supported Geometries

The package supports all major sf geometry types:

- ✅ `POINT` / `MULTIPOINT`
- ✅ `LINESTRING` / `MULTILINESTRING`  
- ✅ `POLYGON` / `MULTIPOLYGON`
- ✅ Mixed geometry collections

---

## ⚡ Performance tips

* Use `preserve_aspect = FALSE` if stretching is acceptable (faster)
* Large datasets: consider chunking / spatial indexing
* Stronger `squeeze_factor` makes glue computations heavier

---

## 📚 Citation

```r
citation("mapycusmaximus")
```
## Dependencies

**Required:**
- R (≥ 3.6.0)

**Suggested:**
- sf (≥ 1.0.0) - for spatial data support
- ggplot2 (≥ 3.0.0) - for visualization functions

## Contributing

Contributions are welcome! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request
---

## 📜 License

MIT License – see [LICENSE](LICENSE).

---

## 🙏 Acknowledgments

* The **sf** package maintainers for the spatial infrastructure
* The R spatial community for discussion & feedback
* Research in **focus+context visualization** for conceptual foundations

