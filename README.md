# mapycusmaximus <a href="https://github.com/Alex-Nguyen-VN/mapycusmaximus" class="pkgdown-release">Development Version</a>

[![R-CMD-check](https://github.com/Alex-Nguyen-VN/mapycusmaximus/workflows/R-CMD-check/badge.svg)](https://github.com/Alex-Nguyen-VN/mapycusmaximus/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

---

## Focus–Glue–Context (FGC) Fisheye Transformations for R

`mapycusmaximus` provides geometric transformations for spatial and planar data inspired by focus–context visualization principles.  
The package implements the **Focus–Glue–Context (FGC)** model, which enlarges a region of interest (*focus*), smoothly compresses its surroundings (*glue*), and preserves the outer region (*context*).  
These transformations facilitate detailed local inspection while maintaining overall spatial structure.

---

## Overview

Fisheye transformations are a well-established approach to visualizing dense data without losing context (Furnas 1986; Sarkar & Brown 1992).  
`mapycusmaximus` extends these principles to modern spatial data workflows in R, integrating seamlessly with the [`sf`](https://r-spatial.github.io/sf/) ecosystem and `ggplot2` visualization grammar.

---

## Main Features

- **Focus–Glue–Context transformation:** smooth magnification, transition, and preservation of spatial zones.  
- **Full integration with `sf`:** works directly on geometries and automatically manages CRS transformation.  
- **Flexible centers:** numeric, geographic (`EPSG:4326`), normalized, or centroid-based.  
- **Customizable parameters:** control zoom, squeeze, and optional angular twist (`revolution`).  
- **Visualization utilities:** compare original and transformed coordinates with [`plot_fisheye_fgc()`](reference/plot_fisheye_fgc.html).  
- **Efficient implementation:** vectorized operations and safe polygon re-closure.

---

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Alex-Nguyen-VN/mapycusmaximus")
```

---

## Quick Start

### Basic coordinate transformation

```r
library(mapycusmaximus)

grid <- create_test_grid(range = c(-1, 1), spacing = 0.1)

transformed <- fisheye_fgc(
  coords = grid,
  r_in = 0.34, r_out = 0.5,
  zoom_factor = 1.5,
  squeeze_factor = 0.3
)

plot_fisheye_fgc(grid, transformed, r_in = 0.34, r_out = 0.5)
```

### Spatial data integration

```r
library(sf)
library(ggplot2)

poly <- st_sfc(st_polygon(list(rbind(
  c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
))), crs = 3857)

fisheye_poly <- sf_fisheye(
  poly,
  r_in = 0.3, r_out = 0.6,
  zoom_factor = 2.0,
  squeeze_factor = 0.25
)

ggplot() +
  geom_sf(data = poly, fill = NA, colour = "grey50") +
  geom_sf(data = fisheye_poly, fill = NA, colour = "red")
```

---

## The Focus–Glue–Context Model

The transformation divides space into three radial zones:

| Zone | Definition | Effect |
|------|-------------|--------|
| **Focus** | `r ≤ r_in` | Enlarged by `zoom_factor` |
| **Glue** | `r_in < r ≤ r_out` | Smooth compression controlled by `squeeze_factor` |
| **Context** | `r > r_out` | Retains original geometry |

| Parameter | Description | Default | Range |
|------------|-------------|----------|-------|
| `r_in` | Focus radius | 0.34 | > 0 |
| `r_out` | Outer radius | 0.50 | > `r_in` |
| `zoom_factor` | Magnification factor | 1.5 | > 1 |
| `squeeze_factor` | Glue compression | 0.3 | (0, 1] |
| `method` | Strategy (`"expand"` / `"outward"`) | `"expand"` | string |
| `revolution` | Angular twist (radians) | 0 | any |

---

## Coordinate System Handling

[`sf_fisheye()`](reference/sf_fisheye.html) automatically manages coordinate reference systems:

- **Geographic data:** projected to a suitable UTM/MGA zone (e.g. EPSG:7855 for Victoria, AU).  
- **Projected data:** used directly.  
- **Manual override:** specify `target_crs`.

### Center specification

- Numeric: `center = c(x, y)` (map units)  
- Geographic: `center_crs = "EPSG:4326"`  
- Normalized: `center = c(nx, ny)`, `normalized_center = TRUE`  
- Geometry input: `center = sf_object` (centroid is used)

---

## Advanced Usage

### Using an `sf` object as the center

```r
melb_poly <- suburbs[suburbs$name == "Melbourne", ]
fisheye_vic <- sf_fisheye(vic, center = melb_poly)
```

### Normalized center input

```r
fisheye_vic <- sf_fisheye(vic, center = c(0.2, -0.1), normalized_center = TRUE)
```

### Geographic center

```r
fisheye_cbd <- sf_fisheye(
  vic,
  center = c(144.9631, -37.8136),
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

---

## Supported Geometries

`mapycusmaximus` supports the following `sf` geometry types:

- `POINT`, `MULTIPOINT`  
- `LINESTRING`, `MULTILINESTRING`  
- `POLYGON`, `MULTIPOLYGON`  
- Geometry collections

---

## Performance Notes

- Set `preserve_aspect = FALSE` to allow stretching and improve performance.  
- For large datasets, apply the transformation in spatial chunks.  
- Stronger compression (`squeeze_factor` → small) increases computational cost.

---

## References

- Furnas, G. W. (1986). *Generalized Fisheye Views*. *Proceedings CHI’86*, 16–23.  
- Sarkar, M., & Brown, M. H. (1992). *Graphical Fisheye Views of Graphs*. *Proceedings CHI’92*, 83–91.  
- Laa, U., Cook, D., & Lee, S. (2020). *Burning Sage: Reversing the Curse of Dimensionality in Visualization*. *arXiv:2009.10979*.

---

## Package Dependencies

**Imports**

- `sf` (≥ 1.0.0)  
- `ggplot2` (≥ 3.0.0)  
- `dplyr`

**Suggests**

- `rmapshaper`, `tmap`, `testthat`, `knitr`, `rmarkdown`

---

## Citation

```r
citation("mapycusmaximus")
```

---

## Contributing

Contributions are encouraged. To contribute:

1. Fork the repository.  
2. Create a feature branch:  
   ```bash
   git checkout -b feature/your-feature
   ```
3. Commit and push changes.  
4. Submit a pull request with a concise summary.

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details.

---

## License

Licensed under the **MIT License**.  
See [LICENSE](LICENSE) for the complete text.

---

## Acknowledgements

We acknowledge the developers of **sf**, **ggplot2**, and the R spatial community for providing the computational and theoretical foundation enabling this work.  
Conceptual inspiration draws from research on focus–context visualization and the *burning-sage* transformation for reversin