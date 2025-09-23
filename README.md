# fisheye

<!-- badges: start -->
[![R-CMD-check](https://github.com/Alex-Nguyen-VN/mapycusmaximus/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/fisheye/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> **Focus–Glue–Context (FGC) Fisheye Transformations for R**

The `fisheye` package provides tools for applying fisheye transformations to 2D coordinate data and spatial vector geometries. It implements the **Focus–Glue–Context (FGC)** model, which creates smooth, radial distortions that magnify areas of interest while preserving context.

## Features

- 🎯 **Focus–Glue–Context transformation**: Magnify focus regions, smoothly transition through glue zones, preserve context areas
- 🗺️ **sf integration**: Native support for spatial vector data with automatic CRS handling
- 🔄 **Flexible parameters**: Control zoom intensity, compression factors, and optional rotation effects
- 📊 **Visualization tools**: Built-in plotting functions for comparing original and transformed coordinates
- ⚡ **Performance**: Vectorized operations for efficient processing of large datasets

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("yourusername/fisheye")
```

## Quick Start

### Basic Coordinate Transformation

```r
library(fisheye)

# Create a test grid
grid <- create_test_grid(range = c(-1, 1), spacing = 0.1)

# Apply fisheye transformation
transformed <- fisheye_fgc(
  coords = grid,
  r_in = 0.34,        # Focus radius
  r_out = 0.5,        # Context boundary
  zoom_factor = 1.5,  # Magnification intensity
  squeeze_factor = 0.3 # Compression in glue zone
)

# Visualize the transformation
plot_fisheye_fgc(grid, transformed, r_in = 0.34, r_out = 0.5)
```

### Spatial Data (sf) Integration

```r
library(sf)
library(fisheye)

# Load your spatial data
# poly <- st_read("your_shapefile.shp")

# Or create a simple example
poly <- st_sfc(st_polygon(list(rbind(
  c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
))), crs = 3857)

# Apply fisheye transformation
# Automatically handles CRS projection and normalization
fisheye_poly <- sf_fisheye(
  poly, 
  r_in = 0.3, 
  r_out = 0.6,
  zoom_factor = 2.0,
  squeeze_factor = 0.25
) 

# Plot comparison
library(ggplot2)
original_plot <- ggplot() + geom_sf(data = poly) + ggtitle("Original")
fisheye_plot <- ggplot() + geom_sf(data = fisheye_poly) + ggtitle("Fisheye")

gridExtra::grid.arrange(original_plot, fisheye_plot, ncol = 2)
```

## The Focus–Glue–Context Model

The FGC fisheye transformation divides space into three concentric zones:

- **🎯 Focus Zone** (`r ≤ r_in`): Points are magnified using `zoom_factor`, but expansion is constrained to the zone boundary
- **🔗 Glue Zone** (`r_in < r ≤ r_out`): Smooth transition region with controllable compression via `squeeze_factor`
- **🌍 Context Zone** (`r > r_out`): Points remain unchanged, preserving spatial context

### Key Parameters

| Parameter | Description | Default | Range |
|-----------|-------------|---------|-------|
| `r_in` | Focus zone radius | 0.34 | > 0 |
| `r_out` | Context boundary radius | 0.5 | > `r_in` |
| `zoom_factor` | Focus magnification intensity | 1.5 | > 1.0 |
| `squeeze_factor` | Glue zone compression | 0.3 | (0, 1] |
| `method` | Glue zone compression method | outward - toward outher boundary | expand - toward both boundary |
| `revolution` | Optional rotation in glue zone | 0.0 | any |

## Advanced Usage

### Custom Transformations

```r
# Apply custom coordinate transformation to sf objects
shift_and_scale <- function(coords, scale = 2, shift_x = 100) {
  cbind(coords[,1] * scale + shift_x, coords[,2] * scale)
}

transformed_sf <- st_transform_custom(
  sf_obj = your_sf_object,
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

## Supported Geometries

The package supports all major sf geometry types:

- ✅ `POINT` / `MULTIPOINT`
- ✅ `LINESTRING` / `MULTILINESTRING`  
- ✅ `POLYGON` / `MULTIPOLYGON`
- ✅ Mixed geometry collections

## CRS Handling

`sf_fisheye()` provides intelligent CRS handling:

- **Geographic data (WGS84)**: Automatically projects to appropriate UTM/MGA zones
- **Victoria, Australia**: Uses GDA2020 MGA Zone 55 (EPSG:7855)
- **Other regions**: Selects UTM zones based on centroid location
- **Projected data**: Uses existing projection as working CRS
- **Custom CRS**: Override with `target_crs` parameter

## Performance Tips

- Use `preserve_aspect = FALSE` for faster processing when shape distortion is acceptable
- For large datasets, consider spatial indexing or chunking
- The `squeeze_factor` parameter has the most impact on computation time in the glue zone

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

## Related Work

- **Cartographic fisheye**: Inspired by geographic focus+context visualization techniques
- **Information visualization**: Related to focus+context approaches in data visualization
- **Spatial data science**: Complements tools like `sf`, `terra`, and `stars`

## Citation

```r
citation("fisheye")
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The sf package maintainers for excellent spatial data infrastructure
- The R spatial community for inspiration and feedback
- Focus+context visualization research community

---

**Questions?** Open an [issue](https://github.com/yourusername/fisheye/issues) or start a [discussion](https://github.com/yourusername/fisheye/discussions).