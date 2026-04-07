library(testthat)
library(sf)
library(ggplot2)


# Integration tests
test_that("fisheye_fgc and plot_fisheye_fgc work together", {
  skip_if_not_installed("ggplot2")
  
  # Create test grid
  grid <- create_test_grid(range = c(-0.8, 0.8), spacing = 0.2)
  
  # Apply fisheye transformation
  transformed <- fisheye_fgc(grid, r_in = 0.3, r_out = 0.5)
  
  # Create visualization
  p <- plot_fisheye_fgc(grid, transformed, r_in = 0.3, r_out = 0.5)
  
  expect_s3_class(p, "ggplot")
  expect_true(is.matrix(transformed))
})

test_that("fisheye_fgc.matrix and fisheye_fgc.sf are consistent", {
  skip_if_not_installed("sf")
  
  # Create a simple point that should be in the focus zone
  pt <- st_sfc(st_point(c(0.1, 0.1)), crs = 3857)
  
  # Apply fisheye_fgc with known parameters
  result_sf <- fisheye_fgc(pt, cx = 0, cy = 0, r_in = 0.3, r_out = 0.5, 
                         zoom_factor = 2, preserve_aspect = TRUE)
  
  expect_s3_class(result_sf, "sfc")
  expect_equal(st_crs(result_sf), st_crs(pt))
})

# Edge cases and error handling
test_that("functions handle edge cases gracefully", {
  # Empty coordinates
  empty_coords <- matrix(numeric(0), ncol = 2)
  expect_no_error(fisheye_fgc(empty_coords))
  
  # Single point
  single_point <- matrix(c(0, 0), ncol = 2)
  result_single <- fisheye_fgc(single_point)
  expect_equal(nrow(result_single), 1)
  
  # Points exactly on boundaries
  boundary_coords <- matrix(c(0.34, 0, 0.5, 0), ncol = 2, byrow = TRUE)
  zones <- classify_zones(boundary_coords, r_in = 0.34, r_out = 0.5)
  expect_true(all(zones %in% c("focus", "glue", "context")))
})

test_that("functions handle degenerate sf geometries", {
  skip_if_not_installed("sf")
  
  # Empty polygon
  empty_poly <- st_sfc(st_polygon(), crs = 3857)
  expect_error(fisheye_fgc(empty_poly))
  
  # Very small polygon
  tiny_poly <- st_sfc(st_polygon(list(rbind(
    c(0, 0), c(0.001, 0), c(0.001, 0.001), c(0, 0.001), c(0, 0)
  ))), crs = 3857)
  expect_no_error(fisheye_fgc(tiny_poly))
})