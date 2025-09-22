library(testthat)
library(sf)
library(ggplot2)

# Test st_transform_custom function
test_that("st_transform_custom works with different geometry types", {
  skip_if_not_installed("sf")
  
  # Simple scaling function
  scale_fun <- function(coords, factor = 2) {
    cbind(coords[,1] * factor, coords[,2] * factor)
  }
  
  # Test with point
  pt <- st_sfc(st_point(c(1, 1)), crs = 3857)
  result_pt <- st_transform_custom(pt, scale_fun, list(factor = 2))
  coords_pt <- st_coordinates(result_pt)
  expect_equal(as.numeric(coords_pt[1, 1]), 2)
  expect_equal(as.numeric(coords_pt[1, 2]), 2)
  
  # Test with linestring
  ln <- st_sfc(st_linestring(rbind(c(0, 0), c(1, 1))), crs = 3857)
  result_ln <- st_transform_custom(ln, scale_fun, list(factor = 3))
  coords_ln <- st_coordinates(result_ln)
  expect_equal(as.numeric(coords_ln[2, 1]), 3)
  expect_equal(as.numeric(coords_ln[2, 2]), 3)
  
  # Test with polygon
  poly <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 3857)
  result_poly <- st_transform_custom(poly, scale_fun, list(factor = 2))
  coords_poly <- st_coordinates(result_poly)
  expect_equal(as.numeric(max(coords_poly[, "X"])), 2)
  expect_equal(as.numeric(max(coords_poly[, "Y"])), 2)
})

test_that("st_transform_custom preserves polygon closure", {
  skip_if_not_installed("sf")
  
  # Identity transformation
  identity_fun <- function(coords) coords
  
  poly <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 3857)
  result <- st_transform_custom(poly, identity_fun, list())
  
  coords <- st_coordinates(result)
  # First and last coordinates should be identical (polygon closure)
  expect_equal(coords[1, "X"], coords[nrow(coords), "X"])
  expect_equal(coords[1, "Y"], coords[nrow(coords), "Y"])
})

test_that("st_transform_custom handles sf data frames", {
  skip_if_not_installed("sf")
  
  shift_fun <- function(coords, dx = 0, dy = 0) {
    cbind(coords[,1] + dx, coords[,2] + dy)
  }
  
  # Create sf data frame
  sf_df <- st_sf(
    id = 1:2,
    geometry = st_sfc(
      st_point(c(0, 0)),
      st_point(c(1, 1))
    ),
    crs = 3857
  )
  
  result <- st_transform_custom(sf_df, shift_fun, list(dx = 10, dy = 20))
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(1, 2))
  
  coords <- st_coordinates(result)
  expect_equal(as.numeric(coords[1, "X"]), 10)
  expect_equal(as.numeric(coords[1, "Y"]), 20)
  expect_equal(as.numeric(coords[2, "X"]), 11)
  expect_equal(as.numeric(coords[2, "Y"]), 21)
})
