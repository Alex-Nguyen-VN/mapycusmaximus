library(testthat)
library(sf)
library(ggplot2)

# Test fisheye_fgc function
test_that("fisheye_fgc works with sf objects", {
  skip_if_not_installed("sf")
  
  # Create a simple polygon
  poly <- st_sfc(st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  ))), crs = 3857)
  
  result <- fisheye_fgc(poly, r_in = 0.3, r_out = 0.6)
  
  expect_s3_class(result, "sfc")
  expect_equal(st_crs(result), st_crs(poly))
  expect_equal(length(result), length(poly))
})

test_that("fisheye_fgc.sf handles lonlat data", {
  skip_if_not_installed("sf")
  
  # Create lonlat point (Melbourne CBD)
  pt_ll <- st_sfc(st_point(c(144.9631, -37.8136)), crs = 4326)
  
  result <- fisheye_fgc(pt_ll, r_in = 0.2, r_out = 0.5)
  
  expect_s3_class(result, "sfc")
  expect_equal(st_crs(result), st_crs(pt_ll))  # Should return to original CRS
})

test_that("fisheye_fgc.sf handles different geometry types", {
  skip_if_not_installed("sf")
  
  # Point
  pt <- st_sfc(st_point(c(0, 0)), crs = 3857)
  result_pt <- fisheye_fgc(pt)
  expect_s3_class(result_pt, "sfc")
  
  # LineString
  ln <- st_sfc(st_linestring(rbind(c(0, 0), c(1, 0), c(1, 1))), crs = 3857)
  result_ln <- fisheye_fgc(ln)
  expect_s3_class(result_ln, "sfc")
  
  # Polygon
  poly <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 3857)
  result_poly <- fisheye_fgc(poly)
  expect_s3_class(result_poly, "sfc")
})

test_that("fisheye_fgc.sf parameter validation", {
  skip_if_not_installed("sf")
  
  poly <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 3857)
  
  # r_out must be greater than r_in
  expect_error(fisheye_fgc(poly, r_in = 0.5, r_out = 0.3))
  
  # Should work with valid parameters
  expect_no_error(fisheye_fgc(poly, r_in = 0.3, r_out = 0.5))
})
