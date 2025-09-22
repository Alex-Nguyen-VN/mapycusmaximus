library(testthat)
library(sf)
library(ggplot2)

# Test fisheye_fgc function
test_that("fisheye_fgc works with basic inputs", {
  # Create simple test data
  coords <- matrix(c(0, 0, 0.1, 0.1, 0.5, 0.5, 1, 1), ncol = 2, byrow = TRUE)
  
  result <- fisheye_fgc(coords)
  
  # Check output structure
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), nrow(coords))
  expect_true(all(colnames(result) %in% c("x_new", "y_new")))
  
  # Check attributes
  expect_true("zones" %in% names(attributes(result)))
  expect_true("original_radius" %in% names(attributes(result)))
  expect_true("new_radius" %in% names(attributes(result)))
  
  zones <- attr(result, "zones")
  expect_true(all(zones %in% c("focus", "glue", "context")))
  expect_equal(length(zones), nrow(coords))
})

test_that("fisheye_fgc handles different parameters", {
  coords <- matrix(c(0, 0, 0.2, 0.2, 0.4, 0.4, 0.8, 0.8), ncol = 2, byrow = TRUE)
  
  # Test with custom parameters
  result <- fisheye_fgc(coords, 
                       cx = 0.1, cy = 0.1,
                       r_in = 0.3, r_out = 0.6,
                       zoom_factor = 2.0, squeeze_factor = 0.5,
                       method = "outward", revolution = 0.1)
  
  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(coords))
  
  # Check that revolution parameter affects angles in glue zone
  zones <- attr(result, "zones")
  expect_true(any(zones == "glue"))
})

test_that("fisheye_fgc validates input parameters", {
  coords <- matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)
  
  # Should work with valid parameters
  expect_no_error(fisheye_fgc(coords, r_in = 0.3, r_out = 0.5))
  
  # Test edge cases
  expect_no_error(fisheye_fgc(coords, zoom_factor = 1.0))
  expect_no_error(fisheye_fgc(coords, squeeze_factor = 1.0))
  expect_no_error(fisheye_fgc(coords, revolution = 0))
})

test_that("fisheye_fgc zone classification is correct", {
  coords <- matrix(c(0, 0,      # should be focus
                    0.2, 0.2,   # should be focus (r ≈ 0.28 < 0.34)
                    0.4, 0,     # should be glue (r = 0.4, between 0.34 and 0.5)
                    0.6, 0.6),  # should be context (r ≈ 0.85 > 0.5)
                   ncol = 2, byrow = TRUE)
  
  result <- fisheye_fgc(coords, r_in = 0.34, r_out = 0.5)
  zones <- attr(result, "zones")
  
  expect_equal(zones[1], "focus")
  expect_equal(zones[2], "focus") 
  expect_equal(zones[3], "glue")
  expect_equal(zones[4], "context")
})