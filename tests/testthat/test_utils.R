library(testthat)
library(sf)
library(ggplot2)

# Test utility functions
test_that("create_test_grid generates correct grid", {
  grid <- create_test_grid(range = c(-1, 1), spacing = 0.5)
  
  expect_true(is.matrix(grid))
  expect_equal(ncol(grid), 2)
  expect_true(all(colnames(grid) %in% c("x", "y")))
  
  # Check that grid covers the expected range
  expect_true(min(grid[, "x"]) >= -1)
  expect_true(max(grid[, "x"]) <= 1)
  expect_true(min(grid[, "y"]) >= -1)
  expect_true(max(grid[, "y"]) <= 1)
  
  # With spacing 0.5, we should have points at -1, -0.5, 0, 0.5, 1
  unique_x <- sort(unique(grid[, "x"]))
  expect_equal(unique_x, c(-1, -0.5, 0, 0.5, 1))
})

test_that("classify_zones correctly classifies points", {
  # Test points at known distances
  coords <- matrix(c(
    0, 0,        # distance = 0 (focus)
    0.2, 0,      # distance = 0.2 (focus if r_in = 0.3)
    0.4, 0,      # distance = 0.4 (glue if r_in = 0.3, r_out = 0.5)
    0.6, 0       # distance = 0.6 (context if r_out = 0.5)
  ), ncol = 2, byrow = TRUE)
  
  zones <- classify_zones(coords, cx = 0, cy = 0, r_in = 0.3, r_out = 0.5)
  
  expect_equal(zones[1], "focus")
  expect_equal(zones[2], "focus") 
  expect_equal(zones[3], "glue")
  expect_equal(zones[4], "context")
  
  expect_equal(length(zones), nrow(coords))
  expect_true(all(zones %in% c("focus", "glue", "context")))
})

test_that("classify_zones handles different centers", {
  coords <- matrix(c(1, 1), ncol = 2)  # Point at (1, 1)
  
  # Center at origin: distance = sqrt(2) ≈ 1.41
  zones1 <- classify_zones(coords, cx = 0, cy = 0, r_in = 1, r_out = 2)
  expect_equal(zones1, "glue")
  
  # Center at the point itself: distance = 0
  zones2 <- classify_zones(coords, cx = 1, cy = 1, r_in = 1, r_out = 2)
  expect_equal(zones2, "focus")
})

test_that("plot_fisheye_fgc creates ggplot object", {
  skip_if_not_installed("ggplot2")
  
  # Create simple test data
  original <- matrix(c(0, 0, 0.5, 0.5, 1, 1), ncol = 2, byrow = TRUE)
  transformed <- original * 1.2  # Simple scaling
  
  p <- plot_fisheye_fgc(original, transformed, r_in = 0.3, r_out = 0.7)
  
  expect_s3_class(p, "ggplot")
  
  # Check that the plot has the expected elements
  expect_true("GeomPoint" %in% class(p$layers[[1]]$geom))
})
