context("Converting East Africa Grid Coordinates")

test_that("Converting East Africa Grid Coordinates", {
  # (0.8003110797877706, 34.57585381772091)
  expect_equal(
    EAGS2LatLong(band = "G", block = "A", subblock = "F", easting = 310, northing = 885)[["latitude"]],
    0.8003111, tolerance = .002
  )
  expect_equal(
    EAGS2LatLong(band = "G", block = "A", subblock = "F", easting = 310, northing = 885)[["longitude"]],
    34.57584, tolerance = .002
  )

  # Pass it a Z block to see if lat is negative
  # Now it's not working with A blocks
  EAGS2LatLong(band = "G", block = "A", subblock = "F", easting = 310, northing = 885) # close by not exact
  EAGS2LatLong(band = "G", block = "Z", subblock = "F", easting = 310, northing = 885) # close by not exact
})
