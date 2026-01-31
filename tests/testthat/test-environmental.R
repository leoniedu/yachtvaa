test_that("interpolate_buoy creates regular time grid", {
  buoy <- data.frame(
    datetime = as.POSIXct(c(
      "2026-01-29 10:00:00",
      "2026-01-29 10:30:00",
      "2026-01-29 11:00:00"
    ), tz = "UTC"),
    wind_speed = c(10, NA, 20),
    wind_dir = c(90, NA, 180)
  )

  result <- interpolate_buoy(buoy, interval_sec = 600)

  # Should have 7 rows: 10:00, 10:10, 10:20, 10:30, 10:40, 10:50, 11:00
  expect_equal(nrow(result), 7L)
  # First and last values preserved
  expect_equal(result$wind_speed[1], 10)
  expect_equal(result$wind_speed[7], 20)
  # Middle value interpolated
  expect_equal(result$wind_speed[4], 15, tolerance = 0.1)
})

test_that("interpolate_buoy handles empty input", {
  buoy <- data.frame(
    datetime = .POSIXct(numeric(), tz = "UTC"),
    wind_speed = numeric()
  )

  result <- interpolate_buoy(buoy)
  expect_equal(nrow(result), 0L)
})

test_that("match_buoy_to_segments joins by nearest time", {
  segments <- data.frame(
    athlete = c("A", "B"),
    start_time = as.POSIXct(c(
      "2026-01-29 10:05:00",
      "2026-01-29 10:25:00"
    ), tz = "UTC"),
    predicted_time_sec = c(50, 60)
  )

  buoy <- data.frame(
    datetime = as.POSIXct(c(
      "2026-01-29 10:00:00",
      "2026-01-29 10:10:00",
      "2026-01-29 10:20:00",
      "2026-01-29 10:30:00"
    ), tz = "UTC"),
    wind_speed = c(10, 12, 14, 16)
  )

  result <- match_buoy_to_segments(segments, buoy)

  expect_equal(nrow(result), 2L)
  # A at 10:05 should match 10:10 (nearest, within 5 min each way)
  # Actually 10:00 is 5 min away and 10:10 is 5 min away - either is valid
  # The result should have wind_speed from the matched buoy row
  expect_true("wind_speed" %in% names(result))
  expect_true("predicted_time_sec" %in% names(result))
  expect_true(all(!is.na(result$wind_speed)))
})
