test_that("relative_wind returns correct tibble", {
  result <- relative_wind(0, 0)
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("wind_angle_deg", "wind_class"))
  expect_equal(result$wind_class, "headwind")
})

test_that("relative_wind returns calm when wind_speed is zero", {
  result <- relative_wind(0, 0, wind_speed = 0)
  expect_equal(result$wind_class, "calm")

  result <- relative_wind(45, 200, wind_speed = 0)
  expect_equal(result$wind_class, "calm")
})

test_that("relative_current returns correct tibble", {
  result <- relative_current(0, 0)
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("current_angle_deg", "current_class"))
  expect_equal(result$current_class, "following")
})

test_that("apparent_conditions computes wind component correctly", {
  segments <- data.frame(
    bearing_deg = c(0, 0, 0),
    wind_direction_deg = c(0, 180, 90),
    wind_speed_kmh = c(10, 10, 10),
    current_direction_deg = c(0, 0, 0),
    current_speed_kmh = c(0, 0, 0)
  )

  result <- apparent_conditions(segments)

  # Headwind (FROM same direction): component should be negative (opposing)
  expect_true(result$wind_component_kmh[1] < 0)
  # Tailwind (FROM opposite): component should be positive (assisting)
  expect_true(result$wind_component_kmh[2] > 0)
  # Pure crosswind: component should be ~0
  expect_equal(result$wind_component_kmh[3], 0, tolerance = 0.01)
})

test_that("apparent_conditions computes current component correctly", {
  segments <- data.frame(
    bearing_deg = c(0, 0, 0),
    wind_direction_deg = c(0, 0, 0),
    wind_speed_kmh = c(0, 0, 0),
    current_direction_deg = c(0, 180, 90),
    current_speed_kmh = c(2, 2, 2)
  )

  result <- apparent_conditions(segments)

  # Following current (TO same direction): positive (assisting)
  expect_true(result$current_component_kmh[1] > 0)
  expect_equal(result$current_component_kmh[1], 2, tolerance = 0.01)
  # Opposing current (TO opposite): negative (opposing)
  expect_true(result$current_component_kmh[2] < 0)
  expect_equal(result$current_component_kmh[2], -2, tolerance = 0.01)
  # Cross current: ~0 along-track component
  expect_equal(result$current_component_kmh[3], 0, tolerance = 0.01)
})

test_that("apparent_conditions adds all expected columns", {
  segments <- data.frame(
    athlete = "A",
    bearing_deg = 45,
    wind_direction_deg = 90,
    wind_speed_kmh = 15,
    current_direction_deg = 180,
    current_speed_kmh = 1.5
  )

  result <- apparent_conditions(segments)

  expected_new <- c(
    "wind_angle_deg", "wind_class", "wind_component_kmh",
    "current_angle_deg", "current_class", "current_component_kmh"
  )
  expect_true(all(expected_new %in% names(result)))
  # Original columns preserved
  expect_true("athlete" %in% names(result))
})

test_that("apparent_conditions errors on missing wind by default", {
  segments <- data.frame(
    bearing_deg = 0,
    current_direction_deg = 0,
    current_speed_kmh = 1
  )

  expect_error(
    apparent_conditions(segments),
    "impute_missing_wind"
  )
})

test_that("apparent_conditions imputes missing wind columns as zero", {
  segments <- data.frame(
    bearing_deg = c(0, 0),
    current_direction_deg = c(0, 180),
    current_speed_kmh = c(1, 1)
  )

  expect_warning(
    result <- apparent_conditions(segments, impute_missing_wind = TRUE),
    "imputing as zero"
  )

  expect_equal(result$wind_speed_kmh, c(0, 0))
  expect_equal(result$wind_direction_deg, c(0, 0))
  expect_equal(result$wind_component_kmh, c(0, 0))
  expect_equal(result$wind_class, c("calm", "calm"))
  # Current should still be computed normally
  expect_true(result$current_component_kmh[1] > 0)   # following
  expect_true(result$current_component_kmh[2] < 0)   # opposing
})

test_that("apparent_conditions imputes all-NA wind as zero", {
  segments <- data.frame(
    bearing_deg = 0,
    wind_direction_deg = NA_real_,
    wind_speed_kmh = NA_real_,
    current_direction_deg = 0,
    current_speed_kmh = 1
  )

  expect_warning(
    result <- apparent_conditions(segments, impute_missing_wind = TRUE),
    "imputing as zero"
  )

  expect_equal(result$wind_speed_kmh, 0)
  expect_equal(result$wind_component_kmh, 0)
  expect_equal(result$wind_class, "calm")
})

test_that("apparent_conditions does not warn when wind data is present", {
  segments <- data.frame(
    bearing_deg = 0,
    wind_direction_deg = 180,
    wind_speed_kmh = 10,
    current_direction_deg = 0,
    current_speed_kmh = 1
  )

  expect_no_warning(
    apparent_conditions(segments, impute_missing_wind = TRUE)
  )
})
