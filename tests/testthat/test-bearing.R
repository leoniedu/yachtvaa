test_that("compute_bearing returns correct cardinal directions", {
  # North: dy positive, dx zero

  expect_equal(compute_bearing(0, 0, 0, 100), 0)
  # East: dx positive, dy zero
  expect_equal(compute_bearing(0, 0, 100, 0), 90)
  # South: dy negative, dx zero
  expect_equal(compute_bearing(0, 0, 0, -100), 180)
  # West: dx negative, dy zero
  expect_equal(compute_bearing(0, 0, -100, 0), 270)
})

test_that("compute_bearing handles intercardinal directions", {
  # NE
  expect_equal(compute_bearing(0, 0, 100, 100), 45)
  # SE
  expect_equal(compute_bearing(0, 0, 100, -100), 135)
  # SW
  expect_equal(compute_bearing(0, 0, -100, -100), 225)
  # NW
  expect_equal(compute_bearing(0, 0, -100, 100), 315)
})

test_that("compute_bearing is vectorized", {
  bearings <- compute_bearing(
    c(0, 0, 0),
    c(0, 0, 0),
    c(0, 100, -100),
    c(100, 0, 0)
  )
  expect_equal(bearings, c(0, 90, 270))
})

test_that("angle_between returns signed angle in (-180, 180]", {
  expect_equal(angle_between(0, 90), 90)
  expect_equal(angle_between(90, 0), -90)
  expect_equal(angle_between(350, 10), 20)
  expect_equal(angle_between(10, 350), -20)
  expect_equal(angle_between(0, 180), 180)
  expect_equal(angle_between(180, 0), 180)
})

test_that("classify_wind_angle identifies headwind correctly", {
  # Wind FROM north, vessel heading north = headwind

  result <- classify_wind_angle(0, 0)
  expect_equal(result$class, "headwind")
  expect_equal(result$angle_deg, 0)

  # Wind FROM 30 degrees, vessel heading 10 = headwind (within 45)
  result <- classify_wind_angle(10, 30)
  expect_equal(result$class, "headwind")
})

test_that("classify_wind_angle identifies tailwind correctly", {
  # Wind FROM south (180), vessel heading north (0) = tailwind
  result <- classify_wind_angle(0, 180)
  expect_equal(result$class, "tailwind")
  expect_equal(result$angle_deg, 180)
})

test_that("classify_wind_angle identifies crosswind correctly", {
  # Wind FROM east (90), vessel heading north (0) = crosswind right
  result <- classify_wind_angle(0, 90)
  expect_equal(result$class, "crosswind_right")

  # Wind FROM west (270), vessel heading north (0) = crosswind left
  result <- classify_wind_angle(0, 270)
  expect_equal(result$class, "crosswind_left")
})

test_that("classify_wind_angle is vectorized", {
  result <- classify_wind_angle(c(0, 0, 0), c(0, 180, 90))
  expect_equal(result$class, c("headwind", "tailwind", "crosswind_right"))
})

test_that("classify_current_angle identifies following current", {
  # Current TO north, vessel heading north = following
  result <- classify_current_angle(0, 0)
  expect_equal(result$class, "following")
  expect_equal(result$angle_deg, 0)
})

test_that("classify_current_angle identifies opposing current", {
  # Current TO south (180), vessel heading north (0) = opposing
  result <- classify_current_angle(0, 180)
  expect_equal(result$class, "opposing")
  expect_equal(result$angle_deg, 180)
})

test_that("classify_current_angle identifies cross current", {
  # Current TO east (90), vessel heading north (0) = cross right
  result <- classify_current_angle(0, 90)
  expect_equal(result$class, "cross_right")

  # Current TO west (270), vessel heading north (0) = cross left
  result <- classify_current_angle(0, 270)
  expect_equal(result$class, "cross_left")
})
