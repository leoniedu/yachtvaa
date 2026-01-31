test_that("fastest_straight_distance finds known segment", {
  # Create a simple track: 10 points heading east, 100m apart, 10s apart
  n <- 10
  coords <- data.frame(
    x = seq(0, 900, by = 100),
    y = rep(0, n)
  )
  pts <- sf::st_as_sf(coords, coords = c("x", "y"), crs = 31984)
  pts$athlete <- "A"
  pts$timestamp <- as.POSIXct("2026-01-29 10:00:00", tz = "UTC") +
    seq(0, by = 10, length.out = n)

  result <- fastest_straight_distance(
    pts,
    athlete_col = "athlete",
    time_col = "timestamp",
    distance_m = 500
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1L)
  expect_equal(result$distance_m_target, 500)
  # Speed is 10 m/s, so 500m should take ~50s

  expect_equal(result$predicted_time_sec, 50, tolerance = 0.1)
  expect_equal(result$avg_speed_kmh, 36, tolerance = 0.1)
  # Bearing should be ~90 (east)
  expect_equal(result$bearing_deg, 90, tolerance = 0.1)
})

test_that("fastest_straight_distance supports multiple distances", {
  n <- 20
  coords <- data.frame(
    x = seq(0, 1900, by = 100),
    y = rep(0, n)
  )
  pts <- sf::st_as_sf(coords, coords = c("x", "y"), crs = 31984)
  pts$athlete <- "A"
  pts$timestamp <- as.POSIXct("2026-01-29 10:00:00", tz = "UTC") +
    seq(0, by = 10, length.out = n)

  result <- fastest_straight_distance(
    pts,
    athlete_col = "athlete",
    time_col = "timestamp",
    distance_m = c(100, 500, 1000)
  )

  expect_equal(nrow(result), 3L)
  expect_equal(sort(result$distance_m_target), c(100, 500, 1000))
})

test_that("fastest_straight_distance returns empty data.table on no results", {
  # Two points, only 10m apart -- not enough for 500m
  coords <- data.frame(x = c(0, 10), y = c(0, 0))
  pts <- sf::st_as_sf(coords, coords = c("x", "y"), crs = 31984)
  pts$athlete <- "A"
  pts$timestamp <- as.POSIXct("2026-01-29 10:00:00", tz = "UTC") + c(0, 10)

  result <- fastest_straight_distance(
    pts,
    athlete_col = "athlete",
    time_col = "timestamp",
    distance_m = 500
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_true("athlete" %in% names(result))
})

test_that("fastest_straight_distance splits tracks by max_gap", {
  # Two track segments with a 30s gap (> default 20s)
  n <- 10
  t1 <- as.POSIXct("2026-01-29 10:00:00", tz = "UTC") + seq(0, by = 5, length.out = n)
  t2 <- t1[n] + 30 + seq(0, by = 5, length.out = n)  # 30s gap

  coords <- data.frame(
    x = c(seq(0, 450, by = 50), seq(0, 450, by = 50)),
    y = rep(0, 2 * n)
  )
  pts <- sf::st_as_sf(coords, coords = c("x", "y"), crs = 31984)
  pts$athlete <- "A"
  pts$timestamp <- c(t1, t2)

  result <- fastest_straight_distance(
    pts,
    athlete_col = "athlete",
    time_col = "timestamp",
    distance_m = 200,
    max_gap = 20
  )

  # Should find results -- both segments span 450m which covers 200m
  expect_true(nrow(result) >= 1L)
})

test_that("fastest_straight_distance handles multiple athletes", {
  n <- 10
  base_time <- as.POSIXct("2026-01-29 10:00:00", tz = "UTC")

  # Athlete A: 10 m/s east
  coords_a <- data.frame(x = seq(0, 900, by = 100), y = rep(0, n))
  # Athlete B: 5 m/s east (slower)
  coords_b <- data.frame(x = seq(0, 450, by = 50), y = rep(0, n))

  coords <- rbind(coords_a, coords_b)
  pts <- sf::st_as_sf(coords, coords = c("x", "y"), crs = 31984)
  pts$athlete <- rep(c("A", "B"), each = n)
  pts$timestamp <- base_time + rep(seq(0, by = 10, length.out = n), 2)

  result <- fastest_straight_distance(
    pts,
    athlete_col = "athlete",
    time_col = "timestamp",
    distance_m = 200
  )

  expect_equal(nrow(result), 2L)
  # A should be faster than B
  time_a <- result[result$athlete == "A", predicted_time_sec]
  time_b <- result[result$athlete == "B", predicted_time_sec]
  expect_true(time_a < time_b)
})

test_that(".fastest_distance_vectorized returns NULL for insufficient points", {
  expect_null(yachtvaa:::.fastest_distance_vectorized(1, 1, 1, 500))
  expect_null(yachtvaa:::.fastest_distance_vectorized(
    c(0, 1), c(0, 0), c(0, 10), 500
  ))
})
