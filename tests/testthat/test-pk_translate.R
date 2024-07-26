# testthat::test_that("pk_translate work with 1 cov", {
#   tibble::tribble(
#     ~ID, ~time, ~evid, ~cmt, ~amt,  ~rate,  ~DV,   ~mdv, ~BW,
#     1,     0,     1,    1,    100,   20,    NA,     1,    90,
#     1,     6,     0,    1,      0,   0,     3.9,    0,    90,
#     1,    15,     0,    1,      0,   0,     1.1,    0,    90,
#     1,    24,     0,    1,      0,   0,     2.0,    1,    90
#   )
# })

test_that("Basic functionality test", {
  result <- pk_data_translate(id = c(1, 1, 1, 1), time = c(0, 6, 15, 24), amt = c(100, 0, 0, 0), dv = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1))
  expected <- data.frame(ID = c(1, 1, 1, 1), time = c(0, 6, 15, 24), evid = c(1, 0, 0, 0), cmt = c(1, 1, 1, 1), amt = c(100, 0, 0, 0), rate = c(0, 0, 0, 0), DV = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1))
  expect_equal(result, expected)
})


test_that("Missing ID test", {
  expect_warning(result <- pk_data_translate(time = c(0, 6, 15, 24), amt = c(100, 0, 0, 0), dv = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1)),  "ID value was not provided. All ID are set to 1.")
  expected <- data.frame(ID = c(1, 1, 1, 1), time = c(0, 6, 15, 24), evid = c(1, 0, 0, 0), cmt = c(1, 1, 1, 1), amt = c(100, 0, 0, 0), rate = c(0, 0, 0, 0), DV = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1))
  expect_equal(result, expected)
  
})


test_that("Non-PO route test", {
  result <- pk_data_translate(id = c(1, 1, 1, 1), time = c(0, 6, 15, 24), amt = c(100, 0, 0, 0), dv = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1), route = "IV", rate = c(20, 0, 0, 0))
  expected <- data.frame(ID = c(1, 1, 1, 1), time = c(0, 6, 15, 24), evid = c(1, 0, 0, 0), cmt = c(1, 1, 1, 1), amt = c(100, 0, 0, 0), rate = c(20, 0, 0, 0), DV = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1))
  expect_equal(result, expected)
})

test_that("Non-PO route test, null rate", {
  expect_error(
    pk_data_translate(id = c(1, 1, 1, 1), time = c(0, 6, 15, 24), amt = c(100, 0, 0, 0), dv = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1), route = "IV"),
    "No infusion rate given, despite the drug not being administered by PO route"
  )
  
})

test_that("Covariates test", {
  cov <- data.frame(BW = c(90, 90, 90, 90))
  result <- pk_data_translate(id = c(1, 1, 1, 1), time = c(0, 6, 15, 24), amt = c(100, 0, 0, 0), dv = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1), cov = cov)
  expected <- data.frame(ID = c(1, 1, 1, 1), time = c(0, 6, 15, 24), evid = c(1, 0, 0, 0), cmt = c(1, 1, 1, 1), amt = c(100, 0, 0, 0), rate = c(0, 0, 0, 0), DV = c(NA, 3.9, 1.1, 2.0), mdv = c(1, 0, 0, 1), BW = c(90, 90, 90, 90))
  expect_equal(result, expected)
})


test_that("get_observation_time works correctly with date and time", {
  date <- c("2021-01-01", "2021-01-01", "2021-01-02")
  time <- c("12:00:00", "14:00:00", "12:00:00")
  expected <- c(0, 2, 24)  # 2 hours and 22 hours
  result <- get_observation_time(date, time)
  expect_equal(result, expected)
})

test_that("get_observation_time works correctly with POSIXct time and merged = TRUE", {
  datetime <- as.POSIXct(c("2021-01-01 12:00:00", "2021-01-01 14:00:00", "2021-01-02 12:00:00"))
  expected <- c(0, 2, 24)
  result <- get_observation_time(time = datetime, merged = TRUE)
  expect_equal(result, expected)
})

test_that("get_observation_time works correctly with different unit_scale", {
  date <- c("2021-01-01", "2021-01-01", "2021-01-02")
  time <- c("12:00:00", "14:00:00", "12:00:00")
  expected <- c(0, 120, 1440)  # 2*60 minutes and 22*60 minutes
  result <- get_observation_time(date, time, unit_scale = "mins")
  expect_equal(result, expected)
})

test_that("get_observation_time throws error for invalid unit_scale", {
  date <- c("2021-01-01", "2021-01-01", "2021-01-02")
  time <- c("12:00:00", "14:00:00", "12:00:00")
  expect_error(get_observation_time(date, time, unit_scale = "years"), "Invalid unit_scale")
})

test_that("get_observation_time throws error for mismatched date and time lengths", {
  date <- c("2021-01-01")
  time <- c("12:00:00", "14:00:00", "12:00:00")
  expect_error(get_observation_time(date, time), "date and time are not the same length")
})

test_that("get_observation_time throws error for invalid POSIXct input with merged = TRUE", {
  time <- c("2021-01-01 12:00:00")
  expect_error(get_observation_time(time = time, merged = TRUE), "must be a POSIXct object")
})