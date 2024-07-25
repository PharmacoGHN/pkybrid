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

skip()
test_that("Empty input test", {
  result <- pk_data_translate(time = numeric(0), amt = numeric(0), dv = numeric(0), mdv = numeric(0))
  expected <- data.frame(ID = integer(0), time = numeric(0), evid = integer(0), cmt = integer(0), amt = numeric(0), rate = numeric(0), DV = numeric(0), mdv = numeric(0))
  expect_equal(result, expected)
})