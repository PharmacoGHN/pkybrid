test_that("call_engine : Valid engine names work correctly", {
  expect_silent(call_engine("parametric"))
  expect_silent(call_engine("NPAG"))
})

test_that("call_engine : Invalid engine name raises an error", {
  expect_error(call_engine("invalid"), "Invalid engine. Must choose betwee, 'parametric' and 'NPAG' (non parametric) engine", fixed = TRUE)
})

test_that("call_engine : Default parameter is 'parametric'", {
  result <- call_engine()
  expect_true(is.list(result))
  expect_true(any(grepl("parametrics", result[[1]])))
})


test_that("call_model : Invalid engine name raises an error", {
  expect_error(call_model("test_model", "invalid"), "Invalid engine. Must choose between, 'parametric' and 'NPAG' (non parametric) engine", fixed = TRUE)
})

skip()
test_that("call_model : Valid engine names work correctly", {
  expect_silent(call_model("test_model", "parametric"))
  expect_silent(call_model("test_model", "NPAG"))
})

test_that("call_model : Default parameter is 'parametric'", {
  result <- call_model("test_model")
  expect_true(grepl("Compiled model:", result))
})

test_that("call_model : Valid model name constructs correct path and calls mcode", {
  model_path <- system.file("models/parametrics/", package = "pkybrid")
  model_name <- "test_model"
  expected_model <- paste0(model_path, model_name)
  result <- call_model(model_name)
  expect_true(grepl("Compiled model:", result))
  expect_true(grepl(expected_model, result))
})