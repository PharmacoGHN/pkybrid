test_that("create_model works as expected", {
  # Testing valid input
  expect_equal(create_model("oral_1cp", "micro"), "placeholder_model_file")
  expect_equal(create_model("oral_2cp", "macro"), "placeholder_model_file")
  expect_equal(create_model("iv_1cp", "micro"), "placeholder_model_file")
  expect_equal(create_model("iv_2cp", "macro"), "placeholder_model_file")
  expect_equal(create_model("scim_1cp", "micro"), "placeholder_model_file")
  expect_equal(create_model("scim_2cp", "macro"), "placeholder_model_file")

  # Testing invalid model type
  expect_error(
    create_model("invalid_type", "micro", engine = "parametric"),
    "Invalid model type. Choose from 'oral_1cp', 'oral_2cp', 'iv_1cp', 'iv_2cp', 'scim_1cp', 'scim_2cp'"
  )

  # Testing invalid engine
  expect_error(
    create_model("oral_1cp", "micro", engine = "invalid_engine"),
    "Invalid engine. Chose either parametric or NPAG."
  )

  # Testing NPAG engine
  expect_error(
    create_model("oral_1cp", "micro", engine = "NPAG"),
    "NPAG is not available in V1.0"
  )

  # Testing invalid constant
  expect_error(create_model("oral_1cp", constant = "invalid_constant", engine = "parametric"), "Invalid constant. Model parameter should be 'macro' (CL, V etc.) or 'micro' (ke, V, etc.).", fixed = TRUE)
})


test_that("get_model_type works as expected", {

  # Test 1cp models with macro and micro constants
  expect_equal(get_model_type("oral_1cp", "macro"), "oral_1cp_ka_Cl.V")
  expect_equal(get_model_type("oral_1cp", "micro"), "oral_1cp_ka_ke.V")
  expect_equal(get_model_type("iv_1cp", "macro"), "iv_1cp_Cl.V")
  expect_equal(get_model_type("iv_1cp", "micro"), "iv_1cp_ke.V")
  expect_equal(get_model_type("oral_1cp", "macro", tlag = TRUE), "oral_1cp_ka_Cl.V_Tlag")

  # Test 2cp models with macro and micro constants
  expect_equal(get_model_type("oral_2cp", "macro"), "oral_2cp_ka_Cl.V.Q.V2")
  expect_equal(get_model_type("oral_2cp", "micro"), "oral_2cp_ka_ke.V.k12.k21")
  expect_equal(get_model_type("iv_2cp", "macro"), "iv_2cp_Cl.V.Q.V2")
  expect_equal(get_model_type("iv_2cp", "micro"), "iv_2cp_ke.V.k12.k21")

  # Test sc_im_1cp models with macro and micro constants
  expect_equal(get_model_type("scim_1cp", "macro"), "scim_1cp_ka_Cl.V")
  expect_equal(get_model_type("scim_1cp", "micro"), "scim_1cp_ka_ke.V")

  # Test scim_2cp models with macro and micro constants
  expect_equal(get_model_type("scim_2cp", "macro"), "scim_2cp_ka_Cl.V.Q.V2")
  expect_equal(get_model_type("scim_2cp", "micro"), "scim_2cp_ka_ke.V.k12.k21")

  # Test invalid model type and constant combinations
  expect_error(get_model_type("oral_3cp", "macro"), "Non existing model. Please see ?pkybrid::get_model_type for more information", fixed = TRUE)
  expect_error(get_model_type("iv_2cp", "macro", tlag = TRUE), "Can't have a Tlag with an iv model")
})