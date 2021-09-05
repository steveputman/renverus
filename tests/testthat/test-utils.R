test_that("validate_auth_info() fails on NA on non-string args", {
  expect_error(validate_auth_info())
  expect_error(validate_auth_info("api_key_string", "client_id_string"))
  expect_error(
    validate_auth_info(
      4,
      "client_id_string",
      "client_secret_string"
    )
  )
  expect_silent(
    validate_auth_info(
      "api_key_string",
      "client_id_string",
      "client_secret_string"
    )
  )
})

test_that("get_auth_vars() uses supplied output
          or retrieves from environment vars", {
  withr::local_envvar(c(
    "ENVERUS_API_KEY" = "env_api_key",
    "ENVERUS_CLIENT_ID" = "env_client_id",
    "ENVERUS_CLIENT_SECRET" = "env_client_secret"
  ))
  envtestexpected <- list(
    api_key = "env_api_key",
    client_id = "env_client_id",
    client_secret = "env_client_secret"
  )
  expect_equal(get_auth_vars(), envtestexpected)
  arg_api_key <- "arg_api_key"
  argtestexpected <- list(
    api_key = "arg_api_key",
    client_id = "env_client_id",
    client_secret = "env_client_secret"
  )
  expect_equal(get_auth_vars(arg_api_key), argtestexpected)
})
