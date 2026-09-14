test_that("teachr_resolve_provider defaults to gemini", {
  withr_unset <- Sys.getenv("TEACHR_PROVIDER")
  Sys.unsetenv("TEACHR_PROVIDER")
  on.exit(if (nzchar(withr_unset)) Sys.setenv(TEACHR_PROVIDER = withr_unset), add = TRUE)

  info <- teachr_resolve_provider()
  expect_identical(info$label, "Google Gemini")
  expect_identical(info$env_var, "GEMINI_API_KEY")
})

test_that("teachr_resolve_provider resolves case-insensitively and trims whitespace", {
  info <- teachr_resolve_provider(" OpenAI ")
  expect_identical(info$label, "OpenAI")
  expect_identical(info$env_var, "OPENAI_API_KEY")
})

test_that("teachr_resolve_provider reads TEACHR_PROVIDER when no argument is given", {
  Sys.setenv(TEACHR_PROVIDER = "anthropic")
  on.exit(Sys.unsetenv("TEACHR_PROVIDER"), add = TRUE)

  info <- teachr_resolve_provider()
  expect_identical(info$label, "Anthropic (Claude)")
})

test_that("teachr_resolve_provider errors clearly on an unknown provider", {
  expect_error(teachr_resolve_provider("not-a-real-provider"), "Unknown provider")
})

test_that("teachr_resolve_api_key falls back to the provider's env var", {
  Sys.setenv(OPENAI_API_KEY = "env-key")
  on.exit(Sys.unsetenv("OPENAI_API_KEY"), add = TRUE)

  info <- teachr_resolve_provider("openai")
  expect_identical(teachr_resolve_api_key(info), "env-key")
  expect_identical(teachr_resolve_api_key(info, "explicit-key"), "explicit-key")
})

test_that("teachr_resolve_model falls back to the provider's default model", {
  info <- teachr_resolve_provider("gemini")
  expect_identical(teachr_resolve_model(info), "gemini-3.7-flash")
  expect_identical(teachr_resolve_model(info, "custom-model"), "custom-model")
})

test_that("teachr_resolve_provider prefers the teachr.provider option over the env var", {
  Sys.setenv(TEACHR_PROVIDER = "openai")
  on.exit(Sys.unsetenv("TEACHR_PROVIDER"), add = TRUE)
  options(teachr.provider = "anthropic")
  on.exit(options(teachr.provider = NULL), add = TRUE)

  info <- teachr_resolve_provider()
  expect_identical(info$label, "Anthropic (Claude)")

  info <- teachr_resolve_provider("gemini")
  expect_identical(info$label, "Google Gemini")
})

test_that("teachr_resolve_model prefers option over TEACHR_MODEL env var over default", {
  info <- teachr_resolve_provider("gemini")

  Sys.setenv(TEACHR_MODEL = "env-model")
  on.exit(Sys.unsetenv("TEACHR_MODEL"), add = TRUE)
  expect_identical(teachr_resolve_model(info), "env-model")

  options(teachr.model = "option-model")
  on.exit(options(teachr.model = NULL), add = TRUE)
  expect_identical(teachr_resolve_model(info), "option-model")

  expect_identical(teachr_resolve_model(info, "explicit-model"), "explicit-model")
})

test_that("teachr_set_model and teachr_set_provider set session options", {
  on.exit(options(teachr.model = NULL, teachr.provider = NULL), add = TRUE)

  teachr_set_provider("openai")
  expect_identical(getOption("teachr.provider"), "openai")

  teachr_set_model("custom-model")
  expect_identical(getOption("teachr.model"), "custom-model")

  teachr_set_model("another-model", provider = "Anthropic")
  expect_identical(getOption("teachr.model"), "another-model")
  expect_identical(getOption("teachr.provider"), "anthropic")
})
