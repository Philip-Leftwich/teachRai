teachr_providers <- function() {
  list(
    gemini = list(
      label = "Google Gemini",
      env_var = "GEMINI_API_KEY",
      signup_url = "https://aistudio.google.com/app/apikey",
      default_model = "gemini-3.7-flash",
      constructor = "chat_google_gemini"
    ),
    openai = list(
      label = "OpenAI",
      env_var = "OPENAI_API_KEY",
      signup_url = "https://platform.openai.com/api-keys",
      default_model = "gpt-4.1-mini",
      constructor = "chat_openai"
    ),
    anthropic = list(
      label = "Anthropic (Claude)",
      env_var = "ANTHROPIC_API_KEY",
      signup_url = "https://console.anthropic.com/settings/keys",
      default_model = "claude-haiku-4-5",
      constructor = "chat_anthropic"
    )
  )
}

teachr_default_provider <- function() {
  Sys.getenv("TEACHR_PROVIDER", unset = "gemini")
}

teachr_resolve_provider <- function(provider = NULL) {
  provider <- tolower(trimws(provider %||% teachr_default_provider()))
  providers <- teachr_providers()

  if (!provider %in% names(providers)) {
    stop(
      "Unknown provider '", provider, "'. Supported: ",
      paste(names(providers), collapse = ", "), ".",
      call. = FALSE
    )
  }

  providers[[provider]]
}

teachr_resolve_api_key <- function(provider_info, api_key = NULL) {
  api_key %||% Sys.getenv(provider_info$env_var, unset = "")
}

teachr_resolve_model <- function(provider_info, model = NULL) {
  model %||% provider_info$default_model
}
