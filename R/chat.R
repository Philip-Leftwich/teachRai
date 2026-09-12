teachr_chat <- function(prompt,
                        system_prompt = NULL,
                        provider = NULL,
                        model = NULL,
                        api_key = NULL) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop(
      "The `ellmer` package is required to chat with an LLM provider. Install `ellmer` first.",
      call. = FALSE
    )
  }

  provider_info <- teachr_resolve_provider(provider)
  api_key <- teachr_resolve_api_key(provider_info, api_key)
  model <- teachr_resolve_model(provider_info, model)

  if (!nzchar(api_key)) {
    stop(
      "No API key was found for provider '", provider_info$label, "'. ",
      "Run teachr_setup() first, or set ", provider_info$env_var, ".",
      call. = FALSE
    )
  }

  constructor <- getExportedValue("ellmer", provider_info$constructor)
  chat <- constructor(
    system_prompt = system_prompt,
    api_key = api_key,
    model = model
  )

  chat$chat(prompt)
}
