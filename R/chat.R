teachr_chat <- function(prompt,
                        system_prompt = NULL,
                        model = "gemini-3.7-flash",
                        api_key = Sys.getenv("GEMINI_API_KEY")) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop(
      "The `ellmer` package is required to chat with Gemini. Install `ellmer` first.",
      call. = FALSE
    )
  }

  if (!nzchar(api_key)) {
    stop(
      "No Gemini API key was found. Run teachr_setup() first.",
      call. = FALSE
    )
  }

  chat <- ellmer::chat_google_gemini(
    system_prompt = system_prompt,
    api_key = api_key,
    model = model
  )

  chat$chat(prompt)
}
