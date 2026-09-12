# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

teachRai is a small R package + RStudio addin, built on `ellmer`, that gives students in-editor help (`teachr_explain`, `teachr_hint`, `teachr_debug`) by sending their selected code, recent console error, and loaded packages to Gemini. It deliberately does not capture a whole project, run a Shiny gadget, or execute arbitrary code.

## Build/test commands

- Run tests: `devtools::test()` (or `testthat::test_dir("tests/testthat")`)
- Full package check: `devtools::check()`
- Lint: `lintr::lint_package()` (config in `.lintr`)
- No CI or Makefile exist in this repo — these are the only verification steps available.

## Conventions and gotchas

- **NAMESPACE is hand-written, not roxygen-generated.** There are no `#'` roxygen tags in `R/`. Adding a new exported function requires manually adding an `export()` line to `NAMESPACE` — `devtools::document()` will not do this.
- **The Gemini model string (`"gemini-3.7-flash"`) is duplicated in three places**: `R/actions.R`, `R/addin.R`, and `R/chat.R`. If it changes, update all three.
- **`plan` mode exists but isn't exposed yet**: `teachr_run_mode`, `teachr_build_prompt`, etc. all support `mode = "plan"` and it's tested (`tests/testthat/test-plan-mode.R`), but it isn't exported in `NAMESPACE` or wired into `teachr_help()`'s addin menu. It's fine to export/wire it up if a task calls for it.
- **System prompts require British English and tidyverse-first output** (`R/prompts.R`) — responses should avoid base-R data manipulation (`apply`, `sapply`, `subset`, `merge`) in favor of the native pipe and dplyr/tidyverse patterns. Preserve this when editing prompt-building logic.
- **`exemplars.R`/`prompts.R` are fragile/iterated-on areas** — recent history shows repeated hardening of exemplar matching and prompt payload trimming. Re-run `test-exemplars.R`, `test-prompts.R`, and `test-plan-mode.R` after touching either file.
- **`GEMINI_API_KEY` lives in `~/.Renviron`**, not a repo `.env` file — set via `teachr_setup()`, which requires an R restart afterward. `.Renviron` is gitignored.
- Tests exercise internals via `teachRai:::` and do not hit the live Gemini API.
