# teachRai

teachRai is a small, teaching-focused R package and RStudio addin built on
[`ellmer`](https://ellmer.tidyverse.org/). It is designed to help students ask
for an explanation, a hint, or debugging help from their current coding
context without adding a large interface or complex setup.

## What v0.2 includes

- a guided setup helper for saving a provider API key in `~/.Renviron`
- support for three LLM providers — Gemini, OpenAI, and Anthropic (Claude) —
  built on `ellmer`
- easy provider/model switching, either per call or for the rest of a session
  with `teachr_set_provider()` / `teachr_set_model()`
- a lightweight RStudio context capture helper
- four teaching modes: explain, hint, debug, and plan
- a simple RStudio addin entry point
- an expanded internal exemplar library covering common introductory
  analytics patterns (strings, dates, duplicates, factors, `ggplot2`, simple
  linear models, and more)
- basic console output rendering

teachRai stays intentionally small and focused so it remains easy for
students and teachers to understand.

## Installation

```r
install.packages("remotes")
remotes::install_github("Philip-Leftwich/teachRai")
```

`teachRai` depends on `ellmer` (>= 0.4.0). Installing from GitHub will install
package dependencies for you.

## Setup

Before first use, run:

```r
library(teachRai)
teachr_setup()
```

`teachr_setup()` walks you through choosing a provider (Gemini, OpenAI, or
Anthropic), helps you get an API key, and saves it to `~/.Renviron` under the
name each provider's SDK expects — `GOOGLE_API_KEY`, `OPENAI_API_KEY`, or
`ANTHROPIC_API_KEY` — along with your chosen provider as `TEACHR_PROVIDER`.
You can optionally pass `model = "..."` to also save a default model as
`TEACHR_MODEL`. After saving, restart R so the new environment variables are
available in your session.

These are the same environment variable names `ellmer` looks for by default,
so your key also works if you ever call `ellmer::chat_google_gemini()` (etc.)
directly, without going through teachRai at all.

### Switching provider or model

The provider/model saved by `teachr_setup()` become your defaults, but you
can override them at any time:

```r
# just for one call
teachr_explain(provider = "openai", model = "gpt-4.1")

# for the rest of the R session, no restart needed
teachr_set_provider("anthropic")
teachr_set_model("claude-haiku-4-5")
```

## First use

Open RStudio, highlight a piece of code if you want to focus on a selection,
and then run:

```r
teachr_help()
```

The addin menu lets you choose between:

- **Explain** — ask for a student-friendly explanation
- **Hint** — ask for a helpful next step without giving everything away
- **Debug** — ask for help understanding an error or likely bug

You can also call the actions directly:

```r
teachr_explain()
teachr_hint()
teachr_debug()
```

A fourth mode, **plan**, helps a student turn a stated goal into a concrete
next step before they start writing code:

```r
teachr_plan(goal_text = "summarise my data by group and make a bar chart")
```

## How teachRai works

teachRai keeps the first workflow deliberately simple:

1. capture the current code selection in RStudio, if there is one
2. capture the recent console error
3. capture the currently loaded packages
4. retrieve a small number of matching teaching exemplars when the selected code, plan goal, or observed debug error clearly matches a known pattern
5. build a short teaching prompt
6. send that prompt to your configured provider (Gemini by default) with `ellmer`
7. print the reply in the console

It does **not** try to capture a whole project, build a Shiny gadget, or run
arbitrary code on your behalf.
