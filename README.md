# teachRai

teachRai is a small, teaching-focused R package and RStudio addin built on
[`ellmer`](https://ellmer.tidyverse.org/). It is designed to help students ask
for an explanation, a hint, or debugging help from their current coding
context without adding a large interface or complex setup.

## What v0.1 includes

- a guided setup helper for saving a Gemini API key in `~/.Renviron`
- a lightweight RStudio context capture helper
- three teaching modes: explain, hint, and debug
- a Gemini-first chat wrapper built on `ellmer`
- a simple RStudio addin entry point
- a small internal exemplar library for common introductory analytics patterns
- basic console output rendering

The first release is intentionally small so it stays easy for students and
teachers to understand.

## Installation

```r
install.packages("remotes")
remotes::install_github("Philip-Leftwich/teachRai")
```

`teachRai` depends on `ellmer`. Installing from GitHub will install package
dependencies for you.

## Setup

Before first use, run:

```r
library(teachRai)
teachr_setup()
```

`teachr_setup()` helps you get a Gemini API key and saves it to
`~/.Renviron` as `GEMINI_API_KEY`. After saving your key, restart R so the new
environment variable is available in your session.

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

## How teachRai works

teachRai keeps the first workflow deliberately simple:

1. capture the current code selection in RStudio, if there is one
2. capture the recent console error
3. capture the currently loaded packages
4. retrieve a small number of matching teaching exemplars when the context clearly fits
5. build a short teaching prompt
6. send that prompt to Gemini with `ellmer`
7. print the reply in the console

It does **not** try to capture a whole project, build a Shiny gadget, or run
arbitrary code on your behalf.
