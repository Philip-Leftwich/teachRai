---
name: add-exemplar
description: Add a new teaching exemplar to the hand-maintained data.frame in R/exemplars.R, following the existing schema, and verify it with the relevant tests. Use when asked to add, extend, or curate teaching exemplars (explain/hint/debug/plan examples).
---

Exemplars are curated teaching examples used by `teachr_find_exemplars` to match student code/errors to the right worked example. They live as a hand-written `data.frame` in `R/exemplars.R` — there is no external data file.

## Steps

1. Read `R/exemplars.R` in full first. Identify the exact column schema of the existing exemplar `data.frame` (e.g. mode, topic/tags, matching terms, prompt text, provenance label) by inspecting current rows — do not assume a schema.
2. Match the existing style exactly: same columns, same tagging/term conventions used for matching, same provenance-label format (these labels are just tags/history, not real file paths — don't try to resolve them).
3. Pick the correct `mode` for the new exemplar (`explain`, `hint`, `debug`, or `plan` — see `teachr_run_mode` in `R/actions.R` for valid modes). Keep exemplar content tidyverse-first and in British English, matching `R/prompts.R`'s system prompt conventions.
4. Add the new row(s) to the `data.frame`, keeping formatting consistent with neighboring rows.
5. Run `testthat::test_file("tests/testthat/test-exemplars.R")` (and `test-prompts.R` / `test-plan-mode.R` if the change touches matching or prompt-building logic) to confirm nothing broke — exemplar matching has been a historically fragile area.
6. Report which exemplar(s) were added and confirm the test files that were run.
