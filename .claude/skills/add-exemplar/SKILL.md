---
name: add-exemplar
description: Add a new teaching exemplar to inst/extdata/exemplars.yaml, following the existing schema, and verify it with the relevant tests. Use when asked to add, extend, or curate teaching exemplars (explain/hint/debug/plan examples).
---

Exemplars are curated teaching examples used by `teachr_find_exemplars()` (`R/exemplars.R`) to match student code/errors to the right worked example. They live in `inst/extdata/exemplars.yaml` — a YAML list of records — loaded once at package load (`R/zzz.R`'s `.onLoad()`) into the `teachr_exemplars` data.frame via `teachr_load_exemplars()`.

## Critical: what actually drives retrieval

**`match_terms` is the only field `teachr_find_exemplars()` matches against.** It tokenizes the student's current code *selection* (or, for debug mode, the observed error text) and checks for word/phrase overlap against each exemplar's `match_terms` (a comma-separated keyword list) and `error_pattern` (a regex, debug mode only). Retrieval never looks at `student_question`.

**`student_question`, `topic`, and `likely_misconception` are display-only.** They're formatted straight into the prompt sent to the LLM (`teachr_format_exemplar_entry()` in `R/prompts.R`) to frame the exemplar for the model — but they are never parsed, tokenized, or compared against anything. Writing a great `student_question` does nothing for retrieval if `match_terms` doesn't overlap with the vocabulary that will actually appear in a student's *code selection*.

This means: when authoring a new exemplar, spend your care on `match_terms` — pull the words a student's code/error would actually contain (function names, argument names, package names, error-message fragments), not paraphrases of the question. An exemplar with a perfect `student_question` and thin/stale `match_terms` will simply never surface.

## Steps

1. Read `inst/extdata/exemplars.yaml` in full first. Identify the exact schema (13 fields per record: `id, mode, topic, student_question, likely_misconception, student_code, instructor_hint, instructor_explanation, tags, source_path, match_terms, packages, error_pattern`) by inspecting current entries — do not assume a schema.
2. Match the existing style exactly: same fields, same YAML quoting (double-quoted plain scalars; `student_code` as a `|-` literal block), same provenance-label format (`source_path`'s first `/`-segment becomes "X teaching materials" — these are tags/history, not real file paths to resolve).
3. Pick the correct `mode` for the new exemplar (`explain`, `hint`, `debug`, or `plan` — see `teachr_run_mode` in `R/actions.R` for valid modes). Keep exemplar content tidyverse-first and in British English, matching `R/prompts.R`'s system prompt conventions.
4. Write `match_terms` deliberately: list the actual function/argument/package names and error fragments a student's selection or error would contain. For `debug` mode, also set `error_pattern` (a perl regex matched against the observed error text).
5. Add the new record(s) to the YAML list, keeping formatting consistent with neighboring entries.
6. Run `testthat::test_file("tests/testthat/test-exemplars.R")` (and `test-prompts.R` / `test-plan-mode.R` if the change touches matching or prompt-building logic) to confirm nothing broke — exemplar matching has been a historically fragile area.
7. Report which exemplar(s) were added and confirm the test files that were run.
