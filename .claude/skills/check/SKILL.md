---
name: check
description: Run the full pre-PR verification gate for teachRai (lintr::lint_package(), devtools::test(), devtools::check()) since there is no CI enforcing this. Use before committing or opening a PR, or whenever asked to verify the package is in a good state.
---

There is no CI in this repo, so this is the only gate before a change ships. Run all steps and report results plainly — don't declare success if any step produced warnings/errors/notes/lints.

1. Run `lintr::lint_package()` (config in `.lintr`). Report any lints verbatim.
2. Run `devtools::test()` from the package root. Report pass/fail counts. If failures touch `test-exemplars.R`, `test-prompts.R`, or `test-plan-mode.R`, call that out specifically — those areas have a history of fragility.
3. Run `devtools::check()`. Report any ERRORs, WARNINGs, or NOTEs verbatim — do not summarize them away.
4. If any step fails, stop and fix the root cause rather than working around it (e.g. do not skip tests, suppress check output, or add lint exclusions just to silence a warning).
5. Summarize final status: lint count, tests passed/failed count, and check ERROR/WARNING/NOTE count.
