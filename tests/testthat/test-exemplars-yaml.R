test_that("YAML exemplar file loads with expected schema", {
  yaml_path <- system.file("extdata", "exemplars.yml", package = "teachRai")

  expect_true(nzchar(yaml_path))
  expect_true(file.exists(yaml_path))

  out <- teachRai:::teachr_load_exemplars(yaml_path)

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) >= 12)
  expect_true(all(teachRai:::teachr_required_exemplar_columns %in% names(out)))
  expect_identical(anyDuplicated(out$id), 0L)
  expect_true(all(out$mode %in% teachRai:::teachr_allowed_exemplar_modes))
})

test_that("teachr_load_exemplars validates malformed YAML content", {
  tmp_missing <- tempfile(fileext = ".yml")
  writeLines("not_exemplars: []", tmp_missing)

  expect_error(
    teachRai:::teachr_load_exemplars(tmp_missing),
    "non-empty `exemplars` key"
  )

  tmp_duplicate <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "exemplars:",
      "  - id: dup-id",
      "    mode: explain",
      "    topic: one",
      "    student_question: one",
      "    likely_misconception: one",
      "    student_code: one",
      "    instructor_hint: one",
      "    instructor_explanation: one",
      "    tags: one",
      "    source_path: one",
      "    match_terms: one",
      "    packages: one",
      "    error_pattern: ''",
      "  - id: dup-id",
      "    mode: explain",
      "    topic: two",
      "    student_question: two",
      "    likely_misconception: two",
      "    student_code: two",
      "    instructor_hint: two",
      "    instructor_explanation: two",
      "    tags: two",
      "    source_path: two",
      "    match_terms: two",
      "    packages: two",
      "    error_pattern: ''"
    ),
    tmp_duplicate
  )

  expect_error(
    teachRai:::teachr_load_exemplars(tmp_duplicate),
    "must be unique"
  )

  tmp_mode <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "exemplars:",
      "  - id: bad-mode",
      "    mode: teach",
      "    topic: one",
      "    student_question: one",
      "    likely_misconception: one",
      "    student_code: one",
      "    instructor_hint: one",
      "    instructor_explanation: one",
      "    tags: one",
      "    source_path: one",
      "    match_terms: one",
      "    packages: one",
      "    error_pattern: ''"
    ),
    tmp_mode
  )

  expect_error(
    teachRai:::teachr_load_exemplars(tmp_mode),
    "must be one of"
  )
})

test_that("ggplot retrieval includes new debug and plan exemplars", {
  debug_out <- teachr_find_exemplars(
    mode = "debug",
    selection = "ggplot(penguins_clean, aes(x = body_mass_g, y = flipper_length_mm)) geom_point()",
    recent_error = "Error: unexpected symbol in geom_point",
    packages = "ggplot2"
  )

  expect_true(nrow(debug_out) >= 1)
  expect_identical(debug_out$id[[1]], "debug-ggplot-missing-plus-layer")

  plan_out <- teachr_find_exemplars(
    mode = "plan",
    goal_text = "Plan a layered ggplot with aesthetics mapped by species and a trend line.",
    packages = "ggplot2"
  )

  expect_true(nrow(plan_out) >= 1)
  expect_identical(plan_out$id[[1]], "plan-ggplot-aesthetics-layered-plot")
})

test_that("linear model retrieval includes new debug and explain exemplars", {
  debug_out <- teachr_find_exemplars(
    mode = "debug",
    selection = "darwin |> dplyr::filter(type == 'Cross') |> lm(height ~ type, data = _)",
    recent_error = "contrasts can be applied only to factors with 2 or more levels",
    packages = c("stats", "dplyr")
  )

  expect_true(nrow(debug_out) >= 1)
  expect_identical(debug_out$id[[1]], "debug-lm-contrasts-two-levels")

  explain_out <- teachr_find_exemplars(
    mode = "explain",
    selection = "Can you explain what lm(height ~ 1) means for the intercept?",
    packages = "stats"
  )

  expect_true(nrow(explain_out) >= 1)
  expect_identical(explain_out$id[[1]], "explain-lm-intercept-only")
})
