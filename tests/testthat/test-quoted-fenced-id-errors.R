test_that("get_revision hints when a fenced-div id is double-quoted", {
  manuscript <- list(
    sections = setNames(list("Text"), "\"quoted-id\"")
  )

  expect_error(
    revise:::get_revision.default(
      id = "quoted-id",
      manuscript = manuscript,
      revise_errors = TRUE
    ),
    regexp = "surrounding double quotes"
  )
})
