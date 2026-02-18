test_that("sanitize_pandoc_tag_artifacts removes leaked id fragments", {
  txt <- "Yet people are often unaware]{#broader-alcohol-motives}."
  out <- revise:::sanitize_pandoc_tag_artifacts(txt)

  expect_false(grepl("\\{#", out, fixed = TRUE))
  expect_equal(out, "Yet people are often unaware.")
})

test_that("sanitize_pandoc_tag_artifacts removes leaked fenced div markers", {
  txt <- "::: {#example}\nInner text\n:::\nAfter."
  out <- revise:::sanitize_pandoc_tag_artifacts(txt)

  expect_false(grepl("^:{3,}", out))
  expect_false(grepl("\\{#example\\}", out, fixed = TRUE))
  expect_match(out, "Inner text", fixed = TRUE)
  expect_match(out, "After.", fixed = TRUE)
})

test_that("get_revision cleans leaked markdown tag fragments", {
  lnz <- c('<span id = "test">Sentence one]{#leaked-id}. Sentence two.</span>')
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(lnz, con = tmp)

  man <- revise::read_manuscript(tmp, to_envir = FALSE)
  out <- revise::get_revision("test", manuscript = man, quote = FALSE,
                              include_pgnum = FALSE, evaluate = FALSE)

  expect_false(grepl("\\{#", out, fixed = TRUE))
  expect_equal(out, "Sentence one. Sentence two.")
})
