test_that("Word letters render custom reviewer chunks and strip manuscript tag wrappers", {
  skip_if_not(rmarkdown::pandoc_available("3.6"))
  dir <- tempfile()
  dir.create(dir)
  writeLines("[Our [revised]{#inner} passage.]{#outer}", file.path(dir, "paper.md"))
  input <- file.path(dir, "letter.Rmd")
  writeLines(c("---", "output: revise::revise_letter_docx", "---", "",
               "```{examplechunk}", "A reviewer question.", "```", "",
               "`r revise::get_revision('outer', revise::read_manuscript('paper.md', to_envir = FALSE))`"),
             input)
  old <- options(reviewer_chunkname = "examplechunk")
  on.exit(options(old))
  out <- rmarkdown::render(input, quiet = TRUE)
  text <- officer::docx_summary(officer::read_docx(out))$text
  expect_true(all(c("COMMENT 1", "Our revised passage.") %in% text))
})
