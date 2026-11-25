test_that("plain-text rendering restores reviewer engines on success and error", {
  skip_if_not(rmarkdown::pandoc_available("2.8"))
  dir <- tempfile()
  dir.create(dir)
  input <- file.path(dir, "reply.plain-language.Rmd")
  header <- c("---", "title: A response", "authors: A. Author", "journal: Journal",
              "manuscript: MS-123", "---", "")
  writeLines(c(header, "```{asis}", "First question", "```"), input)
  engines <- knitr::knit_engines$get()
  out <- rmarkdown::render(input, output_format = revise_letter_txt(), quiet = TRUE)
  expect_identical(basename(out), "reply.plain-language.txt")
  lines <- readLines(out)
  expect_true(all(c("Manuscript MS-123", "COMMENT 1") %in% lines))
  expect_true(any(grepl("First question", lines, fixed = TRUE)))
  expect_identical(knitr::knit_engines$get("asis"), engines$asis)
  expect_identical(knitr::knit_engines$get("reviewer"), engines$reviewer)

  writeLines(c(header, "```{r}", "stop('render failed')", "```"), input)
  expect_error(rmarkdown::render(input, output_format = revise_letter_txt(), quiet = TRUE),
               "render failed")
  expect_identical(knitr::knit_engines$get("asis"), engines$asis)
  expect_identical(knitr::knit_engines$get("reviewer"), engines$reviewer)
})

test_that("PDF reviewer chunks escape manuscript punctuation before LaTeX runs", {
  skip_if_not(rmarkdown::pandoc_available("2.8"))
  input <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "title: Response", "---", "", "```{reviewer estimate}",
               "Is the 95% estimate A & B?", "```"), input)
  md <- rmarkdown::render(input, output_format = revise_letter_pdf(comment_reset_by_section = FALSE),
                          run_pandoc = FALSE, quiet = TRUE)
  text <- paste(readLines(file.path(dirname(input), basename(md))), collapse = "\n")
  expect_match(text, "\\reviewerid{estimate}", fixed = TRUE)
  expect_match(text, "Is the 95\\% estimate A \\& B?", fixed = TRUE)
})
