test_that("DOCX rendering resolves reviewer references and restarts numbering", {
  skip_if_not(rmarkdown::pandoc_available("3.6"))
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  input <- file.path(dir, "response.Rmd")
  writeLines(c(
    "---", "title: Response", "---", "",
    "See \\Comment{later} and \\comment{missing}.", "",
    "```{reviewer first}",
    "As noted in \\comment{later}.",
    "```", "",
    "# Another reviewer", "",
    "```{asis later}",
    "Second reviewer comment.",
    "```", "",
    "In reply to \\comment{first}."
  ), input)

  format <- revise_letter_docx()
  for (i in seq_len(2)) {
    path <- rmarkdown::render(input, output_format = format, quiet = TRUE)
    paragraphs <- officer::docx_summary(officer::read_docx(path))$text
    expect_true(all(c("COMMENT 1", "COMMENT 2") %in% paragraphs))
    expect_true("See Comment 2 and comment missing." %in% paragraphs)
    expect_true(any(grepl("As noted in comment 2.", paragraphs, fixed = TRUE)))
    expect_true("In reply to comment 1." %in% paragraphs)
  }
})
