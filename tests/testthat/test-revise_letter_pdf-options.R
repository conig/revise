test_that("positional PDF arguments are forwarded as in the CRAN release", {
  skip_if_not(rmarkdown::pandoc_available("2.8"))
  # The underlying PDF format takes toc as its first positional argument.
  expect_identical(revise_letter_pdf(FALSE)$pandoc$args,
                   revise_letter_pdf(toc = FALSE)$pandoc$args)
})

test_that("revise_letter_pdf validates comment_reset_by_section", {
  expect_error(revise_letter_pdf(comment_reset_by_section = "no"), "must be TRUE or FALSE")
  expect_error(revise_letter_pdf(comment_reset_by_section = NA), "must be TRUE or FALSE")
  expect_error(revise_letter_pdf(comment_reset_by_section = c(TRUE, FALSE)), "must be TRUE or FALSE")
})

test_that("PDF references use the printed comment number across reviewer sections", {
  # This integration test needs the full papaja LaTeX installation.
  skip_if_not(identical(Sys.getenv("REVISE_TEST_LATEX"), "true"),
              "set REVISE_TEST_LATEX=true to compile PDF letters")
  input <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "title: Response", "authors: An author", "journal: Journal",
               "class: draft", "---", "", "# First reviewer", "",
               "```{reviewer first}", "First request.", "```", "", "# Second reviewer", "",
               "```{reviewer second}", "Second request.", "```", "", "See \\Comment{second}."), input)
  for (reset in c(TRUE, FALSE)) {
    pdf <- rmarkdown::render(input, output_format = revise_letter_pdf(comment_reset_by_section = reset), quiet = TRUE)
    text <- paste(pdftools::pdf_text(pdf), collapse = " ")
    expect_match(text, paste("See Comment", if (reset) 1 else 2), fixed = TRUE)
  }
})
