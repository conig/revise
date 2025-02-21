if(rmarkdown::pandoc_available()){

test_that("Users can change reviewer chunknames", {

  rmdPath <- tempfile(fileext = ".rmd")
  outPath <- tempfile(fileext = ".docx")

  writeLines(c("---",
    "output:  revise::revise_letter_docx",
    "---",
    "```{examplechunk}",
    "Test text appears",
    "```",
    "Author response",
    ""), con = rmdPath)

  options(reviewer_chunkname = "examplechunk",
          warn = 2)

  # Min version pandoc 3.6
  if(rmarkdown::pandoc_available("3.6")) {
    testthat::expect_no_error(rmarkdown::render(rmdPath, output_file = outPath,
                                                quiet = TRUE))
  }

  options(warn = 1)
})

}
