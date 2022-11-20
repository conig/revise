test_that("Users can change reviewer chunknames", {

  rmdPath <- tempfile(fileext = ".rmd")
  outPath <- tempfile(fileext = ".docx")

  writeLines(c("---",
    "output:  revise::letter.docx",
    "---",
    "```{examplechunk}",
    "Test text appears",
    "```",
    "Author response",
    ""), con = rmdPath)

  options(reviewer_chunkname = "examplechunk")
  tryCatch(rmarkdown::render(rmdPath, output_file = outPath), error = function(e) return(FALSE))

  testthat::expect_true(file.exists(outPath))
})
