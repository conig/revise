# Only run on our local machines, add your sys user if you want
if(Sys.info()["user"] %in% c("vanlissa")){
lnz <- c("---", "output            : pdf_document", "---", "", "# Span method",
         "", "<span id = \"spanmethod\">", "", "First line", "", "Second line",
         "", "</span>", "", "", "# MD method", "", "[", "", "First line",
         "", "Second line]{#mdmethod}")

tmpfl <- tempfile("test", fileext = ".Rmd")
writeLines(lnz, con = tmpfl)

test_that("tempfile has expected sections", {
  out <- read_manuscript(tmpfl)

  expect_equal(names(out$sections), c("mdmethod", "spanmethod"))

  expect_equal(unname(unlist(out$sections)),
               c("\nFirst line\nSecond line",
                 "\nFirst line\nSecond line\n"))
})

test_that("rendered tempfile has no brackets", {
  tmp <- rmarkdown::render(tmpfl)
  tmp <- pdftools::pdf_text(tmp)

  expect_false(grepl("[", tmp, fixed = TRUE))
  expect_false(grepl("]", tmp, fixed = TRUE))
})
}
