# Only run on our local machines, add your sys user if you want
if(FALSE){
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
               c("\n\nFirst line\n\nSecond line",
                 "\n\nFirst line\n\nSecond line\n\n"))
})

test_that("rendered tempfile has no brackets", {
  tmp <- rmarkdown::render(tmpfl, quiet = TRUE)
  tmp <- pdftools::pdf_text(tmp)

  expect_false(grepl("[", tmp, fixed = TRUE))
  expect_false(grepl("]", tmp, fixed = TRUE))
})
}

# lnz <- c("---", "output            : md_document", "---", "", "# Span method",
#            "", "<span id = \"spanmethod\">", "", "First line", "", "Second line",
#            "", "</span>", "", "", "# MD method", "", "[", "", "First line",
#            "", "Second line]{#mdmethod}",
#          "This is a [third line]{#third} to test if it shows up.")
#
#   tmpfl <- tempfile("test", fileext = ".Rmd")
#   writeLines(lnz, con = tmpfl)
#
# test_that("rendered md has no brackets", {
#     tmp <- rmarkdown::render(tmpfl, quiet = TRUE)
#     tmp <- readLines(tmp)
#
#     expect_false(any(grepl("[", tmp, fixed = TRUE)))
#     expect_false(any(grepl("]", tmp, fixed = TRUE)))
#   })
#
