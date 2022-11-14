lnz <- c("How does [revise]{#revise} get on with mutliple identical tags like [this one]{#revise}?")

tmpfl <- tempfile("test", fileext = ".Rmd")
writeLines(lnz, con = tmpfl)

test_that("warning on duplicate ids", {
  expect_warning(out <- read_manuscript(tmpfl))
  expect_warning(get_revision("revise", out, revise_errors = FALSE))
})

lnz <- c('How does <span id = "revise">revise get on with mutliple identical ',
         'tags</span>, including when those tags are <span id = "revise2"> span tags like this and [square brackets]{#revise}?</span>')

tmpfl <- tempfile("test", fileext = ".Rmd")
writeLines(lnz, con = tmpfl)

test_that("error on duplicate ids in span and square tags", {
  expect_warning(out <- read_manuscript(tmpfl))
  expect_warning(get_revision("revise", out, revise_errors = FALSE))
})
