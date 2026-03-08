lnz <- c(
  "<span id = \"InlineTest\">Inline value `r 1 + 1` should not be evaluated.</span>"
)

tmpfl <- tempfile("test-inline", fileext = ".Rmd")
writeLines(lnz, con = tmpfl)

test_that("inline r evaluation requires trust", {
  manuscript <- read_manuscript(tmpfl, to_envir = FALSE)
  expect_error(
    get_revision("InlineTest", manuscript, trust_manuscript = FALSE),
    "revise has detected inline r code to evaluate",
    fixed = TRUE
  )
})
