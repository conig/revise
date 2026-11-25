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

test_that("inline evaluation uses the supplied environment and respects evaluate", {
  path <- tempfile(fileext = ".Rmd")
  writeLines("[Value: `r { evaluated <- TRUE; value }`]{#section}", path)
  manuscript <- read_manuscript(path, to_envir = FALSE)
  env <- list2env(list(value = 42, evaluated = FALSE))

  expect_error(get_revision("section", manuscript, envir = env, trust_manuscript = FALSE),
               "evaluated <- TRUE", fixed = TRUE)
  expect_identical(
    get_revision("section", manuscript, quote = FALSE, evaluate = FALSE),
    "Value: `r { evaluated <- TRUE; value }`"
  )
  expect_false(env$evaluated)
  expect_identical(
    get_revision("section", manuscript, quote = FALSE, envir = env, trust_manuscript = TRUE),
    "Value: 42"
  )
  expect_true(env$evaluated)
})
