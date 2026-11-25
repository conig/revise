test_that("the reviewer addin wraps unsaved source without rereading a stale file", {
  skip_if_not_installed("testthat", "3.2.0")
  contents <- c("---", "output:", "  revise::revise_letter_pdf:",
                "    comment_reset_by_section: false", "---", "", "My question?", "Reply")
  context <- list(id = "source", path = tempfile(fileext = ".Rmd"), contents = contents,
                  selection = list(list(range = rstudioapi::document_range(c(7, 1, 7, 13)))))
  edited <- NULL
  testthat::local_mocked_bindings(
    getActiveDocumentContext = function() context,
    setDocumentContents = function(text, id) edited <<- strsplit(text, "\n", fixed = TRUE)[[1]],
    .package = "rstudioapi"
  )
  reviewer_comment()
  expect_identical(edited[seq_len(6)], contents[seq_len(6)])
  expect_identical(tail(edited[nzchar(edited)], 4L), c("```{asis}", "My question?", "```", "Reply"))
})
