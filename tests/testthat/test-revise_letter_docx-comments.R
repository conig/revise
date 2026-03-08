test_that("docx comments are numbered sequentially even with tagged labels", {
  on.exit(options(revise.docx_comment_state = NULL), add = TRUE)
  revise:::init_docx_comment_state()

  first <- revise:::process_chunk_docx(list(
    code = "First reviewer comment",
    label = "rev_a_comment_primary"
  ))
  second <- revise:::process_chunk_docx(list(
    code = "Second reviewer comment",
    label = "rev_b_comment_secondary"
  ))

  expect_match(first, "COMMENT 1", fixed = TRUE)
  expect_match(second, "COMMENT 2", fixed = TRUE)
})

test_that("docx comment references are converted from latex macros", {
  on.exit(options(revise.docx_comment_state = NULL), add = TRUE)
  revise:::init_docx_comment_state()

  revise:::process_chunk_docx(list(code = "First", label = "c1"))
  revise:::process_chunk_docx(list(code = "Second", label = "c2"))

  converted <- revise:::process_docx_document(
    "Please see \\\\comment{c1}, \\\\Comment{c2}, and \\\\comment{unknown_label}."
  )

  expect_match(converted, "comment 1", fixed = TRUE)
  expect_match(converted, "Comment 2", fixed = TRUE)
  expect_match(converted, "comment unknown_label", fixed = TRUE)
})
