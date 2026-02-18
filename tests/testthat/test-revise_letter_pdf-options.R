test_that("revise_letter_pdf uses section-reset header by default", {
  fmt <- revise_letter_pdf()

  header_flag_idx <- which(fmt$pandoc$args == "--include-in-header")
  expect_length(header_flag_idx, 1L)

  header_file <- basename(fmt$pandoc$args[header_flag_idx + 1L])
  expect_equal(header_file, "header.tex")
})

test_that("revise_letter_pdf can keep comment numbering continuous", {
  fmt <- revise_letter_pdf(comment_reset_by_section = FALSE)

  header_flag_idx <- which(fmt$pandoc$args == "--include-in-header")
  expect_length(header_flag_idx, 1L)

  header_file <- basename(fmt$pandoc$args[header_flag_idx + 1L])
  expect_equal(header_file, "header_no_section_reset.tex")
})

test_that("revise_letter_pdf validates comment_reset_by_section", {
  expect_error(revise_letter_pdf(comment_reset_by_section = "no"), "must be TRUE or FALSE")
  expect_error(revise_letter_pdf(comment_reset_by_section = NA), "must be TRUE or FALSE")
  expect_error(revise_letter_pdf(comment_reset_by_section = c(TRUE, FALSE)), "must be TRUE or FALSE")
})
