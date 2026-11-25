test_that("revise_letter_pdf validates comment_reset_by_section", {
  expect_error(revise_letter_pdf(comment_reset_by_section = "no"), "must be TRUE or FALSE")
  expect_error(revise_letter_pdf(comment_reset_by_section = NA), "must be TRUE or FALSE")
  expect_error(revise_letter_pdf(comment_reset_by_section = c(TRUE, FALSE)), "must be TRUE or FALSE")
})
