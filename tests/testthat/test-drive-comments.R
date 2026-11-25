test_that("Drive exports include ordinary comments, not just Revise tags", {
  skip_if_not_installed("googledrive")
  skip_if_not_installed("testthat", "3.2.0")
  doc <- officer::body_add_par(officer::read_docx(), "The selected passage.")
  doc <- officer::body_comment(doc, list(officer::fpar("An ordinary comment")))
  downloaded <- NULL
  testthat::local_mocked_bindings(
    drive_auth = function(...) NULL,
    drive_download = function(file, path) {
      downloaded <<- path
      print(doc, target = path)
    },
    .package = "googledrive"
  )
  expect_identical(drive_comments("example-id"),
                   list("An ordinary comment" = "The selected passage."))
  expect_false(file.exists(downloaded))
})
