test_that("regex matches perfect example", {
  expect_equal(unlist(revise:::extract_sections('<span id = "hai">works</span>', is_span = TRUE)), c("hai" = "works"))
})

test_that("regex matches weird spacing", {
  expect_equal(unlist(
    revise:::extract_sections('<span id ="hai">works</span>', is_span = TRUE)
  ), c("hai" = "works"))})

test_that("regex matches no spacing", {
  expect_equal(unlist(
    revise:::extract_sections('<span id="hai">works</span>', is_span = TRUE)
    ),
    c("hai" = "works"))})

test_that("regex matches weird spacing 2", {
  expect_equal(unlist(
    revise:::extract_sections('<span id= "hai">works</span>', is_span = TRUE)
  ),
  c("hai" = "works"))})

test_that("regex handles other attributes in span tag:", {
  expect_equal(unlist(
    revise:::extract_sections('<span style="color:blue" id= "hai">works</span>', is_span = TRUE)
  ),
  c("hai" = "works"))})

