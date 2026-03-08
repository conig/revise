test_that("fenced div parser works", {
  lnz <- c("::: {#multiline}",
           "## Heading 1",
           "",
           "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
           ":::")
  out <- revise:::extract_fenced_sections(paste0(lnz, collapse = "\n"))
  expect_equal(names(out), "multiline")
  expect_equal(unname(unlist(out)), paste0(lnz[2:4], collapse = "\n"))
})


test_that("read_manuscript picks up fenced div sections", {
  lnz <- c("---", "format: \"pdf\"", "---", "",
           "::: {#multiline}",
           "## Heading 1",
           "",
           "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
           ":::")
  tmp <- tempfile("tmp", fileext = ".txt")
  writeLines(lnz, con = tmp)
  out <- revise::read_manuscript(tmp, to_envir = FALSE)
  expect_equal(names(out$sections), "multiline")
})
