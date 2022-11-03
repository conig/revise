test_that("new parser works for spans", {
  lnz <- c("---", "output            : pdf_document", "---", "", "# Span method",
           "", "<span id = \"spanmethod\">", "", "First line", "", "Second line",
           "", "</span>", "", "", "# MD method", "", "[", "", "First line",
           "", "Second line]{#mdmethod}")


  out <- revise:::extract_sections(string = lnz, is_span = TRUE)
  expect_equal(names(out), c("spanmethod"))
  expect_equal(unname(unlist(out)), c("\r\n\r\nFirst line\r\n\r\nSecond line\r\n\r\n"))
})

test_that("new parser works for square brackets", {
  lnz <- c("---", "output            : pdf_document", "---", "", "# Span method",
           "", "<span id = \"spanmethod\">", "", "First line", "", "Second line",
           "", "</span>", "", "", "# MD method", "", "[", "", "First line",
           "", "Second line]{#mdmethod}")


  out <- revise:::extract_sections(string = lnz)
  expect_equal(names(out), c("mdmethod"))
  expect_equal(unname(unlist(out)), c("\r\n\r\nFirst line\r\n\r\nSecond line"))
})

test_that("new parser works with nested spans", {
  lnz <- c("---", "output            : pdf_document", "---", "", "# Span method",
           "", "<span id = \"spanmethod\">", "", "First line", "<span id = \"anotherone\">a second span...</span>",
           "Second line", "", "</span>")

  out <- revise:::extract_sections(string = lnz, is_span = TRUE)
  expect_equal(names(out), c("spanmethod", "anotherone"))
  expect_equal(unname(unlist(out)), c("a second span...", '\r\n\r\nFirst line\r\n<span id = "anotherone">a second span...</span>\r\nSecond line\r\n\r\n'))
})

test_that("nested example works", {
  nested <- "[This [text]{#inner} has nested references[@ref]]{#outer}"

  out <- revise:::extract_sections(nested)
  expect_equal(names(out), c("inner", "outer"))
  expect_equal(unname(unlist(out)), c("text", "This [text]{#inner} has nested references[@ref]"))
})

test_that("sic works", {
  out <- revise:::extract_sections("This sentence [includes square brackets which aren't [sic] references]{#example}")
  expect_equal(names(out), c("example"))
  expect_equal(unname(unlist(out)), c("includes square brackets which aren't [sic] references"))
})

test_that("another inner works", {
  out <- revise:::extract_sections("[This [text]{#inner} has nested references[-@ref]]{#outer}")
  expect_equal(names(out), c("inner", "outer"))
  expect_equal(unname(unlist(out)), c("text", "This [text]{#inner} has nested references[-@ref]"))
})


test_that("another another inner works", {
  out <- revise:::extract_sections("[This [text]{#inner} has nested references[e.g., @ref]]{#outer}")
  expect_equal(names(out), c("inner", "outer"))
  expect_equal(unname(unlist(out)), c("text", "This [text]{#inner} has nested references[e.g., @ref]"))
})
