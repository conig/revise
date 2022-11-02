test_that("new parser works for spans", {
  lnz <- c("---", "output            : pdf_document", "---", "", "# Span method",
           "", "<span id = \"spanmethod\">", "", "First line", "", "Second line",
           "", "</span>", "", "", "# MD method", "", "[", "", "First line",
           "", "Second line]{#mdmethod}")


  out <- extract_md_sections2(string = lnz, is_span = TRUE)
  expect_equal(out$tag, c("spanmethod"))
  expect_equal(out$section, c("\r\n\r\nFirst line\r\n\r\nSecond line\r\n\r\n"))
})

test_that("new parser works for square brackets", {
  lnz <- c("---", "output            : pdf_document", "---", "", "# Span method",
           "", "<span id = \"spanmethod\">", "", "First line", "", "Second line",
           "", "</span>", "", "", "# MD method", "", "[", "", "First line",
           "", "Second line]{#mdmethod}")


  out <- extract_md_sections2(string = lnz)
  expect_equal(out$tag, c("mdmethod"))
  expect_equal(out$section, c("\r\n\r\nFirst line\r\n\r\nSecond line"))
})

test_that("new parser works with nested spans", {
  lnz <- c("---", "output            : pdf_document", "---", "", "# Span method",
           "", "<span id = \"spanmethod\">", "", "First line", "<span id = \"anotherone\">a second span...</span>",
           "Second line", "", "</span>")

  out <- extract_md_sections2(string = lnz, is_span = TRUE)
  expect_equal(out$tag, c("spanmethod", "anotherone"))
  expect_equal(out$section, c("a second span...", '\r\n\r\nFirst line\r\n<span id = "anotherone">a second span...</span>\r\nSecond line\r\n\r\n'))
})

test_that("nested example works", {
  nested <- "[This [text]{#inner} has nested references[@ref]]{#outer}"

  out <- revise:::extract_md_sections2(nested)
  expect_equal(out$tag, c("inner", "outer"))
  expect_equal(out$section, c("text", "This [text]{#inner} has nested references[@ref]"))
})

test_that("sic works", {
  out <- revise:::extract_md_sections2("This sentence [includes square brackets which aren't [sic] references]{#example}")
  expect_equal(out$tag, c("example"))
  expect_equal(out$section, c("includes square brackets which aren't [sic] references"))
})

test_that("another inner works", {
  out <- revise:::extract_md_sections2("[This [text]{#inner} has nested references[-@ref]]{#outer}")
  expect_equal(out$tag, c("inner", "outer"))
  expect_equal(out$section, c("text", "This [text]{#inner} has nested references[-@ref]"))
})


test_that("another another inner works", {
  out <- revise:::extract_md_sections2("[This [text]{#inner} has nested references[e.g., @ref]]{#outer}")
  expect_equal(out$tag, c("inner", "outer"))
  expect_equal(out$section, c("text", "This [text]{#inner} has nested references[e.g., @ref]"))
})
