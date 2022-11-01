test_that("multiple sections on one line works", {
  out <- revise:::extract_md_sections2("[blabala]{#first} and also [dadada da da]{#second}")
  expect_equal(out$tag, c("first", "second"))
  expect_equal(out$section, c("blabala", "dadada da da"))
})

test_that("section on multiple lines works", {
  out <- revise:::extract_md_sections2("this is silly billy bla [blabala\r\nalso da da da]{#first} also fred and hank")
  expect_equal(out$tag, c("first"))
  expect_equal(out$section, c("blabala\r\nalso da da da"))
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
