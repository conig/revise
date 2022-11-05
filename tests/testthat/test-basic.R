test_that("multiple sections on one line works", {
  out <- revise:::extract_sections("[blabala]{#first} and also [dadada da da]{#second}")
  expect_equal(names(out), c("first", "second"))
  expect_equal(unname(unlist(out)), c("blabala", "dadada da da"))
})

test_that("section on multiple lines works", {
  out <- revise:::extract_sections("this is silly billy bla [blabala\nalso da da da]{#first} also fred and hank")
  expect_equal(names(out), c("first"))
  expect_equal(unname(unlist(out)), c("blabala\nalso da da da"))
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
