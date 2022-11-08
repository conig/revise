lnz <- c("---", "format: \"pdf\"", "---", "",
         "", "## Heading", "", "Maecenas mollis consectetur purus. Ut ultrices metus in mauris congue ultricies. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Integer pulvinar non nisi in tristique. Nam euismod nibh et mauris bibendum pellentesque.",
         "", "Suspendisse vulputate, lacus vel finibus placerat, tortor nulla fermentum ipsum, sit amet facilisis mauris velit at lectus. Nam quis libero eget eros luctus vehicula a sit amet quam. Morbi varius augue a augue posuere, vitae tempor tellus scelerisque. Fusce sed rhoncus felis. Morbi lorem odio, egestas at mollis nec, bibendum ac magna.",
         "", "# References", "", "::: {#refs custom-style=\"Bibliography\"}", ":::")

tmp <- tempfile("tmp", fileext = ".txt")
writeLines(lnz, con = tmp)


test_that("read_manuscript works when there are no sections", {
  out <- revise::read_manuscript(tmp)
  expect_equal(out$sections, NULL)
})

lnz[10] <- paste0("[", lnz[10], "]{#test}")
writeLines(lnz, con = tmp)

test_that("read_manuscript works when there are no span sections", {
  out <- revise::read_manuscript(tmp)
  expect_equal(names(out$sections), "test")
})

lnz <- lnz[-10]
lnz[8] <- paste0("<span id = \"test\">", lnz[8], "</span>")
writeLines(lnz, con = tmp)

test_that("read_manuscript works when there are no bracket sections", {
  out <- revise::read_manuscript(tmp)
  expect_equal(names(out$sections), "test")
})
