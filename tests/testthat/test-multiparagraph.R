lnz <- c("---", "format: \"pdf\"", "---", "", "<span id = \"SpanTest\">",
         "", "## Heading", "", "Maecenas mollis consectetur purus. Ut ultrices metus in mauris congue ultricies. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Integer pulvinar non nisi in tristique. Nam euismod nibh et mauris bibendum pellentesque.",
         "", "Suspendisse vulputate, lacus vel finibus placerat, tortor nulla fermentum ipsum, sit amet facilisis mauris velit at lectus. Nam quis libero eget eros luctus vehicula a sit amet quam. Morbi varius augue a augue posuere, vitae tempor tellus scelerisque. Fusce sed rhoncus felis. Morbi lorem odio, egestas at mollis nec, bibendum ac magna.",
         "", "</span>")

original <- tempfile("original", fileext = ".txt")
writeLines(lnz, con = original)


test_that("collapsing file lines retains exact content", {
  tmp <- paste0(readLines(original, encoding = "UTF8"), collapse = "\n")
  result <- tempfile("result", fileext = ".txt")
  writeLines(tmp, result)
  expect_equal(readLines(result), lnz)
})

test_that("read_manuscript retains exact content", {
  out <- revise::read_manuscript(original)
  # Note: the extra newlines are because the span tags are on separate
  # lines. Technically, it is correct that they are there, because
  # <span id = "bla">\n starts with \n.
  # Intuitively, it does not make sense.
  # One can strip all newlines using trimws, but that also removes
  # intentional newlines.
  res <- strsplit(out$sections$SpanTest, split = "\n")[[1]]
  res <- res[2:length(res)]
  expect_true(all(res == lnz[6:12]))
})
