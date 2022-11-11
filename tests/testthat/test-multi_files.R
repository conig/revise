if(".revise_manuscripts" %in% objects()) rm(".revise_manuscripts")

lnz1 <- c("---", "format: \"pdf\"", "---", "", "<span id = \"SpanTest\">",
         "", "## Heading", "", "Maecenas mollis consectetur purus. Ut ultrices metus in mauris congue ultricies. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Integer pulvinar non nisi in tristique. Nam euismod nibh et mauris bibendum pellentesque.", "</span>")

lnz2 <- c("---", "format: \"pdf\"", "---", "", "<span id = \"SpanTest2\">",
          "", "Suspendisse vulputate, lacus vel finibus placerat, tortor nulla fermentum ipsum, sit amet facilisis mauris velit at lectus. Nam quis libero eget eros luctus vehicula a sit amet quam. Morbi varius augue a augue posuere, vitae tempor tellus scelerisque. Fusce sed rhoncus felis. Morbi lorem odio, egestas at mollis nec, bibendum ac magna.", "</span>")

f1 <- tempfile("f1", fileext = ".txt")
writeLines(lnz1, con = f1)
f2 <- tempfile("f2", fileext = ".txt")
writeLines(lnz2, con = f2)
read_manuscript(f1)
read_manuscript(f2)

test_that(".revise_manuscripts contains both files", {
  expect_true(length(.revise_manuscripts) == 2)
  expect_true(all(sapply(.revise_manuscripts, inherits, what = "revise_manuscript")))
  expect_true(inherits(.revise_manuscripts, what = "revise_corpus"))
})
