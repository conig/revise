if(".revise_manuscripts" %in% objects()) rm(".revise_manuscripts")

lnz1 <- c("---", "format: \"pdf\"", "---", "", "<span id = \"SpanTest\">",
         "", "## Heading", "", "Maecenas mollis consectetur purus. Ut ultrices metus in mauris congue ultricies. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Integer pulvinar non nisi in tristique. Nam euismod nibh et mauris bibendum pellentesque.", "</span>")

lnz2 <- c("---", "format: \"pdf\"", "---", "", "<span id = \"SpanTest2\">",
          "", "Suspendisse vulputate, lacus vel finibus placerat, tortor nulla fermentum ipsum, sit amet facilisis mauris velit at lectus. Nam quis libero eget eros luctus vehicula a sit amet quam. Morbi varius augue a augue posuere, vitae tempor tellus scelerisque. Fusce sed rhoncus felis. Morbi lorem odio, egestas at mollis nec, bibendum ac magna.", "</span>")

f1 <- tempfile("f1", fileext = ".txt")
writeLines(lnz1, con = f1)
f2 <- tempfile("f2", fileext = ".txt")
writeLines(lnz2, con = f2)

test_env <- new.env()
read_manuscript(f1, envir = test_env)
read_manuscript(f2, envir = test_env)

test_that(".revise_manuscripts contains both files", {
  expect_true(length(test_env[[".revise_manuscripts"]]) == 2)
  expect_true(all(sapply(test_env[[".revise_manuscripts"]], inherits, what = "revise_manuscript")))
  expect_true(inherits(test_env[[".revise_manuscripts"]], what = "revise_corpus"))
})

test_that(".revise_manuscripts retrieves sections from both manuscripts", {
  res1 <- get_revision("SpanTest", envir = test_env)
  expect_true(grepl("Maecenas mollis", res1, fixed = TRUE))
  expect_true(grepl("Suspendisse vulputate", get_revision("SpanTest2", envir = test_env), fixed = TRUE))
  expect_true(is.null(get_revision("SpanTest3", envir = test_env)))
})

test_that("adding, rereading and editing manuscripts preserves the rest of the corpus", {
  env <- new.env(parent = emptyenv())
  files <- vapply(1:3, function(i) {
    path <- tempfile(fileext = ".md")
    writeLines(sprintf("[Revision %d]{#tag%d}", i, i), path)
    read_manuscript(path, envir = env)
    path
  }, character(1))
  expect_identical(list_manuscripts(env), basename(files))
  expect_identical(get_revision("tag3", envir = env), ">Revision 3")

  read_manuscript(files[1], envir = env)
  writeLines("[Edited revision]{#tag2}", files[2])
  read_manuscript(files[2], envir = env)
  expect_identical(list_manuscripts(env), basename(files))
  expect_identical(get_revision("tag2", envir = env), ">Edited revision")
  expect_true(remove_manuscript(basename(files[2]), env))
  expect_identical(list_manuscripts(env), basename(files[c(1, 3)]))
})
