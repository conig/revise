if (rmarkdown::pandoc_available("2.0")){
lnz <- c("---","title: \"test doc\"", "format: \"github_document\"", "---", "", "<span id = \"SpanTest\">",
         "", "## Heading", "", "Maecenas mollis consectetur purus. Ut ultrices metus in mauris congue ultricies. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Integer pulvinar non nisi in tristique. Nam euismod nibh et mauris bibendum pellentesque.",
         "", "Suspendisse vulputate, lacus vel finibus placerat, tortor nulla fermentum ipsum, sit amet facilisis mauris velit at lectus. Nam quis libero eget eros luctus vehicula a sit amet quam. Morbi varius augue a augue posuere, vitae tempor tellus scelerisque. Fusce sed rhoncus felis. Morbi lorem odio, egestas at mollis nec, bibendum ac magna.",
         "", "</span>")

original <- tempfile("original", fileext = ".Rmd")
writeLines(lnz, con = original)

lnz <- c("---","title: \"test doc\"", "format: \"github_document\"", "---", "",
            "```{r}",
         "library(revise)",
         "options(revise_errors = FALSE)",
         "options(revise_errors = TRUE)",
            paste0("man <- read_manuscript('", gsub("\\", "\\\\", original, fixed = T), "')"),
            "```",
            "`r revise::get_revision(\"doesntexist\", man)`",
            "")
no_error <- tempfile("noerror", fileext = ".Rmd")
allow_error <- tempfile("error", fileext = ".Rmd")
writeLines(lnz[-9], con = no_error)
writeLines(lnz[-8], con = allow_error)

test_that("errors when section doesn't exist", {
  expect_error(suppressMessages(out <- rmarkdown::render(allow_error, quiet = TRUE)))
})

test_that("errors when section doesn't exist can be switched off", {
  expect_warning(out <- rmarkdown::render(no_error, quiet = TRUE))
  tmp <- readLines(out)
  expect_true(any(grepl("Couldn’t find a section in the manuscript", tmp)))
})

lnz <- c("---","title: \"test doc\"", "format: \"github_document\"", "---", "",
         "```{r}",
         "library(revise)",
         "rm('.revise_manuscript', envir = parent.frame(1))",
         paste0("read_manuscript('", gsub("\\", "\\\\", original, fixed = T), "')"),
         paste0("read_manuscript('", gsub("\\", "\\\\", original, fixed = T), "')"),
         "```",
         "")
action <- tempfile("action", fileext = ".Rmd")
writeLines(lnz, con = action)

test_that("warnings when manuscript has already been loaded", {
  out <- rmarkdown::render(action, quiet = TRUE)
  tmp <- readLines(out)
  expect_false(any(grepl("A manuscript has already been loaded", tmp)))
})
}
