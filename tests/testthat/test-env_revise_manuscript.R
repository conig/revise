if (rmarkdown::pandoc_available("2.0")){
lnz <- c("---", "format: \"github_document\"", "---", "", "<span id = \"SpanTest\">",
         "", "## Heading", "", "Maecenas mollis consectetur purus. Ut ultrices metus in mauris congue ultricies. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Integer pulvinar non nisi in tristique. Nam euismod nibh et mauris bibendum pellentesque.",
         "", "Suspendisse vulputate, lacus vel finibus placerat, tortor nulla fermentum ipsum, sit amet facilisis mauris velit at lectus. Nam quis libero eget eros luctus vehicula a sit amet quam. Morbi varius augue a augue posuere, vitae tempor tellus scelerisque. Fusce sed rhoncus felis. Morbi lorem odio, egestas at mollis nec, bibendum ac magna.",
         "", "</span>")

original <- tempfile("original", fileext = ".Rmd")
writeLines(lnz, con = original)

lnz <- c("---", "format: \"github_document\"", "---", "",
            "```{r}",
         "library(revise)",
            paste0("read_manuscript('", gsub("\\", "\\\\", original, fixed = T), "')"),
            "```",
            "`r revise::get_revision(\"SpanTest\")`",
            "```",
            "")
action <- tempfile("action", fileext = ".Rmd")
writeLines(lnz, con = action)

out <- rmarkdown::render(action, quiet = TRUE)
tmp <- readLines(out)

test_that("get_revision uses .revise_manuscript", {
  expect_true(any(grepl("Maecenas", tmp)))
})
}
