#' reviewer_comment
#'
#' A comment to style chunks for reviewer comments

reviewer_comment <- function(){

  context <- rstudioapi::getActiveDocumentContext()
  #assign("context", context, envir = globalenv())
  contents = context$contents

  selection <- context$select[[1]]$range
  start <- selection$start[[1]]
  end <- selection$end[[1]]

  start_content <- "```{=tex}
\\RC{\\stepcounter{C}\\underline{Comment \\arabic{C}.}
\\vspace{-0.1cm}
"

  end_content <- "
  }
```"

  before <- contents[1:start-1]

  # If a counter has not been started, begin it.

  contains_counter <-
    any(grepl("\\newcounter{C}", context$contents, fixed = TRUE))

  if (!contains_counter & any(grepl("---", contents))) {
    end_yaml <-  max(which(grepl("---", contents)))
    before <-
      c(before[1:end_yaml], "\\newcounter{C}", before[(end_yaml + 1):length(before)])
  }

  middle <- contents[start:end]
  after <- contents[(end+1) : length(contents)]

  new = c(before, start_content, middle, end_content, after)

  rstudioapi::setDocumentContents(paste(new,collapse = "\n"), id = context$id)

}


