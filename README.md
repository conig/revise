
<!-- README.md is generated from README.Rmd. Please edit that file -->

# revise

<!-- badges: start -->

[![R-CMD-check](https://github.com/conig/revise/workflows/R-CMD-check/badge.svg)](https://github.com/conig/revise/actions)
<!-- badges: end -->

The goal of revise is to support authors in responding to revise and
resubmit requests. These functions are designed to work with
[crsh/papaja](https://github.com/crsh/papaja).

## Installation

You can install the development version of {revise} from
[GitHub](https://github.com/conig/revise) with:

``` r
# install.packages("remotes")
remotes::install_github("conig/revise")
```

## Examples

### ReviewerComment addin

We have included an addin so you can mark text as a reviewer comment. If
a YAML header is not detected in document the addin will add it. Fields
will automatically be filled in if there is a .rmd file in the working
directory. Additionally, selected text will be marked as a reviewer
comment by including it within an “asis” chunk. Text below this chunk
will be tagged as an author response. A hotkey can be set for this addin
(e.g. Ctrl+Shift+R).

![animation of ReviewerComment
addin](man/figures/README/ReviewerComment.gif)
