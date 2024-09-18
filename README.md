
<!-- README.md is generated from README.Rmd. Please edit that file -->

# revise

<!-- badges: start -->

[![R-CMD-check](https://github.com/conig/revise/workflows/R-CMD-check/badge.svg)](https://github.com/conig/revise/actions)
<!-- badges: end -->

The goal of revise is to support authors in responding to revise and
resubmit requests. These functions are designed to work with
[crsh/papaja](https://github.com/crsh/papaja).

The core functionality allows you to extract text from a Rmd or docx, and insert it into the RNR letter.
This allows you to avoid having to update the RnR letter, every time you, or a co-author makes a change to the manuscript.

## Installation

You can install the development version of {revise} from
[GitHub](https://github.com/conig/revise) with:

``` r
# install.packages("remotes")
remotes::install_github("conig/revise")
```

# Revision letter template

Revise comes with a template revision letter `RNR-letter.pdf`.
This Rmarkdown template builds off the template provided by papaja.
However it differes in how reviewer chunks are handled. Instead of \RC{}, it uses `asis` markdown chunks.

For example: 

```{asis}

```{asis}
This is a review comment

```

And this is our response.

```


This allows for us to do some R processing on chunks before rendering them in the letter, allowing us to support PDF, docx, and txt outputs.

To change the RNR-letter to docx or txt. just change the extension in the output field.

E.g.,

```yaml
---
output: RNR-letter.docx
---
```

# 


# Loading a manuscript

You can use rmd or docx files with revise. To load either, simply call
revise::read_manuscript(“your_file.rmd”)

For example. Here is an example docx where content has been tagged with
comment boxes. E.g, Revise::hard

``` r
docx_path <- system.file("examples/word_test.docx", package = "revise")

man <- read_manuscript(docx_path)
man
#> <Manuscript>
#> [34m- 6 sections[39m
#> [31mNo PDF attached[39m
```

We can see all tagged sections by running

``` r
names(man$sections)
#> [1] "track_changes" "section"       "multi_lines"   "bullets"       "hard"          "numbered"
```

Then we can retrieve using a section name

``` r
revision <- get_revision("hard", man)
revision
#> [1] ">Hard example\n>\n>This section has all features:\n>\n>* Bullet1\n>\n>* Bullet2\n>\n>* Bullet3\n>\n>Some more text is here."
```

This content can then be put directly into a response document using
single backticks.
