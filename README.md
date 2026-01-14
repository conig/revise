
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

# Starting a revision document

You can use rmd or docx files with revise. To load either, simply call
revise::read_manuscript(“your_file.rmd”)

For example. Here is an example docx where content has been tagged with
comment boxes. E.g, Revise::hard

``` r
docx_path <- system.file("examples/word_test.docx", package = "revise")

man <- read_manuscript(docx_path)
man
#> 
#> ── <Manuscript> ──
#> 
#> • 6 sections
#> ✖ No PDF attached
```

We can see all tagged sections by running

``` r
names(man$sections)
#> [1] "track_changes" "section"       "multi_lines"   "bullets"      
#> [5] "hard"          "numbered"
```

Then we can retrieve using a section name

``` r
revision <- get_revision("hard", man)
revision
#> [1] ">Hard example\n>\n>This section has all features:\n>\n>* Bullet1\n>\n>* Bullet2\n>\n>* Bullet3\n>\n>Some more text is here."
```

This content can then be put directly into a response document using
single backticks.

# Tagging sections in a manuscript for extraction

There are two methods to tag sections in manuscript for extraction. We
support using id attributes in html spans:

``` md
<span id="hard">
This is a multi-line secion of text.

We have tagged it with the section name "multi-line-example".
</span>
```

We also support a markdown variant for quick inline tagging:

``` md
[This is a single line of text we have tagged using markdown.]{#single-line-example}
```

In both cases pandoc ignores these tags when rendering the document, but
they can be used by revise to extract content.
