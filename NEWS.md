# revise 0.1.1

## Manuscript excerpts

- Inline R now requires explicit trust. After reviewing the manuscript, pass
  `trust_manuscript = TRUE` to `get_revision()` or set
  `options(revise_trust_manuscript = TRUE)`. Set `evaluate = FALSE` to insert
  the excerpt without running code. Inline R uses the environment supplied to
  `get_revision()`.
- Tag longer Markdown passages with fenced divs such as `::: {#methods}`.
  Extraction cleans up stray markup from nested or overlapping tags.
- Rereading a registered manuscript replaces the previous copy without dropping
  other manuscripts. Headings containing punctuation are formatted literally.
- PDF page lookup is faster and preserves page numbers in one-page PDFs and
  PDFs with blank pages. It distinguishes numbers in the text from printed
  page numbers and matches whole words. An exact match for the full excerpt takes precedence
  over repeated phrases. If no match is found, the PDF letter shows `Pg. ?` and
  `get_revision()` warns.
- Word excerpts keep tracked insertions and omit tracked deletions.

## Revision letters

- Set `comment_reset_by_section = FALSE` to number PDF reviewer comments
  continuously across sections.
- DOCX reviewer comments are numbered in order on each render.
  `\comment{label}` and `\Comment{label}` resolve to plain-text references,
  including references to later comments.

## Dependencies

- Removed required imports of `dplyr`, `worcs`, and `cli`. Google Drive
  comments reuse the local Word reader, so `docxtractr` is no longer needed.

# revise 0.1.0

- First CRAN release.
