# revise 0.2.0

- New feature: Fenced markdown sections can now be used to extract manuscript text for larger chunks.
- New feature: `revise_letter_pdf()` now supports a `comment_reset_by_section` argument (including via YAML) so reviewer comments can be numbered continuously across reviewer sections when desired.
- Fix: `revise_letter_docx()` now numbers reviewer comments sequentially based on chunk order (matching PDF behavior) even when chunk labels are descriptive tags.
- Fix: DOCX revision letters now convert `\\comment{...}` and `\\Comment{...}` references into plain-text comment references, instead of leaving unresolved LaTeX-style macros.
- Fix: `get_revision()` now sanitizes leaked tag artifacts from overlapping/crossing markup (e.g., `]{#tag}`, stray `{#tag}`, and span wrappers), so extracted text is cleaner in both PDF and DOCX outputs.
- Fix: Addressed an error where inline evaluations could not be completed due to objects not being found in the expected environment.
- Change: Default arguments of `get_revision()` evaluate R code from the main manuscript. This can be risky if users did not write the manuscript themselves and are therefore unaware of the code being evaluated. To reduce this risk, `trust_manuscript` arguments have been added to `get_revision()` and `evaluate_inline()`. By default, these retrieve the `revise_trust_manuscript` option, which returns FALSE when not supplied. When `trust_manuscript = FALSE`, the presence of inline R code together with `evaluate = TRUE` results in an error that prints the code to be evaluated and prompts the user to explicitly state whether they trust the manuscript. This approach preserves default behaviour, and users can address the change with a single line, while also encouraging users to consider the risks of evaluating code from the main manuscript file if they have not written it themselves.

# revise 0.1.0

- First CRAN release
