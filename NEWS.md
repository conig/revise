# revise 0.1.1

- Addressed an error where inline evaluations could not be completed due to objects not being found in the expected environment.
- Default arguments of `get_revision()` evaluate R code from the main manuscript. This can be risky if users did not write the manuscript themselves and are therefore unaware of the code being evaluated. To reduce this risk, `trust_manuscript` arguments have been added to `get_revision()` and `evaluate_inline()`. By default, these retrieve the `revise_trust_manuscript` option, which returns FALSE when not supplied. When `trust_manuscript = FALSE`, the presence of inline R code together with `evaluate = TRUE` results in an error that prints the code to be evaluated and prompts the user to explicitly state whether they trust the manuscript. This approach preserves default behaviour, and users can address the change with a single line, while also encouraging users to consider the risks of evaluating code from the main manuscript file if they have not written it themselves.

# revise 0.1.0

- First CRAN release
