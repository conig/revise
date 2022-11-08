# Unexported function from papaja

escape_latex <- function (x, newlines = FALSE, spaces = FALSE)
{
    x = gsub("\\\\", "\\\\textbackslash", x)
    x = gsub("([#$%&_{}])", "\\\\\\1", x)
    x = gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
    x = gsub("~", "\\\\textasciitilde{}", x)
    x = gsub("\\^", "\\\\textasciicircum{}", x)
    if (newlines)
        x = gsub("(?<!\n)\n(?!\n)", "\\\\\\\\", x, perl = TRUE)
    if (spaces)
        x = gsub("(?<= ) ", "\\\\ ", x, perl = TRUE)
    x
}
