make_test_manuscript <- function(section, pdf_pages, page_ids = seq_along(pdf_pages)) {
  list(
    sections = list(target = section),
    PDF = data.frame(
      page_id = page_ids,
      text = pdf_pages,
      stringsAsFactors = FALSE
    ),
    refs = list(
      tables = data.frame(
        text = character(),
        ref = character(),
        stringsAsFactors = FALSE
      ),
      figures = data.frame(
        text = character(),
        ref = character(),
        stringsAsFactors = FALSE
      )
    )
  )
}

test_that("get_revision falls back cleanly when split anchors miss", {
  section <- paste(
    "SDT is an influential general theory of human motivation, development, and wellness,",
    "widely used across psychology, education, health, and organizational research [@RyanSelfdeterminationTheory2017; @ReeveTeachersFacilitators2006; @NtoumaMetaanalysisSelfdetermination2021].",
    "Within SDT, motivation varies in both amount and quality, ranging from more controlled to more autonomous forms [@RyanSelfdeterminationTheory2017].",
    "Research applying SDT to substance use is growing, and has primarily drawn from three of SDT mini-theories [@HercheApplicationSelfDetermination2024].",
    "Work using _Causality Orientations Theory_ has linked more controlled orientations to greater risk from drinking [@DukesRoleCausality2022].",
    "Research drawing on Organismic Integration Theory has shown that motivation quality for both drinking and drinking responsibly is associated with drinking patterns and harm-reduction behavior, with more self-determined motivations tending to correlate with more adaptive outcomes and amotivation with greater risk [@CourtnAssessingSelfdetermined2025; @RicharComprehensiveExamination2022; @RicharComprehensiveExamination2023].",
    "Studies have also used _Basic Psychological Needs Theory_ to show that need-related processes are implicated in both risky drinking and engagement in protective or harm-reduction behaviors, including motivations for responsible drinking and the use of protective behavioral strategies [@RicharProfilesMotivations2020; @RicharComprehensiveExamination2022]."
  )

  manuscript <- make_test_manuscript(
    section,
    pdf_pages = c(
      "unrelated preface page",
      paste(
        "sdt is an influential general theory of human motivation development and wellness widely used across psychology education health and organizational research",
        "within sdt motivation varies in both amount and quality ranging from more controlled to more autonomous forms",
        "research applying sdt to substance use is growing and has primarily drawn from three of sdt mini theories",
        "work using causality orientations theory has linked more controlled orientations to greater risk from drinking",
        "research drawing on organismic integration theory has shown that motivation quality for both drinking and drinking responsibly is associated with drinking patterns and harm reduction behavior with more self determined motivations tending to correlate with more adaptive outcomes and amotivation with greater risk",
        "studies have also used basic psychological needs theory to show that need related processes are implicated in both risky drinking and engagement in protective or harm reduction behaviors including motivations for responsible drinking and the use of protective behavioral strategies"
      ),
      "unrelated appendix page"
    ),
    page_ids = c(4, 5, 6)
  )

  out <- get_revision(
    "target",
    manuscript = manuscript,
    quote = FALSE,
    evaluate = FALSE
  )

  expect_match(out, "Pg\\. 5")
  expect_false(grepl("Pg\\.\\s*\\\\end\\{flushright\\}", out))
})

test_that("get_pdf_pagenumber returns character(0) when no page matches", {
  pdf <- data.frame(
    page_id = 1:2,
    text = c("alpha beta gamma", "delta epsilon zeta"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    get_pdf_pagenumber("this text is not present", pdf_text = pdf),
    character(0)
  )
})

test_that("get_revision formats page ranges with double hyphens", {
  section <- paste(
    rep(
      "This section starts on one page and continues onto the next page without any markup artifacts.",
      6
    ),
    collapse = " "
  )

  pdf_pages <- c(
    paste(
      rep(
        "this section starts on one page and continues onto the next page without any markup artifacts",
        3
      ),
      collapse = " "
    ),
    paste(
      rep(
        "this section starts on one page and continues onto the next page without any markup artifacts",
        3
      ),
      collapse = " "
    )
  )

  manuscript <- make_test_manuscript(section, pdf_pages)
  out <- get_revision(
    "target",
    manuscript = manuscript,
    quote = FALSE,
    evaluate = FALSE,
    search_length = 80
  )

  expect_match(out, "Pg\\. 1--2")
})

test_that("an excerpt with no searchable text gets an unknown page", {
  manuscript <- make_test_manuscript("[@citation]", c("main text", "appendix"))
  expect_warning(
    output <- get_revision("target", manuscript, split_string = TRUE),
    "Couldn't match the extracted text"
  )
  expect_match(output, "Pg. ?", fixed = TRUE)
})

test_that("a complete page match takes precedence over a repeated opening", {
  section <- "We asked participants about their experiences. Only the second page contains these results."
  manuscript <- make_test_manuscript(section, c(
    "We asked participants about their experiences. This page describes the pilot.",
    section
  ))
  out <- get_revision("target", manuscript, search_length = 20)
  expect_match(out, "Pg. 2\\end", fixed = TRUE)
})

test_that("a one-page PDF can be used to cite an excerpt", {
  pdf <- tempfile(fileext = ".pdf")
  path <- tempfile(fileext = ".Rmd")
  writeLines("[Only one page of results.]{#results}", path)
  grDevices::pdf(pdf)
  graphics::plot.new()
  graphics::text(.5, .5, "Only one page of results.")
  grDevices::dev.off()

  manuscript <- read_manuscript(path, PDF = pdf, to_envir = FALSE)
  expect_match(get_revision("results", manuscript), "Pg. 1\\end", fixed = TRUE)
})

test_that("printed headers are literal text and blank PDF pages keep their IDs", {
  pdf <- tempfile(fileext = ".pdf")
  path <- tempfile(fileext = ".Rmd")
  writeLines("[Final observations.]{#results}", path)
  grDevices::pdf(pdf)
  for (i in 1:5) {
    graphics::plot.new()
    if (i == 3) next
    graphics::text(.5, .9, paste("Study [draft", i))
    graphics::text(.5, .5, if (i == 5) "Final observations." else "Other results.")
  }
  grDevices::dev.off()

  manuscript <- read_manuscript(path, PDF = pdf, to_envir = FALSE)
  expect_identical(manuscript$PDF$page_id, 1:5)
  expect_identical(manuscript$PDF$text[3], "")
  expect_false(any(grepl("draft", manuscript$PDF$text)))
  expect_match(get_revision("results", manuscript), "Pg. 5\\end", fixed = TRUE)
})

test_that("PDF = TRUE changes only the final extension of the manuscript path", {
  dir <- tempfile(pattern = "md-project-")
  dir.create(dir)
  path <- file.path(dir, "study.md")
  writeLines("[A revised passage.]{#tag}", path)
  grDevices::pdf(file.path(dir, "study.pdf"))
  graphics::plot.new()
  graphics::text(.5, .5, "A revised passage.")
  grDevices::dev.off()
  man <- read_manuscript(path, PDF = TRUE, to_envir = FALSE)
  expect_match(get_revision("tag", man), "Pg. 1\\end", fixed = TRUE)
})

test_that("PDF lookup distinguishes revised numbers and whole words", {
  man <- make_test_manuscript("We retained 278 responses.", c(
    "We retained 143 responses. This is a scatterplot.",
    "We retained 278 responses. The cat returned."
  ))
  expect_match(get_revision("target", man), "Pg. 2\\end", fixed = TRUE)
  man$sections$target <- "cat"
  expect_match(get_revision("target", man), "Pg. 2\\end", fixed = TRUE)
})

test_that("separated PDF anchors cite both ends, and minor typos can still match", {
  man <- make_test_manuscript("An opening phrase and a closing observation", c(
    "An opening phrase", "A figure and its caption", "a closing observation"
  ))
  expect_match(get_revision("target", man, search_length = 14), "Pg. 1--3\\end", fixed = TRUE)
  man$sections$target <- "A closing observatoin"
  expect_match(get_revision("target", man), "Pg. 3\\end", fixed = TRUE)
})

test_that("printed page numbers do not interrupt a short cross-page excerpt", {
  dir <- tempfile()
  dir.create(dir)
  path <- file.path(dir, "paper.md")
  writeLines("[We met.]{#tag}", path)
  grDevices::pdf(file.path(dir, "paper.pdf"))
  for (i in 1:2) {
    graphics::plot.new()
    graphics::text(.5, .5, c("We", "met.")[i])
    graphics::text(.5, .1, as.character(i))
  }
  grDevices::dev.off()
  man <- read_manuscript(path, PDF = TRUE, to_envir = FALSE)
  expect_match(get_revision("tag", man), "Pg. 1--2\\end", fixed = TRUE)
})

test_that("different numeric results on the first line are not mistaken for a running head", {
  path <- tempfile(fileext = ".md")
  pdf <- tempfile(fileext = ".pdf")
  writeLines("[We retained 278 responses.]{#sample}", path)
  grDevices::pdf(pdf)
  for (n in c(143, 278)) {
    graphics::plot.new()
    graphics::text(.5, .9, paste("We retained", n, "responses."))
    graphics::text(.5, .6, "Results follow.")
  }
  grDevices::dev.off()
  m <- read_manuscript(path, PDF = pdf, to_envir = FALSE)
  expect_match(get_revision("sample", m), "Pg. 2\\end", fixed = TRUE)
})
