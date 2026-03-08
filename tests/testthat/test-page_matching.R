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
