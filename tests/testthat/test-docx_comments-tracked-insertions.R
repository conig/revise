extract_fixture_comment <- function(body, id = 1L) {
  ns <- 'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"'
  document <- xml2::read_xml(paste0(
    "<w:document ", ns, "><w:body>", body, "</w:body></w:document>"
  ))
  comments <- xml2::read_xml(paste0(
    "<w:comments ", ns, '><w:comment w:id="', id, '">',
    "<w:p><w:r><w:t>Revise::example</w:t></w:r></w:p>",
    "</w:comment></w:comments>"
  ))
  doc <- structure(
    list(doc_obj = list(get = function() document),
         comments = list(get = function() comments)),
    class = "rdocx"
  )
  get_docx_comments(doc)
}

test_that("DOCX extraction keeps the range starting inside a tracked insertion", {
  comments <- extract_fixture_comment('
    <w:p w:paraId="p1">
      <w:r><w:t>Outside the range. </w:t></w:r>
      <w:ins w:id="10">
        <w:commentRangeStart w:id="1"/>
        <w:r><w:t>Inserted </w:t></w:r>
      </w:ins>
      <w:del w:id="11">
        <w:r><w:delText>Deleted </w:delText></w:r>
      </w:del>
      <w:r><w:t>then kept.</w:t></w:r>
      <w:commentRangeEnd w:id="1"/>
      <w:r><w:t> Also outside the range.</w:t></w:r>
    </w:p>')

  expect_identical(comments, list("Revise::example" = "Inserted then kept."))
})

test_that("DOCX extraction preserves paragraph breaks without paragraph IDs", {
  comments <- extract_fixture_comment('
    <w:p>
      <w:r><w:t>Outside the range. </w:t></w:r>
      <w:commentRangeStart w:id="1"/>
      <w:r><w:t>First paragraph.</w:t></w:r>
    </w:p>
    <w:p>
      <w:r><w:t>Second paragraph.</w:t></w:r>
      <w:commentRangeEnd w:id="1"/>
    </w:p>')

  expect_identical(comments[["Revise::example"]], "First paragraph.\n\nSecond paragraph.")
})
