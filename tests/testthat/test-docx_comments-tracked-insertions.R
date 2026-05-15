test_that("get_docx_comments keeps text when a comment starts inside tracked insertion", {
  document_xml <- paste(
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '  <w:body>',
    '    <w:p w:paraId="p1">',
    '      <w:r><w:t xml:space="preserve">Intro. </w:t></w:r>',
    '      <w:ins w:id="53">',
    '        <w:r><w:t xml:space="preserve"> </w:t></w:r>',
    '        <w:commentRangeStart w:id="54"/>',
    '        <w:r><w:t xml:space="preserve">Alpha beta </w:t></w:r>',
    '        <w:r><w:t>gamma</w:t></w:r>',
    '        <w:r><w:t xml:space="preserve"> delta.</w:t></w:r>',
    '      </w:ins>',
    '      <w:r><w:t xml:space="preserve"> Epsilon zeta.</w:t></w:r>',
    '      <w:commentRangeEnd w:id="54"/>',
    '      <w:r><w:commentReference w:id="54"/></w:r>',
    '    </w:p>',
    '  </w:body>',
    '</w:document>',
    sep = "\n"
  )

  comments_xml <- paste(
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '  <w:comment w:id="54" w:author="A" w:date="2026-04-12T21:18:00Z" w:initials="A">',
    '    <w:p>',
    '      <w:r><w:t>Revise::placeholder_tag</w:t></w:r>',
    '    </w:p>',
    '  </w:comment>',
    '</w:comments>',
    sep = "\n"
  )

  fake_docx <- structure(
    list(
      doc_obj = list(get = function() xml2::read_xml(document_xml)),
      comments = list(get = function() xml2::read_xml(comments_xml))
    ),
    class = "rdocx"
  )

  comments <- revise:::get_docx_comments(fake_docx)

  expect_equal(comments$comment_id[[1]], "54")
  expect_equal(unlist(comments$text[[1]]), "Revise::placeholder_tag")
  expect_equal(comments$commented_text[[1]], "Alpha beta gamma delta. Epsilon zeta.")
})

test_that("get_docx_comments keeps inserted text and skips deleted text in comment ranges", {
  document_xml <- paste(
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '  <w:body>',
    '    <w:p w:paraId="p2">',
    '      <w:r><w:t xml:space="preserve">Intro. </w:t></w:r>',
    '      <w:ins w:id="10">',
    '        <w:r><w:t xml:space="preserve"> </w:t></w:r>',
    '        <w:commentRangeStart w:id="55"/>',
    '        <w:r><w:t>Alpha</w:t></w:r>',
    '      </w:ins>',
    '      <w:del w:id="11">',
    '        <w:r><w:delText xml:space="preserve"> old text </w:delText></w:r>',
    '      </w:del>',
    '      <w:ins w:id="12">',
    '        <w:r><w:t xml:space="preserve"> beta</w:t></w:r>',
    '      </w:ins>',
    '      <w:r><w:t xml:space="preserve"> gamma.</w:t></w:r>',
    '      <w:commentRangeEnd w:id="55"/>',
    '      <w:r><w:commentReference w:id="55"/></w:r>',
    '    </w:p>',
    '  </w:body>',
    '</w:document>',
    sep = "\n"
  )

  comments_xml <- paste(
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '  <w:comment w:id="55" w:author="A" w:date="2026-04-12T21:18:00Z" w:initials="A">',
    '    <w:p>',
    '      <w:r><w:t>Revise::placeholder_tag_2</w:t></w:r>',
    '    </w:p>',
    '  </w:comment>',
    '</w:comments>',
    sep = "\n"
  )

  fake_docx <- structure(
    list(
      doc_obj = list(get = function() xml2::read_xml(document_xml)),
      comments = list(get = function() xml2::read_xml(comments_xml))
    ),
    class = "rdocx"
  )

  comments <- revise:::get_docx_comments(fake_docx)

  expect_equal(comments$comment_id[[1]], "55")
  expect_equal(unlist(comments$text[[1]]), "Revise::placeholder_tag_2")
  expect_equal(comments$commented_text[[1]], "Alpha beta gamma.")
})

test_that("get_docx_comments falls back to xml path when paragraph ids are absent", {
  document_xml <- paste(
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '  <w:body>',
    '    <w:p>',
    '      <w:r><w:t>Alpha </w:t></w:r>',
    '      <w:commentRangeStart w:id="56"/>',
    '      <w:r><w:t>beta</w:t></w:r>',
    '      <w:r><w:t> gamma</w:t></w:r>',
    '      <w:commentRangeEnd w:id="56"/>',
    '      <w:r><w:commentReference w:id="56"/></w:r>',
    '    </w:p>',
    '  </w:body>',
    '</w:document>',
    sep = "\n"
  )

  comments_xml <- paste(
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '  <w:comment w:id="56" w:author="A" w:date="2026-04-22T02:00:00Z" w:initials="A">',
    '    <w:p>',
    '      <w:r><w:t>Revise::placeholder_tag_3</w:t></w:r>',
    '    </w:p>',
    '  </w:comment>',
    '</w:comments>',
    sep = "\n"
  )

  fake_docx <- structure(
    list(
      doc_obj = list(get = function() xml2::read_xml(document_xml)),
      comments = list(get = function() xml2::read_xml(comments_xml))
    ),
    class = "rdocx"
  )

  comments <- revise:::get_docx_comments(fake_docx)

  expect_equal(comments$comment_id[[1]], "56")
  expect_equal(unlist(comments$text[[1]]), "Revise::placeholder_tag_3")
  expect_equal(comments$commented_text[[1]], "beta gamma")
})
