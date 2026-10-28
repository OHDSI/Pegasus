# Builds a minimal Office-style zip archive from named XML parts.
makeArchive <- function(extension, parts) {
  folder <- tempfile("archive-")
  dir.create(folder)
  for (member in names(parts)) {
    dir.create(dirname(file.path(folder, member)), recursive = TRUE, showWarnings = FALSE)
    writeLines(parts[[member]], file.path(folder, member), useBytes = TRUE)
  }
  archive <- tempfile(fileext = paste0(".", extension))
  previous <- setwd(folder)
  on.exit(setwd(previous))
  zip::zip(archive, files = unique(sub("/.*", "", names(parts))), mode = "mirror")
  archive
}

wordXml <- function(...) {
  paragraphs <- vapply(list(...), function(text) sprintf("<w:p><w:r><w:t>%s</w:t></w:r></w:p>", text), "")
  sprintf('<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>%s</w:body></w:document>',
          paste(paragraphs, collapse = ""))
}

slideXml <- function(...) {
  paragraphs <- vapply(list(...), function(text) sprintf("<a:p><a:r><a:t>%s</a:t></a:r></a:p>", text), "")
  sprintf('<p:sld xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:cSld>%s</p:cSld></p:sld>',
          paste(paragraphs, collapse = ""))
}

test_that("a Word document gives its paragraphs", {
  path <- makeArchive("docx", list("word/document.xml" = wordXml("Asthma is a chronic disease.", "", "Second paragraph.")))
  on.exit(unlink(path))
  result <- extractFileText(path, "request.docx")
  expect_equal(result$text, "Asthma is a chronic disease.\nSecond paragraph.")
  expect_false(result$truncated)
})

test_that("a PowerPoint presentation gives its slides in order", {
  path <- makeArchive("pptx", list(
    "ppt/slides/slide2.xml" = slideXml("Second slide"),
    "ppt/slides/slide10.xml" = slideXml("Tenth slide"),
    "ppt/slides/slide1.xml" = slideXml("Title", "Subtitle")
  ))
  on.exit(unlink(path))
  text <- extractFileText(path, "deck.pptx")$text
  expect_equal(text, paste(
    "--- Slide 1 ---\nTitle\nSubtitle", "--- Slide 2 ---\nSecond slide", "--- Slide 3 ---\nTenth slide", sep = "\n\n"
  ))
})

test_that("a CSV file is passed on as text, and Latin-1 is converted", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c("code,name", "J45,Asthma"), path)
  expect_equal(extractFileText(path, "codes.csv")$text, "code,name\nJ45,Asthma")
  writeBin(c(charToRaw("name\ncaf"), as.raw(0xe9)), path)
  expect_equal(extractFileText(path, "codes.csv")$text, "name\ncafé")
})

test_that("the type comes from the original name, not the temporary path", {
  path <- tempfile(fileext = ".bin")
  on.exit(unlink(path))
  writeLines("a,b", path)
  expect_equal(extractFileText(path, "Codes.CSV")$text, "a,b")
  expect_error(extractFileText(path, "notes.txt"), "not supported")
})

test_that("long text is cut at the limit and says so", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(strrep("abcdefghij", 10), path)
  result <- extractFileText(path, "long.csv", maxChars = 25)
  expect_equal(nchar(result$text), 25)
  expect_true(result$truncated)
  expect_equal(result$characters, 100)
})

test_that("unusable files give a clear message and no file content", {
  empty <- tempfile(fileext = ".csv")
  blank <- tempfile(fileext = ".csv")
  binary <- tempfile(fileext = ".csv")
  broken <- tempfile(fileext = ".docx")
  big <- tempfile(fileext = ".csv")
  on.exit(unlink(c(empty, blank, binary, broken, big)))
  file.create(empty)
  writeLines(c("", "  "), blank)
  writeBin(as.raw(c(1, 2, 0, 3)), binary)
  writeLines("SECRET-CONTENT is not a zip", broken)
  writeLines(strrep("x", 2000), big)

  expect_error(extractFileText(empty, "a.csv"), "empty")
  expect_error(extractFileText(blank, "a.csv"), "No text could be read")
  expect_error(extractFileText(binary, "a.csv"), "does not look like a text file")
  expect_error(extractFileText(big, "a.csv", maxBytes = 1024), "larger than")
  message <- tryCatch(extractFileText(broken, "a.docx"), error = conditionMessage)
  expect_match(message, "could not be read")
  expect_false(grepl("SECRET", message))
})

test_that("a Word file without a document part is refused", {
  path <- makeArchive("docx", list("other.xml" = "<a/>"))
  on.exit(unlink(path))
  expect_error(extractFileText(path, "a.docx"), "does not look like a Word document")
})

test_that("an Excel workbook gives every sheet as tab-separated rows", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))
  writexl::write_xlsx(list(Codes = data.frame(code = c("J45", "J46"), name = c("Asthma", NA))), path)
  expect_equal(extractFileText(path, "codes.xlsx")$text, "--- Sheet: Codes ---\ncode\tname\nJ45\tAsthma\nJ46")
})

test_that("a PDF gives the text of its pages", {
  skip_if_not_installed("pdftools")
  # A one-page PDF written by hand (without a cross-reference table, which the reader rebuilds).
  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path))
  writeLines(c(
    "%PDF-1.4",
    "1 0 obj<</Type/Catalog/Pages 2 0 R>>endobj",
    "2 0 obj<</Type/Pages/Kids[3 0 R]/Count 1>>endobj",
    "3 0 obj<</Type/Page/Parent 2 0 R/MediaBox[0 0 300 100]/Contents 4 0 R/Resources<</Font<</F1 5 0 R>>>>>>endobj",
    "4 0 obj<</Length 44>>stream",
    "BT /F1 12 Tf 10 50 Td (Asthma definition) Tj ET",
    "endstream endobj",
    "5 0 obj<</Type/Font/Subtype/Type1/BaseFont/Helvetica>>endobj",
    "trailer<</Root 1 0 R>>"
  ), path)
  expect_match(extractFileText(path, "request.pdf")$text, "Asthma definition")
})
