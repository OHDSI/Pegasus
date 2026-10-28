# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Pegasus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Reads the text of a file a user hands to the New Phenotype chat. The file is only read, never copied or
# kept: the caller deletes the upload afterwards, and nothing here logs a file name or content.

uploadExtensions <- c("pdf", "docx", "pptx", "xlsx", "csv")

# Office files are zip archives; an archive that expands far beyond this is refused rather than read.
maxUncompressedBytes <- 100 * 1024^2

fileExtension <- function(name) {
  tolower(tools::file_ext(name))
}

# One member of a zip archive, parsed as XML.
readArchiveXml <- function(path, member, size) {
  connection <- unz(path, member, open = "rb")
  on.exit(close(connection))
  xml2::read_xml(readBin(connection, "raw", n = size))
}

# The paragraphs (w:p or a:p) of a Word or PowerPoint XML part, each the text runs (w:t or a:t) joined. Elements
# are matched by local name, so the namespace prefix does not matter.
paragraphTexts <- function(document, paragraph, run) {
  paragraphs <- xml2::xml_find_all(document, sprintf("//*[local-name()='%s']", paragraph))
  texts <- vapply(paragraphs, function(node) {
    paste(xml2::xml_text(xml2::xml_find_all(node, sprintf(".//*[local-name()='%s']", run))), collapse = "")
  }, "")
  texts[nzchar(trimws(texts))]
}

extractDocx <- function(path, members) {
  if (!"word/document.xml" %in% members$Name) {
    stop("This does not look like a Word document.")
  }
  size <- members$Length[members$Name == "word/document.xml"]
  paste(paragraphTexts(readArchiveXml(path, "word/document.xml", size), "p", "t"), collapse = "\n")
}

extractPptx <- function(path, members) {
  slides <- members[grepl("^ppt/slides/slide[0-9]+\\.xml$", members$Name), , drop = FALSE]
  if (nrow(slides) == 0) {
    stop("This does not look like a PowerPoint presentation.")
  }
  slides <- slides[order(as.integer(gsub("\\D", "", slides$Name))), , drop = FALSE]
  parts <- vapply(seq_len(nrow(slides)), function(i) {
    lines <- paragraphTexts(readArchiveXml(path, slides$Name[i], slides$Length[i]), "p", "t")
    paste(c(sprintf("--- Slide %d ---", i), lines), collapse = "\n")
  }, "")
  paste(parts, collapse = "\n\n")
}

extractXlsx <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Reading Excel files needs the readxl package, which is not installed.")
  }
  sheets <- readxl::excel_sheets(path)
  parts <- vapply(sheets, function(sheet) {
    data <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
    rows <- apply(data, 1, function(row) paste(ifelse(is.na(row), "", row), collapse = "\t"))
    paste(c(sprintf("--- Sheet: %s ---", sheet), rows[nzchar(trimws(rows))]), collapse = "\n")
  }, "")
  paste(parts, collapse = "\n\n")
}

extractPdf <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Reading PDF files needs the pdftools package, which is not installed.")
  }
  pages <- pdftools::pdf_text(path)
  paste(sprintf("--- Page %d ---\n%s", seq_along(pages), trimws(pages)), collapse = "\n\n")
}

# A CSV is already text; it is passed on as it is.
extractCsv <- function(path, size) {
  bytes <- readBin(path, "raw", n = size)
  if (any(bytes == as.raw(0))) {
    stop("This does not look like a text file.")
  }
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    text <- iconv(text, from = "latin1", to = "UTF-8", sub = "?")
  }
  text
}

# Returns list(text, truncated, characters): the file's text (at most maxChars long), whether it was cut short,
# and how long it was before that. name is the file's original name, which decides the type (a temporary upload
# path has a different name). Stops with a message fit to show the user when the file cannot be used.
extractFileText <- function(path, name = basename(path), maxChars = 60000, maxBytes = 10 * 1024^2) {
  extension <- fileExtension(name)
  if (!extension %in% uploadExtensions) {
    stop(sprintf("Files of this type are not supported. Use %s.", paste0(".", uploadExtensions, collapse = ", ")), call. = FALSE)
  }
  size <- file.size(path)
  if (is.na(size) || size == 0) {
    stop("The file is empty.", call. = FALSE)
  }
  if (size > maxBytes) {
    stop(sprintf("The file is larger than the %d MB limit.", as.integer(maxBytes / 1024^2)), call. = FALSE)
  }
  members <- NULL
  if (extension %in% c("docx", "pptx")) {
    members <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL, warning = function(w) NULL)
    if (is.null(members)) {
      stop("The file could not be read; it may be damaged.", call. = FALSE)
    }
    if (sum(members$Length) > maxUncompressedBytes) {
      stop("The file expands to more than the allowed size.", call. = FALSE)
    }
  }
  text <- tryCatch(
    switch(
      extension,
      docx = extractDocx(path, members),
      pptx = extractPptx(path, members),
      xlsx = extractXlsx(path),
      pdf = extractPdf(path),
      csv = extractCsv(path, size)
    ),
    error = function(e) {
      # Reader errors can quote file content; only this fixed text and our own messages reach the user.
      known <- conditionMessage(e)
      if (grepl("^(This does not look|Reading .* needs the)", known)) {
        stop(known, call. = FALSE)
      }
      stop("The file could not be read; it may be damaged or protected.", call. = FALSE)
    }
  )
  text <- gsub("\r\n?", "\n", text)
  text <- trimws(gsub("\n{3,}", "\n\n", text))
  if (!nzchar(text)) {
    stop(
      if (extension == "pdf") {
        "No text could be read from this PDF. It may be a scan (images of pages), which is not supported."
      } else {
        "No text could be read from this file."
      },
      call. = FALSE
    )
  }
  characters <- nchar(text)
  list(text = substr(text, 1, maxChars), truncated = characters > maxChars, characters = characters)
}
