test_that("shortlist helpers add, remove and toggle without duplicates", {
  shortlist <- reactiveVal(integer(0))
  isolate({
    expect_equal(addToShortlist(shortlist, c(3, 1, 3, NA)), 2)
    expect_equal(shortlist(), c(3L, 1L))
    expect_equal(addToShortlist(shortlist, c(1, 2)), 1)
    expect_setequal(shortlist(), c(1L, 2L, 3L))

    removeFromShortlist(shortlist, c(1, 99))
    expect_setequal(shortlist(), c(2L, 3L))

    expect_false(toggleShortlist(shortlist, 2))
    expect_equal(shortlist(), 3L)
    expect_true(toggleShortlist(shortlist, 2))
    expect_setequal(shortlist(), c(2L, 3L))

    removeFromShortlist(shortlist, c(2, 3))
    expect_length(shortlist(), 0)
  })
})

test_that("shortlistButton reflects membership and stops row selection", {
  html <- shortlistButton("browse-toggleShortlist", c(5, 6), c(TRUE, FALSE))
  expect_match(html[1], "Remove", fixed = TRUE)
  expect_match(html[2], "Add", fixed = TRUE)
  expect_match(html[1], "event.stopPropagation()", fixed = TRUE)
  expect_match(html[2], "Shiny.setInputValue('browse-toggleShortlist', 6", fixed = TRUE)
  expect_length(shortlistButton("x", integer(0), logical(0)), 0)
})

test_that("csvSafe neutralises formula characters and leaves other values alone", {
  data <- data.frame(a = c("=1+1", "+x", "@y", "-5", "plain", NA), b = 1:6)
  safe <- csvSafe(data)
  expect_equal(safe$a, c("'=1+1", "'+x", "'@y", "'-5", "plain", NA))
  expect_equal(safe$b, 1:6)
})

test_that("safeFileName produces file-system safe names", {
  expect_equal(safeFileName("Asthma - template"), "Asthma_template")
  expect_equal(safeFileName("../../etc/passwd"), "etc_passwd")
  expect_equal(safeFileName("???"), "cohort")
  expect_lte(nchar(safeFileName(strrep("a", 200))), 80)
})

test_that("export tables contain only the shortlisted phenotypes with snake_case columns", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- withConnection(pool, function(connection) getPhenotypeSummary(connection, "main"))
  evaluations <- withConnection(pool, function(connection) getEvaluations(connection, "main"))

  table <- shortlistPhenotypeTable(phenotypes, c(1L, 6L))
  expect_equal(sort(table$phenotype_id), c(1, 6))
  expect_true(all(c("phenotype_name", "clinical_definition", "status", "best_ppv", "best_sensitivity") %in% names(table)))
  expect_equal(table$status[table$phenotype_id == 6], "Concept sets only")

  evalTable <- shortlistEvaluationTable(evaluations, c(1L, 6L))
  expect_equal(nrow(evalTable), 7)
  expect_true(all(c("phenotype_id", "cdm_source_abbreviation", "ppv_lb", "evaluation_type") %in% names(evalTable)))
  expect_equal(nrow(shortlistEvaluationTable(evaluations, integer(0))), 0)
})

test_that("getCohortExpressions and writeCohortZip export Circe JSON with a manifest", {
  skip_if_not_installed("zip")
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  cohorts <- withConnection(pool, function(connection) getCohortExpressions(connection, "main", c(1L, 2L)))
  expect_equal(nrow(cohorts), 5)
  expect_equal(nrow(withConnection(pool, function(connection) getCohortExpressions(connection, "main", integer(0)))), 0)

  file <- tempfile(fileext = ".zip")
  on.exit(unlink(file), add = TRUE)
  writeCohortZip(cohorts, file)
  files <- zip::zip_list(file)$filename
  expect_true("manifest.csv" %in% files)
  expect_true("1_Asthma_template.json" %in% files)
  expect_equal(sum(grepl("\\.json$", files)), 5)

  folder <- tempfile()
  zip::unzip(file, exdir = folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  expect_equal(nrow(utils::read.csv(file.path(folder, "manifest.csv"))), 5)
  expect_match(paste(readLines(file.path(folder, "1_Asthma_template.json")), collapse = ""), "cohortName", fixed = TRUE)
})

test_that("browse server toggles single phenotypes and adds all shown rows to the shortlist", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- reactiveVal(withConnection(pool, function(connection) getPhenotypeSummary(connection, "main")))
  shortlist <- reactiveVal(integer(0))

  testServer(
    browseServer,
    args = list(
      connectionPool = pool,
      databaseSchema = "main",
      phenotypes = phenotypes,
      refreshPhenotypes = function() NULL,
      selectedId = reactiveVal(NULL),
      shortlist = shortlist
    ),
    {
      session$setInputs(status = statusLevels, creator = NULL)
      session$setInputs(toggleShortlist = 1)
      session$setInputs(toggleShortlist = 2)
      expect_setequal(shortlist(), c(1L, 2L))
      session$setInputs(toggleShortlist = 1)
      expect_equal(shortlist(), 2L)

      session$setInputs(table_rows_all = c(1, 3))
      session$setInputs(addAll = 1)
      expect_setequal(shortlist(), c(2L, filtered()$phenotypeId[c(1, 3)]))
    }
  )
})
