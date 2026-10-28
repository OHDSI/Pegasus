createTestModule <- function(pool, userName = "tester") {
  phenotypes <- reactiveVal(withConnection(pool, function(connection) getPhenotypes(connection, "main")))
  refreshPhenotypes <- function() {
    phenotypes(withConnection(pool, function(connection) getPhenotypes(connection, "main")))
  }
  list(
    args = list(
      connectionPool = pool,
      databaseSchema = "main",
      llmConfig = list(),
      systemPrompt = "",
      userName = userName,
      phenotypes = phenotypes,
      refreshPhenotypes = refreshPhenotypes
    ),
    phenotypes = phenotypes
  )
}

test_that("Save button stores the phenotype and refreshes the cache", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  module <- createTestModule(pool)

  testServer(createServer, args = module$args, {
    session$setInputs(saveName = "Asthma", definition = "Chronic airway disease.")
    session$setInputs(save = 1)
  })
  expect_equal(isolate(module$phenotypes())$phenotypeName, "Asthma")
  expect_equal(isolate(module$phenotypes())$creator, "tester")
})

test_that("Saving is blocked without a user", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  module <- createTestModule(pool, userName = NULL)
  module$args["userName"] <- list(NULL)

  testServer(createServer, args = module$args, {
    session$setInputs(saveName = "Asthma", definition = "Chronic airway disease.")
    session$setInputs(save = 1)
  })
  expect_equal(nrow(isolate(module$phenotypes())), 0)
})

test_that("Start refuses a duplicate name", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  withConnection(pool, function(connection) {
    savePhenotype(connection, "main", "Asthma", "Chronic airway disease.", "bob")
  })
  module <- createTestModule(pool)

  testServer(createServer, args = module$args, {
    session$setInputs(name = "asthma", start = 1)
    expect_null(client)
  })
})

test_that("Saving a refinement links the new phenotype to its source and clears the clone", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  parentId <- withConnection(pool, function(connection) {
    savePhenotype(connection, "main", "Asthma", "Chronic airway disease.", "bob")
  })
  module <- createTestModule(pool)
  cloneSource <- reactiveVal(list(id = parentId, name = "Asthma", definition = "Chronic airway disease."))
  module$args$cloneSource <- cloneSource

  testServer(createServer, args = module$args, {
    session$setInputs(saveName = "Asthma v2", definition = "Chronic airway disease, clinician diagnosed.")
    session$setInputs(save = 1)
  })

  lineage <- withConnection(pool, function(connection) {
    DatabaseConnector::querySql(connection, "SELECT * FROM main.phenotype_ancestor")
  })
  expect_equal(nrow(lineage), 1)
  expect_equal(lineage$PARENT_PHENOTYPE_ID, parentId)
  expect_equal(lineage$PHENOTYPE_ID, parentId + 1)
  expect_null(isolate(cloneSource()))
})

test_that("Saving a brand-new phenotype records no lineage", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  module <- createTestModule(pool)

  testServer(createServer, args = module$args, {
    session$setInputs(saveName = "Asthma", definition = "Chronic airway disease.")
    session$setInputs(save = 1)
  })
  lineage <- withConnection(pool, function(connection) {
    DatabaseConnector::querySql(connection, "SELECT * FROM main.phenotype_ancestor")
  })
  expect_equal(nrow(lineage), 0)
})

test_that("Refine this hands the selected phenotype to the wizard callback", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- reactiveVal(withConnection(pool, function(connection) getPhenotypeSummary(connection, "main")))
  refined <- NULL

  testServer(
    phenotypeDetailServer,
    args = list(
      connectionPool = pool,
      databaseSchema = "main",
      phenotypes = phenotypes,
      phenotypeId = reactiveVal(1L),
      shortlist = reactiveVal(integer(0)),
      navigate = function(page = NULL, id = NULL) NULL,
      onRefine = function(id) refined <<- id
    ),
    {
      session$setInputs(refine = 1)
    }
  )
  expect_equal(refined, 1L)
})

test_that("an uploaded file is read, then deleted, and a bad file only gives a message", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  module <- createTestModule(pool)

  good <- tempfile(fileext = ".csv")
  writeLines(c("code,name", "J45,Asthma"), good)
  bad <- tempfile(fileext = ".exe")
  writeLines("not supported", bad)

  testServer(createServer, args = module$args, {
    session$setInputs(upload0 = list(name = "codes.csv", datapath = good, size = file.size(good), type = "text/csv"))
    expect_equal(pendingAttachment()$name, "codes.csv")
    expect_equal(pendingAttachment()$text, "code,name\nJ45,Asthma")
    expect_false(file.exists(good))
    expect_equal(uploadCounter(), 1)

    session$setInputs(upload1 = list(name = "tool.exe", datapath = bad, size = file.size(bad), type = ""))
    expect_null(pendingAttachment())
    expect_false(file.exists(bad))

    session$setInputs(upload2 = list(name = "again.csv", datapath = good, size = 0, type = ""))
    session$setInputs(removeAttachment = 1)
    expect_null(pendingAttachment())
  })
})
