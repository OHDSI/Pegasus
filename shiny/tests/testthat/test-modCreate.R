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
