test_that("savePhenotype inserts rows with sequential IDs and quotes intact", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))

  withConnection(pool, function(connection) {
    savePhenotype(connection, "main", " Acute liver injury ", "It's a 'liver'; DROP TABLE x;", "alice")
    savePhenotype(connection, "main", "Asthma", "Chronic airway disease.", "bob")
    phenotypes <- getPhenotypes(connection, "main")

    expect_equal(nrow(phenotypes), 2)
    expect_equal(sort(phenotypes$phenotypeId), c(1, 2))
    row <- phenotypes[phenotypes$phenotypeName == "Acute liver injury", ]
    expect_equal(row$clinicalDefinition, "It's a 'liver'; DROP TABLE x;")
    expect_equal(row$creator, "alice")
  })
})

test_that("savePhenotype rejects duplicate (case-insensitive) and empty input", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))

  withConnection(pool, function(connection) {
    savePhenotype(connection, "main", "Asthma", "Chronic airway disease.", "bob")
    expect_error(savePhenotype(connection, "main", " ASTHMA", "x", "bob"), "already exists")
    expect_error(savePhenotype(connection, "main", "  ", "x", "bob"), "name cannot be empty")
    expect_error(savePhenotype(connection, "main", "COPD", " ", "bob"), "definition cannot be empty")
  })
})

test_that("findSimilarNames finds approximate matches", {
  existing <- c("Acute liver injury", "Asthma", "Drug-induced liver injury")
  expect_setequal(findSimilarNames("liver injury", existing), c("Acute liver injury", "Drug-induced liver injury"))
  expect_length(findSimilarNames("Psoriasis", existing), 0)
  expect_length(findSimilarNames("Psoriasis", character(0)), 0)
})
