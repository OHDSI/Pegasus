builtTraces <- function(plot) {
  plotly::plotly_build(plot)$x$data
}

seedEvaluations <- function() {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  withConnection(pool, function(connection) getEvaluations(connection, "main"))
}

test_that("getEvaluations returns all evaluations or one phenotype's", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  all <- withConnection(pool, function(connection) getEvaluations(connection, "main"))
  expect_equal(nrow(all), 17)
  expect_named(all, names(emptyEvaluations()), ignore.order = TRUE)

  asthma <- withConnection(pool, function(connection) getEvaluations(connection, "main", 1))
  expect_equal(nrow(asthma), 7)
  expect_true(all(asthma$phenotypeId == 1))

  none <- withConnection(pool, function(connection) getEvaluations(connection, "main", 6))
  expect_equal(nrow(none), 0)
})

test_that("orderedLevels puts known levels first in fixed order", {
  expect_equal(orderedLevels(c("User", "zeta", "Templates", NA, "alpha"), sourceTypeOrder), c("Templates", "User", "alpha", "zeta"))
  expect_equal(orderedLevels(character(0), sourceTypeOrder), character(0))
})

test_that("assignSeries keys colors to a fixed order and folds the tail into Other", {
  order <- c("A", "B", "C", "D", "E")
  result <- assignSeries(c("C", "A", "E", "Z"), order, "Other databases")
  expect_equal(result$slot, c(3, 1, NA, NA))
  expect_equal(result$label, c("C", "A", "Other databases", "Other databases"))

  # Filtering the data must not change an entity's slot.
  expect_equal(assignSeries("C", order)$slot, 3)
})

test_that("hoverText escapes untrusted names", {
  data <- data.frame(
    phenotypeName = "<a href='x'>Evil</a>",
    cohortDefinitionName = "cohort & more",
    cdmSourceAbbreviation = "DB",
    evaluationType = "PheValuator",
    ppv = 0.9,
    sensitivity = 0.8,
    cohortSubjects = 1234
  )
  text <- hoverText(data)
  expect_false(grepl("<a ", text, fixed = TRUE))
  expect_match(text, "&lt;a href", fixed = TRUE)
  expect_match(text, "1,234 subjects", fixed = TRUE)
  expect_length(hoverText(emptyEvaluations()), 0)
})

test_that("buildScatter makes one trace per series and folds extra databases into Other", {
  skip_if_not_installed("plotly")
  data <- seedEvaluations()

  bySource <- builtTraces(buildScatter(data, colorBy = "source"))
  expect_setequal(vapply(bySource, function(trace) trace$name, ""), c("Templates", "MonoFlow", "User"))

  byEvaluationType <- builtTraces(buildScatter(data, colorBy = "evaluationType"))
  expect_length(byEvaluationType, 3)

  byDatabase <- builtTraces(buildScatter(data, colorBy = "database"))
  expect_length(byDatabase, 3)

  manyDatabases <- builtTraces(buildScatter(data, colorBy = "database", databaseOrder = c("DEVEHR", "DEVREG", "X1", "DEVCLM")))
  expect_setequal(vapply(manyDatabases, function(trace) trace$name, ""), c("DEVEHR", "DEVREG", "Other databases"))
})

test_that("buildScatter keeps colors stable when the data is filtered", {
  skip_if_not_installed("plotly")
  data <- seedEvaluations()
  colorOf <- function(plot, name) {
    traces <- builtTraces(plot)
    trace <- traces[[which(vapply(traces, function(t) t$name, "") == name)]]
    trace$marker$color
  }
  full <- colorOf(buildScatter(data, colorBy = "source"), "User")
  filtered <- colorOf(buildScatter(data[data$cdSourceType %in% c("User", "MonoFlow"), ], colorBy = "source"), "User")
  expect_equal(full, filtered)
})

test_that("buildScatter adds error bars only when asked and handles empty data", {
  skip_if_not_installed("plotly")
  data <- seedEvaluations()

  without <- builtTraces(buildScatter(data))
  expect_null(without[[1]]$error_x)
  withCi <- builtTraces(buildScatter(data, showCi = TRUE))
  expect_false(is.null(withCi[[1]]$error_x))

  empty <- buildScatter(emptyEvaluations())
  expect_true(all(vapply(builtTraces(empty), function(trace) length(trace$x) == 0, TRUE)))
  expect_equal(empty$x$source, "performanceScatter")
})

test_that("values outside the built-in lists still get their own colors and rows without PPV are dropped", {
  skip_if_not_installed("plotly")
  data <- seedEvaluations()
  data$cdSourceType <- c("Human", "Machine")[1 + (seq_len(nrow(data)) %% 2)]
  data$evaluationType <- c("Alpha", "Beta")[1 + (seq_len(nrow(data)) %% 2)]
  for (colorBy in c("source", "evaluationType")) {
    traces <- builtTraces(buildScatter(data, colorBy = colorBy))
    expect_length(traces, 2)
    expect_false(any(vapply(traces, function(trace) trace$marker$color == otherColor, TRUE)))
  }

  data$ppv[1:2] <- NA
  traces <- builtTraces(buildScatter(data, colorBy = "source"))
  expect_equal(sum(vapply(traces, function(trace) length(trace$x), 1L)), nrow(data) - 2)
})

test_that("buildComparison makes a PPV and a sensitivity panel for every grouping", {
  skip_if_not_installed("plotly")
  data <- seedEvaluations()
  for (groupBy in c("database", "source", "evaluationType")) {
    expect_length(builtTraces(buildComparison(data, groupBy)), 2)
  }
  expect_true(all(vapply(builtTraces(buildComparison(emptyEvaluations())), function(trace) length(trace$x) == 0, TRUE)))
})

test_that("filterEvaluations restricts by phenotype, database, type, source and thresholds", {
  data <- seedEvaluations()
  allTypes <- orderedLevels(data$evaluationType, evaluationTypeOrder)
  allSources <- orderedLevels(data$cdSourceType, sourceTypeOrder)
  filterWith <- function(databases = character(0), phenotypeIds = character(0), minPpv = 0, minSensitivity = 0,
                         evalTypes = allTypes, sources = allSources) {
    filterEvaluations(data, evalTypes, sources, databases, phenotypeIds, minPpv, minSensitivity)
  }

  expect_equal(nrow(filterWith()), 17)
  expect_equal(nrow(filterWith(phenotypeIds = "1")), 7)
  expect_equal(nrow(filterWith(phenotypeIds = c("1", "2"))), 12)
  expect_equal(nrow(filterWith(phenotypeIds = "6")), 0)
  expect_true(all(filterWith(databases = "DEVEHR")$cdmSourceAbbreviation == "DEVEHR"))
  expect_equal(nrow(filterWith(phenotypeIds = "1", databases = "DEVREG")), 1)
  expect_true(all(filterWith(minPpv = 90)$ppv >= 0.9))
  expect_equal(nrow(filterWith(evalTypes = character(0))), 0)
  expect_true(all(filterWith(sources = "MonoFlow")$cdSourceType == "MonoFlow"))
})

test_that("data colors come from the OHDSI plot palette", {
  palette <- c("#336B91", "#69AED5", "#11A08A", "#FBC511", "#EB6622", "#1F425A", "#F18A1D", "#FBC50F")
  expect_true(all(c(seriesColors, otherColor) %in% palette))
  expect_equal(anyDuplicated(c(seriesColors, otherColor)), 0)
})
