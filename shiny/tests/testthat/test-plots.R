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
  expect_equal(orderedLevels(c("user", "zeta", "template", NA, "alpha"), sourceTypeOrder), c("template", "user", "alpha", "zeta"))
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
    evaluationType = "phevaluator",
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
  expect_setequal(vapply(bySource, function(trace) trace$name, ""), c("template", "ai", "user"))

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
  full <- colorOf(buildScatter(data, colorBy = "source"), "user")
  filtered <- colorOf(buildScatter(data[data$paSourceType %in% c("user", "ai"), ], colorBy = "source"), "user")
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
  expect_length(builtTraces(empty), 0)
})

test_that("buildComparison makes a PPV and a sensitivity panel for every grouping", {
  skip_if_not_installed("plotly")
  data <- seedEvaluations()
  for (groupBy in c("database", "source", "evaluationType")) {
    expect_length(builtTraces(buildComparison(data, groupBy)), 2)
  }
  expect_length(builtTraces(buildComparison(emptyEvaluations())), 0)
})
