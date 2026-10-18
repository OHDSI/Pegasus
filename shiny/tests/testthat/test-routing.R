test_that("makeRoute accepts known pages with positive whole-number IDs only", {
  expect_equal(makeRoute("phenotype", 5), list(page = "phenotype", id = 5L))
  expect_equal(makeRoute("cohort_definition", "12"), list(page = "cohort_definition", id = 12L))
  expect_equal(makeRoute("concept_set", 100000), list(page = "concept_set", id = 100000L))
  expect_null(makeRoute("unknown", 1))
  expect_null(makeRoute("phenotype", 0))
  expect_null(makeRoute("phenotype", -3))
  expect_null(makeRoute("phenotype", 2.5))
  expect_null(makeRoute("phenotype", "abc"))
  expect_null(makeRoute("phenotype", "1; DROP TABLE"))
  expect_null(makeRoute("phenotype", NULL))
  expect_null(makeRoute(NULL, 1))
  expect_null(makeRoute(c("phenotype", "concept_set"), 1))
  expect_null(makeRoute("phenotype", NA))
})

test_that("parseRoute reads the page and ID from a query string", {
  expect_equal(parseRoute("?page=phenotype&id=5"), list(page = "phenotype", id = 5L))
  expect_equal(parseRoute("?id=7&page=cohort_definition"), list(page = "cohort_definition", id = 7L))
  expect_equal(parseRoute("?page=concept_set&id=42&other=1"), list(page = "concept_set", id = 42L))
  expect_null(parseRoute(""))
  expect_null(parseRoute("?"))
  expect_null(parseRoute(NULL))
  expect_null(parseRoute("?page=phenotype"))
  expect_null(parseRoute("?id=5"))
  expect_null(parseRoute("?page=admin&id=5"))
  expect_null(parseRoute("?page=phenotype&id=five"))
})

test_that("routeQuery and parseRoute round-trip", {
  for (page in routePages) {
    route <- makeRoute(page, 33)
    expect_equal(parseRoute(routeQuery(route)), route)
  }
  expect_equal(routeQuery(NULL), "?")
  expect_equal(routeQuery(makeRoute("phenotype", 5)), "?page=phenotype&id=5")
})

test_that("entityLink builds a real URL, carries the page and ID, and escapes the label", {
  link <- entityLink("cohort_definition", c(3, NA), c("A <b>&</b>", "skipped"))
  expect_match(link[1], 'href="?page=cohort_definition&amp;id=3"', fixed = TRUE)
  expect_match(link[1], 'data-page="cohort_definition"', fixed = TRUE)
  expect_match(link[1], 'data-id="3"', fixed = TRUE)
  expect_match(link[1], "A &lt;b&gt;&amp;&lt;/b&gt;", fixed = TRUE)
  expect_equal(link[2], "")
  expect_equal(entityLink("nothing", 1, "x"), "")
  expect_length(entityLink("phenotype", integer(0), character(0)), 0)

  list <- as.character(entityLinkList("phenotype", c(1, 2), c("One", "Two")))
  expect_match(list, "One</a>, <a", fixed = TRUE)
  expect_equal(as.character(entityLinkList("phenotype", integer(0), character(0))), "none")
})

test_that("copyLinkButton refers to the page it is on", {
  html <- as.character(copyLinkButton(makeRoute("concept_set", 9)))
  expect_match(html, "pegasusCopyLink('concept_set', 9, this)", fixed = TRUE)
})

test_that("the navigator changes the route, records history once and ignores invalid targets", {
  route <- reactiveVal(NULL)
  urls <- character(0)
  shown <- 0
  navigate <- createNavigator(
    route,
    updateUrl = function(query) urls <<- c(urls, query),
    showBrowse = function() shown <<- shown + 1
  )
  isolate({
    navigate("phenotype", 5)
    expect_equal(route(), list(page = "phenotype", id = 5L))
    expect_equal(urls, "?page=phenotype&id=5")
    expect_equal(shown, 1)

    # Same page again: no new history entry, but the Browse tab is still brought forward.
    navigate("phenotype", 5)
    expect_equal(urls, "?page=phenotype&id=5")
    expect_equal(shown, 2)

    navigate("cohort_definition", 12)
    expect_equal(route()$page, "cohort_definition")
    expect_equal(urls, c("?page=phenotype&id=5", "?page=cohort_definition&id=12"))

    # Coming from the browser's own history: the route changes but no entry is added.
    navigate("concept_set", 3, push = FALSE)
    expect_equal(route()$page, "concept_set")
    expect_length(urls, 2)

    navigate("bogus", 1)
    navigate("phenotype", "x")
    expect_equal(route()$page, "concept_set")
    expect_length(urls, 2)

    navigate()
    expect_null(route())
    expect_equal(urls[3], "?")
  })
})

test_that("copyTextButton names the element whose text it copies", {
  html <- as.character(copyTextButton("cohort-jsonText"))
  expect_match(html, "pegasusCopyText('cohort-jsonText', this)", fixed = TRUE)
  expect_match(html, "Copy to clipboard", fixed = TRUE)
  expect_match(routingScript, "function pegasusCopyText", fixed = TRUE)
})
