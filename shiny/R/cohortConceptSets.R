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

# Concept sets are compared by content: the same distinct items (concept, excluded, descendants, mapped).
# Identical content under different IDs and names can exist, because concept sets are stored per definition.

# Summary rows (the columns of getCohortConceptSets) for the given concept set IDs.
getConceptSetSummaries <- function(connection, databaseSchema, conceptSetIds) {
  if (length(conceptSetIds) == 0) {
    return(data.frame(
      conceptSetId = integer(0), conceptSetName = character(0), csSourceType = character(0),
      valid = integer(0), hasDescendants = integer(0), atlasConceptSetId = integer(0)
    ))
  }
  queryPegasus(
    connection, databaseSchema,
    "SELECT concept_set_id,
      concept_set_name,
      cs_source_type,
      valid,
      has_descendants,
      atlas_concept_set_id
    FROM @database_schema.concept_set
    WHERE concept_set_id IN (@concept_set_ids)
    ORDER BY concept_set_name;",
    concept_set_ids = as.integer(conceptSetIds)
  )
}

# ---- Matching many cohorts at once, inside the database ----

# The concept sets of the given cohort definitions (rows: cohortDefinitionId, circeCohortExpression), flattened:
# one row per concept set in `sets` and one row per distinct item in `items`. A set's position in its cohort
# identifies it, because the IDs inside Circe JSON mean nothing outside of that cohort.
flattenCohortConceptSets <- function(cohorts) {
  sets <- list()
  items <- list()
  for (i in seq_len(nrow(cohorts))) {
    circe <- tryCatch(jsonlite::fromJSON(cohorts$circeCohortExpression[i], simplifyVector = FALSE), error = function(e) NULL)
    for (position in seq_along(circe$ConceptSets)) {
      conceptSet <- circe$ConceptSets[[position]]
      setItems <- unique(circeItemsTable(conceptSet$expression$items))
      name <- conceptSet$name
      sets[[length(sets) + 1]] <- data.frame(
        cohortDefinitionId = cohorts$cohortDefinitionId[i],
        setIndex = position,
        name = if (is.null(name)) NA_character_ else as.character(name),
        itemCount = nrow(setItems),
        stringsAsFactors = FALSE
      )
      if (nrow(setItems) > 0) {
        items[[length(items) + 1]] <- cbind(
          cohortDefinitionId = as.integer(cohorts$cohortDefinitionId[i]), setIndex = as.integer(position), setItems
        )
      }
    }
  }
  list(
    sets = if (length(sets) > 0) do.call(rbind, sets) else data.frame(
      cohortDefinitionId = integer(0), setIndex = integer(0), name = character(0), itemCount = integer(0)
    ),
    items = if (length(items) > 0) do.call(rbind, items) else data.frame(
      cohortDefinitionId = integer(0), setIndex = integer(0), conceptId = integer(0), isExcluded = integer(0),
      includeDescendants = integer(0), includeMapped = integer(0)
    )
  )
}

# Two concept sets have the same content when they have the same distinct items (concept, excluded, descendants,
# mapped). The cohort items are uploaded to a temporary table and compared with concept_set_item in one query:
# a pair matches when every item of the cohort's set is in the stored set and the two have equally many items.
# Returns the links (cohortDefinitionId, conceptSetId), the matches (which concept set in a cohort's JSON matched
# which stored set, and how) and a report of the totals.
#   matchByName: also match sets that have no content match by name (ignoring case).
#   onDuplicates: when several stored sets have identical content, link "all" of them or only the "lowest" ID.
matchCohortConceptSetsInDatabase <- function(connection, databaseSchema, cohorts, matchByName = TRUE,
                                             onDuplicates = c("all", "lowest")) {
  onDuplicates <- match.arg(onDuplicates)
  flat <- flattenCohortConceptSets(cohorts)
  sets <- flat$sets
  setKey <- paste(sets$cohortDefinitionId, sets$setIndex)

  byContent <- data.frame(cohortDefinitionId = integer(0), setIndex = integer(0), conceptSetId = integer(0))
  if (nrow(flat$items) > 0) {
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = "cohort_set_item",
      data = flat$items,
      tempTable = TRUE,
      dropTableIfExists = TRUE,
      createTable = TRUE,
      camelCaseToSnakeCase = TRUE,
      progressBar = FALSE
    )
    on.exit(
      DatabaseConnector::renderTranslateExecuteSql(
        connection, "DROP TABLE IF EXISTS #cohort_set_item;", progressBar = FALSE, reportOverallTime = FALSE
      ),
      add = TRUE
    )
    sql <- "SELECT m.cohort_definition_id, m.set_index, m.concept_set_id
    FROM (
      SELECT c.cohort_definition_id, c.set_index, s.concept_set_id, COUNT(*) AS matched_items
      FROM #cohort_set_item c
      INNER JOIN (
        SELECT DISTINCT concept_set_id, concept_id, is_excluded, include_descendants, include_mapped
        FROM @database_schema.concept_set_item
      ) s
        ON s.concept_id = c.concept_id
        AND s.is_excluded = c.is_excluded
        AND s.include_descendants = c.include_descendants
        AND s.include_mapped = c.include_mapped
      GROUP BY c.cohort_definition_id, c.set_index, s.concept_set_id
    ) m
    INNER JOIN (
      SELECT cohort_definition_id, set_index, COUNT(*) AS item_count
      FROM #cohort_set_item
      GROUP BY cohort_definition_id, set_index
    ) a
      ON a.cohort_definition_id = m.cohort_definition_id AND a.set_index = m.set_index
    INNER JOIN (
      SELECT concept_set_id, COUNT(*) AS item_count
      FROM (
        SELECT DISTINCT concept_set_id, concept_id, is_excluded, include_descendants, include_mapped
        FROM @database_schema.concept_set_item
      ) distinct_items
      GROUP BY concept_set_id
    ) b
      ON b.concept_set_id = m.concept_set_id
    WHERE m.matched_items = a.item_count
      AND m.matched_items = b.item_count;"
    byContent <- queryPegasus(connection, databaseSchema, sql)
  }
  ambiguousKeys <- unique(paste(byContent$cohortDefinitionId, byContent$setIndex)[
    duplicated(paste(byContent$cohortDefinitionId, byContent$setIndex))
  ])
  if (onDuplicates == "lowest" && nrow(byContent) > 0) {
    byContent <- byContent[order(byContent$conceptSetId), , drop = FALSE]
    byContent <- byContent[!duplicated(paste(byContent$cohortDefinitionId, byContent$setIndex)), , drop = FALSE]
  }
  contentKeys <- unique(paste(byContent$cohortDefinitionId, byContent$setIndex))

  byName <- data.frame(cohortDefinitionId = integer(0), setIndex = integer(0), conceptSetId = integer(0))
  leftover <- !setKey %in% contentKeys
  if (matchByName && any(leftover & !is.na(sets$name))) {
    stored <- queryPegasus(connection, databaseSchema, "SELECT concept_set_id, concept_set_name FROM @database_schema.concept_set;")
    candidates <- sets[leftover & !is.na(sets$name), , drop = FALSE]
    rows <- lapply(seq_len(nrow(candidates)), function(i) {
      ids <- stored$conceptSetId[normalizeName(stored$conceptSetName) == normalizeName(candidates$name[i])]
      if (length(ids) == 0) NULL else data.frame(
        cohortDefinitionId = candidates$cohortDefinitionId[i], setIndex = candidates$setIndex[i], conceptSetId = ids
      )
    })
    rows <- rows[!vapply(rows, is.null, TRUE)]
    if (length(rows) > 0) {
      byName <- do.call(rbind, rows)
    }
  }
  matchedKeys <- unique(c(contentKeys, paste(byName$cohortDefinitionId, byName$setIndex)))

  links <- unique(rbind(
    data.frame(cohortDefinitionId = byContent$cohortDefinitionId, conceptSetId = byContent$conceptSetId),
    data.frame(cohortDefinitionId = byName$cohortDefinitionId, conceptSetId = byName$conceptSetId)
  ))
  # Which concept set of which cohort (by position and name in its JSON) matched which stored set, and how.
  matches <- rbind(
    data.frame(byContent[, c("cohortDefinitionId", "setIndex", "conceptSetId")], how = rep("content", nrow(byContent))),
    data.frame(byName[, c("cohortDefinitionId", "setIndex", "conceptSetId")], how = rep("name", nrow(byName)))
  )
  matches$name <- sets$name[match(paste(matches$cohortDefinitionId, matches$setIndex), setKey)]
  matches$conceptSetName <- NA_character_
  if (nrow(matches) > 0) {
    storedNames <- getConceptSetSummaries(connection, databaseSchema, unique(matches$conceptSetId))
    matches$conceptSetName <- storedNames$conceptSetName[match(matches$conceptSetId, storedNames$conceptSetId)]
  }
  list(
    links = links[order(links$cohortDefinitionId, links$conceptSetId), , drop = FALSE],
    matches = matches,
    report = list(
      conceptSets = nrow(sets),
      matchedByContent = length(contentKeys),
      matchedByName = length(setdiff(matchedKeys, contentKeys)),
      duplicateContent = length(ambiguousKeys),
      unmatched = sets[!setKey %in% matchedKeys, c("cohortDefinitionId", "setIndex", "name"), drop = FALSE]
    )
  )
}

# Other stored concept sets with exactly the same content (distinct items) as the given one.
getIdenticalConceptSets <- function(connection, databaseSchema, conceptSetId) {
  sql <- "SELECT cs.concept_set_id, cs.concept_set_name, cs.valid
  FROM (
    SELECT s.concept_set_id, COUNT(*) AS matched_items
    FROM (
      SELECT DISTINCT concept_id, is_excluded, include_descendants, include_mapped
      FROM @database_schema.concept_set_item
      WHERE concept_set_id = @concept_set_id
    ) x
    INNER JOIN (
      SELECT DISTINCT concept_set_id, concept_id, is_excluded, include_descendants, include_mapped
      FROM @database_schema.concept_set_item
      WHERE concept_set_id <> @concept_set_id
    ) s
      ON s.concept_id = x.concept_id
      AND s.is_excluded = x.is_excluded
      AND s.include_descendants = x.include_descendants
      AND s.include_mapped = x.include_mapped
    GROUP BY s.concept_set_id
  ) m
  INNER JOIN (
    SELECT concept_set_id, COUNT(*) AS item_count
    FROM (
      SELECT DISTINCT concept_set_id, concept_id, is_excluded, include_descendants, include_mapped
      FROM @database_schema.concept_set_item
    ) distinct_items
    GROUP BY concept_set_id
  ) b
    ON b.concept_set_id = m.concept_set_id
  INNER JOIN @database_schema.concept_set cs
    ON cs.concept_set_id = m.concept_set_id
  WHERE m.matched_items = b.item_count
    AND m.matched_items = (
      SELECT COUNT(*) FROM (
        SELECT DISTINCT concept_id, is_excluded, include_descendants, include_mapped
        FROM @database_schema.concept_set_item
        WHERE concept_set_id = @concept_set_id
      ) own_items
    )
  ORDER BY cs.concept_set_name;"
  queryPegasus(connection, databaseSchema, sql, concept_set_id = as.integer(conceptSetId))
}
