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

# Finds the stored concept sets that a cohort definition uses by reading the ConceptSets inside its Circe JSON.
# cohort_definition_to_concept_set is the proper link, but data that was migrated from elsewhere may not have
# it filled in. A concept set of the cohort is matched to a stored concept set when both list the same items
# (concept, exclusion, descendants and mapped flags), or, failing that, when they have the same name.

matchCohortConceptSets <- function(connection, databaseSchema, circeJson, matchByName = TRUE) {
  circe <- tryCatch(jsonlite::fromJSON(circeJson, simplifyVector = FALSE), error = function(e) NULL)
  cohortSets <- circe$ConceptSets
  if (length(cohortSets) == 0) {
    return(integer(0))
  }
  names <- queryPegasus(
    connection, databaseSchema,
    "SELECT concept_set_id, concept_set_name FROM @database_schema.concept_set;"
  )
  matched <- integer(0)
  for (cohortSet in cohortSets) {
    items <- circeItemsTable(cohortSet$expression$items)
    found <- integer(0)
    if (nrow(items) > 0) {
      # Only sets that contain one of the concepts can be identical, so compare just those.
      candidates <- queryPegasus(
        connection, databaseSchema,
        "SELECT DISTINCT concept_set_id FROM @database_schema.concept_set_item WHERE concept_id IN (@concept_ids);",
        concept_ids = unique(items$conceptId)
      )$conceptSetId
      if (length(candidates) > 0) {
        stored <- queryPegasus(
          connection, databaseSchema,
          "SELECT concept_set_id, circe_concept_set_expression FROM @database_schema.concept_set
          WHERE concept_set_id IN (@concept_set_ids);",
          concept_set_ids = as.integer(candidates)
        )
        signature <- conceptSetSignature(items)
        same <- vapply(stored$circeConceptSetExpression, function(expression) {
          parsed <- tryCatch(jsonlite::fromJSON(expression, simplifyVector = FALSE), error = function(e) NULL)
          identical(conceptSetSignature(circeItemsTable(parsed$items)), signature)
        }, TRUE, USE.NAMES = FALSE)
        found <- stored$conceptSetId[same]
      }
    }
    if (matchByName && length(found) == 0 && !is.null(cohortSet$name)) {
      found <- names$conceptSetId[normalizeName(names$conceptSetName) == normalizeName(as.character(cohortSet$name))]
    }
    matched <- c(matched, found)
  }
  as.integer(unique(matched))
}

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
