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

# Concept lookups. The vocabulary lives in a separate database (a different dbms than the Pegasus
# tables), so it is queried on its own connection and joined to Pegasus data in R.

queryVocabulary <- function(connection, vocabularySchema, sql, ...) {
  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    vocabulary_schema = vocabularySchema,
    snakeCaseToCamelCase = TRUE,
    ...
  )
}

# Search terms are placed into a LIKE pattern as text, so only plain characters are kept. Quotes, backslashes
# and wildcards are dropped rather than escaped, because escaping rules differ between database platforms.
sanitizeSearchTerm <- function(term) {
  term <- substr(trimws(term), 1, 100)
  term <- gsub("[^A-Za-z0-9 .,()/+-]", "", term)
  # Two hyphens would start a SQL comment.
  gsub("-{2,}", "-", term)
}

emptyConcepts <- function() {
  data.frame(
    conceptId = integer(0),
    conceptName = character(0),
    domainId = character(0),
    vocabularyId = character(0),
    conceptClassId = character(0),
    standardConcept = character(0)
  )
}

searchConcepts <- function(connection, vocabularySchema, term, limit = 100) {
  term <- tolower(sanitizeSearchTerm(term))
  if (!nzchar(term)) {
    return(emptyConcepts())
  }
  sql <- "SELECT TOP @limit concept_id,
    concept_name,
    domain_id,
    vocabulary_id,
    concept_class_id,
    standard_concept
  FROM @vocabulary_schema.concept
  WHERE LOWER(concept_name) LIKE '%@term%'
    AND invalid_reason IS NULL
  ORDER BY CASE WHEN standard_concept = 'S' THEN 0 ELSE 1 END, LENGTH(concept_name), concept_name;"
  queryVocabulary(connection, vocabularySchema, sql, term = term, limit = as.integer(limit))
}

getConcepts <- function(connection, vocabularySchema, conceptIds) {
  if (length(conceptIds) == 0) {
    return(emptyConcepts())
  }
  sql <- "SELECT concept_id,
    concept_name,
    domain_id,
    vocabulary_id,
    concept_class_id,
    standard_concept
  FROM @vocabulary_schema.concept
  WHERE concept_id IN (@concept_ids)
  ORDER BY concept_id;"
  queryVocabulary(connection, vocabularySchema, sql, concept_ids = as.integer(conceptIds))
}

# Includes the concepts themselves (concept_ancestor holds a self row for standard concepts).
getConceptAncestors <- function(connection, vocabularySchema, conceptIds) {
  if (length(conceptIds) == 0) {
    return(integer(0))
  }
  sql <- "SELECT DISTINCT ancestor_concept_id
  FROM @vocabulary_schema.concept_ancestor
  WHERE descendant_concept_id IN (@concept_ids);"
  result <- queryVocabulary(connection, vocabularySchema, sql, concept_ids = as.integer(conceptIds))
  union(as.integer(conceptIds), as.integer(result$ancestorConceptId))
}

# Standard concepts a (source) concept maps to.
getMappedConcepts <- function(connection, vocabularySchema, conceptId) {
  sql <- "SELECT DISTINCT concept_id_2
  FROM @vocabulary_schema.concept_relationship
  WHERE concept_id_1 = @concept_id
    AND relationship_id = 'Maps to'
    AND invalid_reason IS NULL
    AND concept_id_2 <> concept_id_1;"
  as.integer(queryVocabulary(connection, vocabularySchema, sql, concept_id = as.integer(conceptId))$conceptId2)
}

emptyItems <- function() {
  data.frame(
    conceptSetId = integer(0),
    conceptId = integer(0),
    isExcluded = integer(0),
    includeDescendants = integer(0),
    includeMapped = integer(0)
  )
}

getConceptSetItems <- function(connection, databaseSchema, conceptIds) {
  if (length(conceptIds) == 0) {
    return(emptyItems())
  }
  sql <- "SELECT concept_set_id,
    concept_id,
    is_excluded,
    include_descendants,
    include_mapped
  FROM @database_schema.concept_set_item
  WHERE concept_id IN (@concept_ids);"
  queryPegasus(connection, databaseSchema, sql, concept_ids = as.integer(conceptIds))
}

getPhenotypesForConceptSets <- function(connection, databaseSchema, conceptSetIds) {
  if (length(conceptSetIds) == 0) {
    return(data.frame(
      phenotypeId = integer(0), phenotypeName = character(0), conceptSetId = integer(0),
      conceptSetName = character(0), pertains = character(0), category = character(0)
    ))
  }
  sql <- "SELECT p.phenotype_id,
    p.phenotype_name,
    cs.concept_set_id,
    cs.concept_set_name,
    pcs.pertains,
    pcs.category
  FROM @database_schema.phenotype_to_concept_set pcs
  INNER JOIN @database_schema.phenotype p
    ON p.phenotype_id = pcs.phenotype_id
  INNER JOIN @database_schema.concept_set cs
    ON cs.concept_set_id = pcs.concept_set_id
  WHERE pcs.concept_set_id IN (@concept_set_ids)
  ORDER BY p.phenotype_name, cs.concept_set_name;"
  queryPegasus(connection, databaseSchema, sql, concept_set_ids = as.integer(conceptSetIds))
}

# Decides which concept sets contain a concept, following the Circe rules: an item covers the concept when
# it is the concept itself, an ancestor with "include descendants", or (with "include mapped") a concept the
# concept maps to. A concept set is excluded when an excluded item covers the concept.
# ancestorIds must include the concept itself. Returns one row per matching concept set.
matchConceptSetItems <- function(items, conceptId, ancestorIds, mappedIds = integer(0), mappedAncestorIds = integer(0)) {
  empty <- data.frame(conceptSetId = integer(0), itemConceptId = integer(0), matchType = character(0))
  if (nrow(items) == 0) {
    return(empty)
  }
  direct <- items$conceptId == conceptId
  descendant <- items$includeDescendants == 1 & items$conceptId %in% ancestorIds
  mapped <- items$includeMapped == 1 &
    (items$conceptId %in% mappedIds | (items$includeDescendants == 1 & items$conceptId %in% mappedAncestorIds))
  matchType <- ifelse(direct, "Direct", ifelse(descendant, "Descendant", ifelse(mapped, "Mapped", NA_character_)))
  covers <- !is.na(matchType)
  excludedSets <- unique(items$conceptSetId[covers & items$isExcluded == 1])
  keep <- covers & items$isExcluded == 0 & !(items$conceptSetId %in% excludedSets)
  if (!any(keep)) {
    return(empty)
  }
  matched <- data.frame(
    conceptSetId = items$conceptSetId[keep],
    itemConceptId = items$conceptId[keep],
    matchType = matchType[keep]
  )
  # Report each concept set once, by its strongest match.
  matched <- matched[order(match(matched$matchType, c("Direct", "Descendant", "Mapped"))), , drop = FALSE]
  matched <- matched[!duplicated(matched$conceptSetId), , drop = FALSE]
  rownames(matched) <- NULL
  matched
}

# Finds the phenotypes whose concept sets contain a concept. Without a vocabulary connection only
# concept sets listing the concept itself can be found.
findPhenotypesByConcept <- function(connection, databaseSchema, conceptId, vocabularyConnection = NULL, vocabularySchema = NULL) {
  conceptId <- as.integer(conceptId)
  if (is.null(vocabularyConnection)) {
    ancestorIds <- conceptId
    mappedIds <- integer(0)
    mappedAncestorIds <- integer(0)
  } else {
    ancestorIds <- getConceptAncestors(vocabularyConnection, vocabularySchema, conceptId)
    mappedIds <- getMappedConcepts(vocabularyConnection, vocabularySchema, conceptId)
    mappedAncestorIds <- getConceptAncestors(vocabularyConnection, vocabularySchema, mappedIds)
  }
  items <- getConceptSetItems(
    connection, databaseSchema, unique(c(conceptId, ancestorIds, mappedIds, mappedAncestorIds))
  )
  matched <- matchConceptSetItems(items, conceptId, ancestorIds, mappedIds, mappedAncestorIds)
  phenotypes <- getPhenotypesForConceptSets(connection, databaseSchema, matched$conceptSetId)
  result <- merge(phenotypes, matched, by = "conceptSetId")
  result <- result[order(result$phenotypeName, result$conceptSetName), , drop = FALSE]
  rownames(result) <- NULL
  result
}
