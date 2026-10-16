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
# limitations under the License.

withConnection <- function(connectionPool, fun) {
  connection <- pool::poolCheckout(connectionPool)
  on.exit(pool::poolReturn(connection))
  fun(connection)
}

getPhenotypes <- function(connection, databaseSchema) {
  sql <- "SELECT phenotype_id,
    phenotype_name,
    clinical_definition,
    creator,
    time_created
  FROM @database_schema.phenotype
  ORDER BY phenotype_name;"
  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseSchema,
    snakeCaseToCamelCase = TRUE
  )
}

normalizeName <- function(name) {
  tolower(trimws(name))
}

phenotypeNameExists <- function(name, existingNames) {
  normalizeName(name) %in% normalizeName(existingNames)
}

findSimilarNames <- function(name, existingNames) {
  if (length(existingNames) == 0) {
    return(character(0))
  }
  similar <- agrepl(normalizeName(name), normalizeName(existingNames), max.distance = 0.2, fixed = TRUE)
  existingNames[similar]
}

getNextPhenotypeId <- function(connection, databaseSchema) {
  sql <- "SELECT MAX(phenotype_id) AS max_id FROM @database_schema.phenotype;"
  maxId <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseSchema,
    snakeCaseToCamelCase = TRUE
  )$maxId
  if (length(maxId) == 0 || is.na(maxId)) 1 else maxId + 1
}

savePhenotype <- function(connection, databaseSchema, name, clinicalDefinition, creator, parentPhenotypeId = NA_integer_) {
  name <- trimws(name)
  clinicalDefinition <- trimws(clinicalDefinition)
  if (!nzchar(name)) {
    stop("The phenotype name cannot be empty.")
  }
  if (!nzchar(clinicalDefinition)) {
    stop("The clinical definition cannot be empty.")
  }
  existingNames <- getPhenotypes(connection, databaseSchema)$phenotypeName
  if (phenotypeNameExists(name, existingNames)) {
    stop(sprintf("A phenotype named '%s' already exists.", name))
  }
  timeCreated <- Sys.time()
  attr(timeCreated, "tzone") <- "UTC"
  phenotypeId <- getNextPhenotypeId(connection, databaseSchema)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = databaseSchema,
    tableName = "phenotype",
    data = data.frame(
      phenotypeId = as.integer(phenotypeId),
      phenotypeName = name,
      clinicalDefinition = clinicalDefinition,
      creator = creator,
      timeCreated = timeCreated,
      draft = 1L,
      valid = 0L
    ),
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
  if (!is.na(parentPhenotypeId)) {
    DatabaseConnector::insertTable(
      connection = connection,
      databaseSchema = databaseSchema,
      tableName = "phenotype_ancestor",
      data = data.frame(
        phenotypeId = as.integer(phenotypeId),
        parentPhenotypeId = as.integer(parentPhenotypeId)
      ),
      dropTableIfExists = FALSE,
      createTable = FALSE,
      tempTable = FALSE,
      camelCaseToSnakeCase = TRUE,
      progressBar = FALSE
    )
  }
  invisible(phenotypeId)
}

# Status is derived from what exists for a phenotype, in order of precedence.
statusLevels <- c("Evaluated", "Cohort defined", "Concept sets only", "Definition only", "Draft (queued)", "Invalid")

deriveStatus <- function(draft, valid, conceptSetCount, cohortCount, evaluationCount) {
  status <- rep("Definition only", length(draft))
  status[conceptSetCount > 0] <- "Concept sets only"
  status[cohortCount > 0] <- "Cohort defined"
  status[evaluationCount > 0] <- "Evaluated"
  status[draft == 1] <- "Draft (queued)"
  status[draft != 1 & valid == 0] <- "Invalid"
  status
}

emptyPhenotypeSummary <- function() {
  data.frame(
    phenotypeId = integer(0),
    phenotypeName = character(0),
    clinicalDefinition = character(0),
    creator = character(0),
    timeCreated = as.POSIXct(character(0)),
    draft = integer(0),
    valid = integer(0),
    conceptSetCount = integer(0),
    cohortCount = integer(0),
    evaluationCount = integer(0),
    databaseCount = integer(0),
    bestPpv = numeric(0),
    bestSensitivity = numeric(0),
    status = character(0)
  )
}

queryPegasus <- function(connection, databaseSchema, sql, ...) {
  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseSchema,
    snakeCaseToCamelCase = TRUE,
    ...
  )
}

# One row per phenotype, with the counts needed to derive its status.
getPhenotypeSummary <- function(connection, databaseSchema) {
  sql <- "SELECT p.phenotype_id,
    p.phenotype_name,
    p.clinical_definition,
    p.creator,
    p.time_created,
    p.draft,
    p.valid,
    COALESCE(cs.concept_set_count, 0) AS concept_set_count,
    COALESCE(c.cohort_count, 0) AS cohort_count,
    COALESCE(e.evaluation_count, 0) AS evaluation_count,
    COALESCE(e.database_count, 0) AS database_count,
    e.best_ppv,
    e.best_sensitivity
  FROM @database_schema.phenotype p
  LEFT JOIN (
    SELECT phenotype_id, COUNT(DISTINCT concept_set_id) AS concept_set_count
    FROM @database_schema.phenotype_to_concept_set
    GROUP BY phenotype_id
  ) cs ON cs.phenotype_id = p.phenotype_id
  LEFT JOIN (
    SELECT phenotype_id, COUNT(*) AS cohort_count
    FROM @database_schema.cohort_definition
    GROUP BY phenotype_id
  ) c ON c.phenotype_id = p.phenotype_id
  LEFT JOIN (
    SELECT cd.phenotype_id,
      COUNT(*) AS evaluation_count,
      COUNT(DISTINCT ev.database_id) AS database_count,
      MAX(ev.ppv) AS best_ppv,
      MAX(ev.sensitivity) AS best_sensitivity
    FROM @database_schema.cohort_definition cd
    INNER JOIN @database_schema.cohort_definition_evaluation ev
      ON ev.cohort_definition_id = cd.cohort_definition_id
    GROUP BY cd.phenotype_id
  ) e ON e.phenotype_id = p.phenotype_id
  ORDER BY p.phenotype_name;"
  summary <- queryPegasus(connection, databaseSchema, sql)
  summary$status <- deriveStatus(
    summary$draft,
    summary$valid,
    summary$conceptSetCount,
    summary$cohortCount,
    summary$evaluationCount
  )
  summary
}

getConceptSets <- function(connection, databaseSchema, phenotypeId) {
  sql <- "SELECT cs.concept_set_id,
    cs.concept_set_name,
    pcs.pertains,
    pcs.category,
    cs.cs_source_type,
    cs.valid,
    cs.has_descendants,
    cs.capr_concept_set_expression,
    cs.circe_concept_set_expression,
    cs.atlas_concept_set_id
  FROM @database_schema.phenotype_to_concept_set pcs
  INNER JOIN @database_schema.concept_set cs
    ON cs.concept_set_id = pcs.concept_set_id
  WHERE pcs.phenotype_id = @phenotype_id
  ORDER BY pcs.pertains, pcs.category, cs.concept_set_name;"
  queryPegasus(connection, databaseSchema, sql, phenotype_id = as.integer(phenotypeId))
}

getConceptSetPersonCounts <- function(connection, databaseSchema, phenotypeId) {
  sql <- "SELECT pc.concept_set_id,
    d.cdm_source_abbreviation,
    pc.condition_persons,
    pc.procedure_persons,
    pc.drug_persons,
    pc.device_persons,
    pc.measurement_persons,
    pc.observation_persons,
    pc.visit_persons,
    pc.overall_persons
  FROM @database_schema.phenotype_to_concept_set pcs
  INNER JOIN @database_schema.concept_set_person_count pc
    ON pc.concept_set_id = pcs.concept_set_id
  INNER JOIN @database_schema.database_meta_data d
    ON d.database_id = pc.database_id
  WHERE pcs.phenotype_id = @phenotype_id
  ORDER BY d.cdm_source_abbreviation;"
  queryPegasus(connection, databaseSchema, sql, phenotype_id = as.integer(phenotypeId))
}

getDraftTargets <- function(connection, databaseSchema, phenotypeId) {
  sql <- "SELECT pertains,
    category,
    term,
    concept_id,
    concept_name,
    domain_id,
    vocabulary_id,
    match_type
  FROM @database_schema.draft_concept_set_target
  WHERE phenotype_id = @phenotype_id
  ORDER BY pertains, category, term;"
  queryPegasus(connection, databaseSchema, sql, phenotype_id = as.integer(phenotypeId))
}

getCohortDefinitions <- function(connection, databaseSchema, phenotypeId) {
  sql <- "SELECT cohort_definition_id,
    cohort_definition_name,
    pa_source_type,
    valid,
    atlas_cohort_definition_id
  FROM @database_schema.cohort_definition
  WHERE phenotype_id = @phenotype_id
  ORDER BY cohort_definition_name;"
  queryPegasus(connection, databaseSchema, sql, phenotype_id = as.integer(phenotypeId))
}

# All evaluations when phenotypeId is NULL, otherwise only those of one phenotype.
getEvaluations <- function(connection, databaseSchema, phenotypeId = NULL) {
  sql <- "SELECT cd.phenotype_id,
    p.phenotype_name,
    cd.cohort_definition_id,
    cd.cohort_definition_name,
    cd.pa_source_type,
    d.database_id,
    d.cdm_source_abbreviation,
    ev.evaluation_type,
    ev.cohort_entries,
    ev.cohort_subjects,
    ev.true_positives,
    ev.false_positives,
    ev.true_negatives,
    ev.false_negatives,
    ev.ppv,
    ev.ppv_lb,
    ev.ppv_ub,
    ev.sensitivity,
    ev.sensitivity_lb,
    ev.sensitivity_ub
  FROM @database_schema.cohort_definition cd
  INNER JOIN @database_schema.phenotype p
    ON p.phenotype_id = cd.phenotype_id
  INNER JOIN @database_schema.cohort_definition_evaluation ev
    ON ev.cohort_definition_id = cd.cohort_definition_id
  INNER JOIN @database_schema.database_meta_data d
    ON d.database_id = ev.database_id"
  if (is.null(phenotypeId)) {
    evaluations <- queryPegasus(
      connection,
      databaseSchema,
      paste(sql, "ORDER BY cd.cohort_definition_name, d.cdm_source_abbreviation, ev.evaluation_type;")
    )
  } else {
    evaluations <- queryPegasus(
      connection,
      databaseSchema,
      paste(sql, "WHERE cd.phenotype_id = @phenotype_id",
            "ORDER BY cd.cohort_definition_name, d.cdm_source_abbreviation, ev.evaluation_type;"),
      phenotype_id = as.integer(phenotypeId)
    )
  }
  evaluations$evaluationType[is.na(evaluations$evaluationType)] <- "Unspecified"
  evaluations
}

emptyEvaluations <- function() {
  data.frame(
    phenotypeId = integer(0),
    phenotypeName = character(0),
    cohortDefinitionId = integer(0),
    cohortDefinitionName = character(0),
    paSourceType = character(0),
    databaseId = integer(0),
    cdmSourceAbbreviation = character(0),
    evaluationType = character(0),
    cohortEntries = numeric(0),
    cohortSubjects = numeric(0),
    truePositives = numeric(0),
    falsePositives = numeric(0),
    trueNegatives = numeric(0),
    falseNegatives = numeric(0),
    ppv = numeric(0),
    ppvLb = numeric(0),
    ppvUb = numeric(0),
    sensitivity = numeric(0),
    sensitivityLb = numeric(0),
    sensitivityUb = numeric(0)
  )
}

# Phenotypes this one was cloned from.
getParentPhenotypes <- function(connection, databaseSchema, phenotypeId) {
  sql <- "SELECT p.phenotype_id, p.phenotype_name
  FROM @database_schema.phenotype_ancestor a
  INNER JOIN @database_schema.phenotype p
    ON p.phenotype_id = a.parent_phenotype_id
  WHERE a.phenotype_id = @phenotype_id
  ORDER BY p.phenotype_name;"
  queryPegasus(connection, databaseSchema, sql, phenotype_id = as.integer(phenotypeId))
}

# Phenotypes that were cloned from this one.
getChildPhenotypes <- function(connection, databaseSchema, phenotypeId) {
  sql <- "SELECT p.phenotype_id, p.phenotype_name
  FROM @database_schema.phenotype_ancestor a
  INNER JOIN @database_schema.phenotype p
    ON p.phenotype_id = a.phenotype_id
  WHERE a.parent_phenotype_id = @phenotype_id
  ORDER BY p.phenotype_name;"
  queryPegasus(connection, databaseSchema, sql, phenotype_id = as.integer(phenotypeId))
}

# Returns NULL when no Atlas instance is configured.
getAtlasBaseUrl <- function(connection, databaseSchema) {
  baseUrl <- queryPegasus(connection, databaseSchema, "SELECT base_url FROM @database_schema.atlas_instance;")$baseUrl
  if (length(baseUrl) == 0 || is.na(baseUrl[1]) || !nzchar(baseUrl[1])) NULL else baseUrl[1]
}

getPhenotypeDetails <- function(connection, databaseSchema, phenotypeId) {
  list(
    conceptSets = getConceptSets(connection, databaseSchema, phenotypeId),
    personCounts = getConceptSetPersonCounts(connection, databaseSchema, phenotypeId),
    draftTargets = getDraftTargets(connection, databaseSchema, phenotypeId),
    cohorts = getCohortDefinitions(connection, databaseSchema, phenotypeId),
    evaluations = getEvaluations(connection, databaseSchema, phenotypeId),
    parents = getParentPhenotypes(connection, databaseSchema, phenotypeId),
    children = getChildPhenotypes(connection, databaseSchema, phenotypeId),
    atlasBaseUrl = getAtlasBaseUrl(connection, databaseSchema)
  )
}

# Cohort definitions (including the Circe JSON) of the given phenotypes, for export.
getCohortExpressions <- function(connection, databaseSchema, phenotypeIds) {
  if (length(phenotypeIds) == 0) {
    return(data.frame(
      cohortDefinitionId = integer(0),
      cohortDefinitionName = character(0),
      phenotypeId = integer(0),
      paSourceType = character(0),
      valid = integer(0),
      circeCohortExpression = character(0)
    ))
  }
  sql <- "SELECT cohort_definition_id,
    cohort_definition_name,
    phenotype_id,
    pa_source_type,
    valid,
    circe_cohort_expression
  FROM @database_schema.cohort_definition
  WHERE phenotype_id IN (@phenotype_ids)
  ORDER BY phenotype_id, cohort_definition_id;"
  queryPegasus(connection, databaseSchema, sql, phenotype_ids = as.integer(phenotypeIds))
}
