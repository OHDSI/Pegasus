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

# Evaluation requests: a user pastes a Circe cohort definition, links it to a phenotype and queues
# evaluation runs. The backend process picks up rows of evaluation_request that have no matching
# cohort_definition_evaluation row yet.

# Light structural check; the backend does the real validation when it builds the cohort.
validateCirceJson <- function(text) {
  fail <- function(message) list(ok = FALSE, message = message, canonical = NULL)
  if (is.null(text) || !nzchar(trimws(text))) {
    return(fail("Paste a cohort definition in Circe JSON format."))
  }
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed) || !is.list(parsed) || is.null(names(parsed))) {
    return(fail("This is not a valid JSON object."))
  }
  if (is.null(parsed$PrimaryCriteria)) {
    return(fail("This JSON has no PrimaryCriteria, so it does not look like a Circe cohort definition."))
  }
  if (!is.null(parsed$ConceptSets) && !is.list(parsed$ConceptSets)) {
    return(fail("ConceptSets must be a list."))
  }
  list(
    ok = TRUE,
    message = "Valid Circe structure.",
    canonical = as.character(jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null", digits = NA))
  )
}

getDatabases <- function(connection, databaseSchema) {
  sql <- "SELECT database_id,
    cdm_source_name,
    cdm_source_abbreviation,
    cdm_release_date,
    vocabulary_version
  FROM @database_schema.database_meta_data
  ORDER BY database_id;"
  queryPegasus(connection, databaseSchema, sql)
}

getNextId <- function(connection, databaseSchema, table, column) {
  sql <- sprintf("SELECT MAX(%s) AS max_id FROM @database_schema.%s;", column, table)
  maxId <- queryPegasus(connection, databaseSchema, sql)$maxId
  if (length(maxId) == 0 || is.na(maxId)) 1L else as.integer(maxId) + 1L
}

getCohortNames <- function(connection, databaseSchema) {
  queryPegasus(connection, databaseSchema, "SELECT cohort_definition_name FROM @database_schema.cohort_definition;")$cohortDefinitionName
}

insertPegasusRows <- function(connection, databaseSchema, tableName, data) {
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = databaseSchema,
    tableName = tableName,
    data = data,
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
}

# Items of a Circe concept set expression as a data frame (one row per item).
circeItemsTable <- function(items) {
  items <- Filter(function(item) !is.null(item$concept$CONCEPT_ID), items)
  if (length(items) == 0) {
    return(data.frame(conceptId = integer(0), isExcluded = integer(0), includeDescendants = integer(0), includeMapped = integer(0)))
  }
  data.frame(
    conceptId = vapply(items, function(item) as.integer(item$concept$CONCEPT_ID), 1L),
    isExcluded = vapply(items, function(item) as.integer(isTRUE(item$isExcluded)), 1L),
    includeDescendants = vapply(items, function(item) as.integer(isTRUE(item$includeDescendants)), 1L),
    includeMapped = vapply(items, function(item) as.integer(isTRUE(item$includeMapped)), 1L)
  )
}

# Order-independent fingerprint, so a concept set that is already stored can be recognised and reused.
conceptSetSignature <- function(itemsTable) {
  paste(
    sort(paste(itemsTable$conceptId, itemsTable$isExcluded, itemsTable$includeDescendants, itemsTable$includeMapped)),
    collapse = ";"
  )
}

# Pertains and category are required by the data model; a pasted cohort does not say what its concept sets are for.
userConceptSetPertains <- "unspecified"
userConceptSetCategory <- "unspecified"

# Stores the concept sets of a pasted Circe cohort (as draft concept sets of the user source type), links them to
# the phenotype and to the cohort, and fills concept_set_item so they can be found by concept search.
# A concept set that already exists with identical items is reused instead of duplicated.
saveCohortConceptSets <- function(connection, databaseSchema, phenotypeId, cohortDefinitionId, cohortName, circe) {
  conceptSets <- circe$ConceptSets
  if (length(conceptSets) == 0) {
    return(invisible(integer(0)))
  }
  stored <- queryPegasus(
    connection, databaseSchema,
    "SELECT concept_set_id, concept_set_name, circe_concept_set_expression FROM @database_schema.concept_set;"
  )
  storedSignatures <- vapply(stored$circeConceptSetExpression, function(expression) {
    parsed <- tryCatch(jsonlite::fromJSON(expression, simplifyVector = FALSE), error = function(e) NULL)
    conceptSetSignature(circeItemsTable(parsed$items))
  }, "", USE.NAMES = FALSE)
  usedNames <- normalizeName(stored$conceptSetName)
  nextId <- getNextId(connection, databaseSchema, "concept_set", "concept_set_id")
  linkedIds <- integer(0)
  for (conceptSet in conceptSets) {
    items <- circeItemsTable(conceptSet$expression$items)
    signature <- conceptSetSignature(items)
    match <- which(storedSignatures == signature)
    if (nrow(items) > 0 && length(match) > 0) {
      conceptSetId <- stored$conceptSetId[match[1]]
    } else {
      name <- trimws(as.character(conceptSet$name))
      if (length(name) == 0 || !nzchar(name)) {
        name <- "Concept set"
      }
      candidate <- name
      if (normalizeName(candidate) %in% usedNames) {
        candidate <- sprintf("%s (%s)", name, cohortName)
      }
      suffix <- 1
      while (normalizeName(candidate) %in% usedNames) {
        suffix <- suffix + 1
        candidate <- sprintf("%s (%s) %d", name, cohortName, suffix)
      }
      expression <- as.character(jsonlite::toJSON(
        list(items = conceptSet$expression$items), auto_unbox = TRUE, null = "null", digits = NA
      ))
      conceptSetId <- nextId
      nextId <- nextId + 1L
      insertPegasusRows(connection, databaseSchema, "concept_set", data.frame(
        conceptSetId = as.integer(conceptSetId),
        conceptSetName = candidate,
        circeConceptSetExpression = expression,
        caprConceptSetExpression = NA_character_,
        hasDescendants = as.integer(any(items$includeDescendants == 1)),
        clinicalDefinition = NA_character_,
        conceptId = NA_integer_,
        valid = 0L,
        atlasConceptSetId = NA_integer_,
        csSourceType = "user"
      ))
      if (nrow(items) > 0) {
        insertPegasusRows(connection, databaseSchema, "concept_set_item", cbind(conceptSetId = as.integer(conceptSetId), items))
      }
      usedNames <- c(usedNames, normalizeName(candidate))
      stored <- rbind(stored, data.frame(
        conceptSetId = as.integer(conceptSetId), conceptSetName = candidate, circeConceptSetExpression = expression
      ))
      storedSignatures <- c(storedSignatures, signature)
    }
    linkedIds <- c(linkedIds, as.integer(conceptSetId))
  }
  linkedIds <- unique(linkedIds)
  alreadyLinked <- queryPegasus(
    connection, databaseSchema,
    "SELECT concept_set_id FROM @database_schema.phenotype_to_concept_set WHERE phenotype_id = @phenotype_id;",
    phenotype_id = as.integer(phenotypeId)
  )$conceptSetId
  newLinks <- setdiff(linkedIds, alreadyLinked)
  if (length(newLinks) > 0) {
    insertPegasusRows(connection, databaseSchema, "phenotype_to_concept_set", data.frame(
      phenotypeId = as.integer(phenotypeId),
      pertains = userConceptSetPertains,
      category = userConceptSetCategory,
      conceptSetId = newLinks
    ))
  }
  insertPegasusRows(connection, databaseSchema, "cohort_definition_to_concept_set", data.frame(
    cohortDefinitionId = as.integer(cohortDefinitionId),
    conceptSetId = linkedIds
  ))
  invisible(linkedIds)
}

# Saves a pasted cohort as an immutable cohort definition of the user source type (valid = 0 until the
# backend validates it), together with its concept sets. Pasting the same definition again for the same phenotype reuses the existing row.
saveCohortDefinition <- function(connection, databaseSchema, phenotypeId, cohortName, json, atlasCohortDefinitionId = NA) {
  validation <- validateCirceJson(json)
  if (!validation$ok) {
    stop(validation$message)
  }
  existing <- getCohortExpressions(connection, databaseSchema, phenotypeId)
  for (i in seq_len(nrow(existing))) {
    existingCanonical <- validateCirceJson(existing$circeCohortExpression[i])$canonical
    if (!is.null(existingCanonical) && identical(existingCanonical, validation$canonical)) {
      return(list(
        cohortDefinitionId = existing$cohortDefinitionId[i],
        cohortDefinitionName = existing$cohortDefinitionName[i],
        reused = TRUE,
        conceptSetCount = 0L
      ))
    }
  }
  cohortName <- trimws(cohortName)
  if (!nzchar(cohortName)) {
    stop("The cohort name cannot be empty.")
  }
  if (normalizeName(cohortName) %in% normalizeName(getCohortNames(connection, databaseSchema))) {
    stop(sprintf("A cohort named '%s' already exists.", cohortName))
  }
  cohortDefinitionId <- getNextId(connection, databaseSchema, "cohort_definition", "cohort_definition_id")
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = databaseSchema,
    tableName = "cohort_definition",
    data = data.frame(
      cohortDefinitionId = cohortDefinitionId,
      cohortDefinitionName = cohortName,
      phenotypeId = as.integer(phenotypeId),
      circeCohortExpression = trimws(json),
      paSourceType = "user",
      valid = 0L,
      atlasCohortDefinitionId = as.integer(atlasCohortDefinitionId)
    ),
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
  conceptSetIds <- saveCohortConceptSets(
    connection, databaseSchema, phenotypeId, cohortDefinitionId, cohortName,
    jsonlite::fromJSON(json, simplifyVector = FALSE)
  )
  list(
    cohortDefinitionId = cohortDefinitionId, cohortDefinitionName = cohortName, reused = FALSE,
    conceptSetCount = length(conceptSetIds)
  )
}

# Combinations that already have a request or a result.
getExistingEvaluations <- function(connection, databaseSchema, cohortDefinitionId) {
  sql <- "SELECT database_id, evaluation_type
  FROM @database_schema.evaluation_request
  WHERE cohort_definition_id = @cohort_definition_id
  UNION
  SELECT database_id, evaluation_type
  FROM @database_schema.cohort_definition_evaluation
  WHERE cohort_definition_id = @cohort_definition_id;"
  queryPegasus(connection, databaseSchema, sql, cohort_definition_id = as.integer(cohortDefinitionId))
}

# Adds one request per database and evaluation type, skipping combinations that already exist.
queueEvaluations <- function(connection, databaseSchema, cohortDefinitionId, databaseIds, evaluationTypes, requestedBy, notes = "") {
  combinations <- expand.grid(
    databaseId = as.integer(databaseIds),
    evaluationType = as.character(evaluationTypes),
    stringsAsFactors = FALSE
  )
  if (nrow(combinations) == 0) {
    stop("Choose at least one database and one evaluation type.")
  }
  existing <- getExistingEvaluations(connection, databaseSchema, cohortDefinitionId)
  alreadyThere <- paste(combinations$databaseId, combinations$evaluationType) %in%
    paste(existing$databaseId, existing$evaluationType)
  toQueue <- combinations[!alreadyThere, , drop = FALSE]
  if (nrow(toQueue) > 0) {
    timeRequested <- Sys.time()
    attr(timeRequested, "tzone") <- "UTC"
    firstId <- getNextId(connection, databaseSchema, "evaluation_request", "evaluation_request_id")
    DatabaseConnector::insertTable(
      connection = connection,
      databaseSchema = databaseSchema,
      tableName = "evaluation_request",
      data = data.frame(
        evaluationRequestId = firstId + seq_len(nrow(toQueue)) - 1L,
        cohortDefinitionId = as.integer(cohortDefinitionId),
        databaseId = toQueue$databaseId,
        evaluationType = toQueue$evaluationType,
        requestedBy = requestedBy,
        timeRequested = timeRequested,
        notes = if (is.null(notes) || !nzchar(trimws(notes))) NA_character_ else trimws(notes)
      ),
      dropTableIfExists = FALSE,
      createTable = FALSE,
      tempTable = FALSE,
      camelCaseToSnakeCase = TRUE,
      progressBar = FALSE
    )
  }
  list(queued = toQueue, skipped = combinations[alreadyThere, , drop = FALSE])
}

# Saves the pasted cohort (or reuses it) and queues the requested evaluations.
submitEvaluationRequest <- function(connection, databaseSchema, phenotypeId, cohortName, json, atlasCohortDefinitionId,
                                    databaseIds, evaluationTypes, requestedBy, notes = "") {
  if (length(databaseIds) == 0 || length(evaluationTypes) == 0) {
    stop("Choose at least one database and one evaluation type.")
  }
  cohort <- saveCohortDefinition(connection, databaseSchema, phenotypeId, cohortName, json, atlasCohortDefinitionId)
  queue <- queueEvaluations(connection, databaseSchema, cohort$cohortDefinitionId, databaseIds, evaluationTypes, requestedBy, notes)
  c(cohort, queue)
}

# Status is derived: complete once the backend has stored a matching evaluation.
getEvaluationRequests <- function(connection, databaseSchema, limit = 500) {
  sql <- "SELECT TOP @limit r.evaluation_request_id,
    r.time_requested,
    r.requested_by,
    p.phenotype_id,
    p.phenotype_name,
    cd.cohort_definition_id,
    cd.cohort_definition_name,
    d.cdm_source_abbreviation,
    r.evaluation_type,
    r.notes,
    CASE WHEN ev.cohort_definition_id IS NULL THEN 0 ELSE 1 END AS completed
  FROM @database_schema.evaluation_request r
  INNER JOIN @database_schema.cohort_definition cd
    ON cd.cohort_definition_id = r.cohort_definition_id
  INNER JOIN @database_schema.phenotype p
    ON p.phenotype_id = cd.phenotype_id
  INNER JOIN @database_schema.database_meta_data d
    ON d.database_id = r.database_id
  LEFT JOIN @database_schema.cohort_definition_evaluation ev
    ON ev.cohort_definition_id = r.cohort_definition_id
    AND ev.database_id = r.database_id
    AND ev.evaluation_type = r.evaluation_type
  ORDER BY r.time_requested DESC, r.evaluation_request_id DESC;"
  requests <- queryPegasus(connection, databaseSchema, sql, limit = as.integer(limit))
  requests$status <- ifelse(requests$completed == 1, "Complete", "Queued")
  requests
}

evaluationUi <- function(id) {
  ns <- NS(id)
  tagList(
    # Cards do not fill the height, so nothing inside them scrolls; the page grows instead. The queue
    # button sits in the footer of the run settings card and is always visible with it.
    bslib::layout_columns(
      col_widths = c(7, 5),
      fill = FALSE,
      bslib::card(
        fill = FALSE,
        bslib::card_header("1. Cohort definition"),
        textInput(ns("cohortName"), "Cohort name", width = "100%"),
        textAreaInput(
          ns("json"), "Circe JSON", rows = 16, width = "100%",
          placeholder = "Paste the cohort definition exported from Atlas (Export > JSON)."
        ),
        uiOutput(ns("jsonStatus")),
        numericInput(ns("atlasId"), "Atlas cohort ID (optional)", value = NA, min = 1, step = 1, width = "50%")
      ),
      tags$div(
        class = "d-flex flex-column gap-3",
        bslib::card(
          fill = FALSE,
          bslib::card_header("2. Phenotype"),
          shinyWidgets::pickerInput(
            ns("phenotype"),
            "Phenotype this cohort belongs to",
            choices = c("Choose a phenotype" = ""),
            multiple = FALSE,
            width = "100%",
            options = shinyWidgets::pickerOptions(container = "body", liveSearch = TRUE)
          ),
          actionButton(ns("newPhenotype"), "Create a new phenotype instead", icon = icon("plus"), class = "btn-outline-primary btn-sm"),
          uiOutput(ns("phenotypeSummary"))
        ),
        bslib::card(
          fill = FALSE,
          bslib::card_header("3. Run settings"),
          shinyWidgets::pickerInput(
            ns("databases"),
            "Databases",
            choices = character(0),
            multiple = TRUE,
            width = "100%",
            options = shinyWidgets::pickerOptions(
              container = "body",
              actionsBox = TRUE,
              selectedTextFormat = "count > 2",
              noneSelectedText = "Choose one or more databases"
            )
          ),
          checkboxGroupInput(ns("evalTypes"), "Evaluation type", choices = evaluationTypeOrder, selected = evaluationTypeOrder[1], inline = TRUE),
          textAreaInput(ns("notes"), "Notes (optional)", rows = 2, width = "100%"),
          bslib::card_footer(
            class = "d-flex justify-content-between align-items-center",
            uiOutput(ns("userNote")),
            actionButton(ns("queue"), "Queue evaluation", icon = icon("paper-plane"), class = "btn-success")
          )
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      min_height = "520px",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Evaluation requests",
        tags$div(
          class = "d-flex gap-3 align-items-center",
          checkboxInput(ns("onlyMine"), "Only my requests", value = FALSE),
          actionButton(ns("refreshRequests"), "Refresh", icon = icon("rotate"), class = "btn-sm")
        )
      ),
      DT::DTOutput(ns("requests"))
    )
  )
}

evaluationServer <- function(id, connectionPool, databaseSchema, userName, phenotypes, refreshPhenotypes,
                             preselect, onNewPhenotype) {
  moduleServer(id, function(input, output, session) {
    requestsVersion <- reactiveVal(0)

    output$userNote <- renderUI({
      if (is.null(userName)) {
        tags$div(class = "alert alert-danger", "You are not logged in. Queuing is disabled.")
      } else {
        tags$p(class = "text-muted small", paste("Requested as:", userName))
      }
    })

    databases <- reactive({
      tryCatch(
        withConnection(connectionPool, function(connection) getDatabases(connection, databaseSchema)),
        error = function(e) {
          showNotification(paste("Unable to load databases:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    observe({
      data <- databases()
      req(data)
      choices <- stats::setNames(data$databaseId, sprintf("%s - %s", data$cdmSourceAbbreviation, data$cdmSourceName))
      shinyWidgets::updatePickerInput(session, "databases", choices = choices, selected = isolate(input$databases))
    })

    phenotypeChoices <- function() {
      data <- phenotypes()
      data <- data[order(data$phenotypeName), , drop = FALSE]
      c("Choose a phenotype" = "", stats::setNames(as.character(data$phenotypeId), data$phenotypeName))
    }

    observe({
      shinyWidgets::updatePickerInput(
        session, "phenotype", choices = phenotypeChoices(), selected = isolate(input$phenotype)
      )
    })

    # Returning from the New phenotype screen: select the phenotype that was just created.
    observeEvent(preselect(), {
      shinyWidgets::updatePickerInput(
        session, "phenotype", choices = phenotypeChoices(), selected = as.character(preselect()$id)
      )
    })

    observeEvent(input$newPhenotype, {
      logEvent("evaluation: switching to the New phenotype tab")
      onNewPhenotype()
    })

    validation <- reactive(validateCirceJson(input$json))

    output$jsonStatus <- renderUI({
      if (!nzchar(trimws(input$json))) {
        return(NULL)
      }
      result <- validation()
      tags$p(class = if (result$ok) "text-success small" else "text-danger small", result$message)
    })

    selectedPhenotype <- reactive({
      id <- suppressWarnings(as.integer(input$phenotype))
      req(!is.na(id))
      data <- phenotypes()
      data[data$phenotypeId == id, , drop = FALSE]
    })

    output$phenotypeSummary <- renderUI({
      row <- selectedPhenotype()
      req(nrow(row) == 1)
      tagList(
        tags$p(HTML(statusBadge(row$status))),
        tags$p(class = "small text-muted", style = "white-space: pre-wrap;", truncateText(row$clinicalDefinition, 400))
      )
    })

    observeEvent(input$queue, {
      if (is.null(userName)) {
        showNotification("You must be logged in to queue an evaluation.", type = "error")
        return()
      }
      phenotypeId <- suppressWarnings(as.integer(input$phenotype))
      if (is.na(phenotypeId)) {
        showNotification("Choose a phenotype first.", type = "warning")
        return()
      }
      if (!validation()$ok) {
        showNotification(validation()$message, type = "warning")
        return()
      }
      atlasId <- if (is.null(input$atlasId) || is.na(input$atlasId)) NA_integer_ else as.integer(input$atlasId)
      result <- tryCatch(
        withConnection(connectionPool, function(connection) {
          submitEvaluationRequest(
            connection, databaseSchema, phenotypeId, input$cohortName, input$json, atlasId,
            input$databases, input$evalTypes, userName, input$notes
          )
        }),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(result)) {
        return()
      }
      message <- sprintf(
        "%s %d evaluation%s for '%s'.",
        if (result$reused) "Cohort already existed; queued" else "Saved the cohort and queued",
        nrow(result$queued), if (nrow(result$queued) == 1) "" else "s", result$cohortDefinitionName
      )
      if (!result$reused && result$conceptSetCount > 0) {
        message <- paste0(message, sprintf(" Linked %d concept set%s to the phenotype.", result$conceptSetCount, if (result$conceptSetCount == 1) "" else "s"))
      }
      if (nrow(result$skipped) > 0) {
        message <- paste0(message, sprintf(" %d already requested or evaluated were skipped.", nrow(result$skipped)))
      }
      showNotification(message, type = "message", duration = 10)
      updateTextInput(session, "cohortName", value = "")
      updateTextAreaInput(session, "json", value = "")
      updateTextAreaInput(session, "notes", value = "")
      updateNumericInput(session, "atlasId", value = NA)
      requestsVersion(requestsVersion() + 1)
      refreshPhenotypes()
    })

    observeEvent(input$refreshRequests, requestsVersion(requestsVersion() + 1))

    requests <- reactive({
      requestsVersion()
      tryCatch(
        withConnection(connectionPool, function(connection) getEvaluationRequests(connection, databaseSchema)),
        error = function(e) {
          showNotification(paste("Unable to load evaluation requests:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    output$requests <- DT::renderDT({
      data <- requests()
      req(data)
      if (isTRUE(input$onlyMine) && !is.null(userName)) {
        data <- data[data$requestedBy == userName, , drop = FALSE]
      }
      DT::datatable(
        data.frame(
          Requested = formatTimestamp(data$timeRequested),
          By = htmltools::htmlEscape(data$requestedBy),
          Phenotype = htmltools::htmlEscape(data$phenotypeName),
          Cohort = htmltools::htmlEscape(data$cohortDefinitionName),
          Database = htmltools::htmlEscape(data$cdmSourceAbbreviation),
          "Evaluation type" = htmltools::htmlEscape(data$evaluationType),
          Status = sprintf(
            '<span class="badge %s">%s</span>',
            ifelse(data$status == "Complete", "text-bg-success", "text-bg-warning"),
            data$status
          ),
          Notes = htmltools::htmlEscape(ifelse(is.na(data$notes), "", data$notes)),
          check.names = FALSE
        ),
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        options = list(
          pageLength = 10,
          order = list(list(0, "desc")),
          language = list(emptyTable = "No evaluation requests yet.")
        )
      )
    })
  })
}
