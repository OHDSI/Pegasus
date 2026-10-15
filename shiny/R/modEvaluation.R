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

# Saves a pasted cohort as an immutable cohort definition of the user source type (valid = 0 until the
# backend validates it). Pasting the same definition again for the same phenotype reuses the existing row.
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
        reused = TRUE
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
  list(cohortDefinitionId = cohortDefinitionId, cohortDefinitionName = cohortName, reused = FALSE)
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
    # A fixed height keeps the cards from being squeezed by the page, which would clip their contents.
    bslib::layout_columns(
      col_widths = c(7, 5),
      height = "670px",
      fill = FALSE,
      bslib::card(
        bslib::card_header("1. Cohort definition"),
        textInput(ns("cohortName"), "Cohort name", width = "100%"),
        textAreaInput(
          ns("json"), "Circe JSON", rows = 18, width = "100%",
          placeholder = "Paste the cohort definition exported from Atlas (Export > JSON)."
        ),
        uiOutput(ns("jsonStatus")),
        numericInput(ns("atlasId"), "Atlas cohort ID (optional)", value = NA, min = 1, step = 1, width = "50%")
      ),
      bslib::layout_columns(
        col_widths = 12,
        row_heights = c(1, 1),
        bslib::card(
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
          bslib::card_header("3. Run settings"),
          checkboxGroupInput(ns("databases"), "Databases", choices = character(0)),
          checkboxGroupInput(ns("evalTypes"), "Evaluation type", choices = evaluationTypeOrder, selected = evaluationTypeOrder[1], inline = TRUE),
          textAreaInput(ns("notes"), "Notes (optional)", rows = 2, width = "100%"),
          uiOutput(ns("userNote")),
          actionButton(ns("queue"), "Queue evaluation", icon = icon("paper-plane"), class = "btn-success")
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
      updateCheckboxGroupInput(session, "databases", choices = choices, selected = isolate(input$databases))
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
