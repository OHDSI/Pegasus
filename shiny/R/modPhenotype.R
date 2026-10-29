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

# The base URL is the Atlas web UI (e.g. https://host/atlas), as stored in atlas_instance.
atlasLink <- function(baseUrl, type, id) {
  link <- rep("", length(id))
  if (is.null(baseUrl)) {
    return(link)
  }
  hasId <- !is.na(id)
  path <- switch(type, cohort = "cohortdefinition/%d", conceptSet = "conceptset/%d/details")
  link[hasId] <- sprintf(
    '<a href="%s/#/%s" target="_blank" rel="noopener">Atlas</a>',
    htmltools::htmlEscape(sub("/+$", "", baseUrl), attribute = TRUE),
    sprintf(path, as.integer(id[hasId]))
  )
  link
}

formatPercentWithInterval <- function(estimate, lower, upper) {
  sprintf("%.1f (%.1f-%.1f)", 100 * estimate, 100 * lower, 100 * upper)
}

# Adds one column per database with the overall person count of each concept set. Databases without counts
# get an empty column, so it is visible that nothing has been computed for them.
addPersonCountColumns <- function(conceptSets, personCounts, databases = unique(personCounts$cdmSourceAbbreviation)) {
  for (abbreviation in unique(c(databases, personCounts$cdmSourceAbbreviation))) {
    counts <- personCounts[personCounts$cdmSourceAbbreviation == abbreviation, , drop = FALSE]
    conceptSets[[abbreviation]] <- counts$overallPersons[match(conceptSets$conceptSetId, counts$conceptSetId)]
  }
  conceptSets
}

# Indented JSON for display; text that is not valid JSON is shown as it is.
prettyJson <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(text)) {
    return("Not available")
  }
  tryCatch(as.character(jsonlite::prettify(text)), error = function(e) text)
}

# Specificity (true negative rate) from the confusion matrix counts; NA when there are no negatives.
specificity <- function(trueNegatives, falsePositives) {
  negatives <- trueNegatives + falsePositives
  ifelse(is.na(negatives) | negatives == 0, NA_real_, trueNegatives / negatives)
}

draftOrValid <- function(valid) {
  ifelse(valid == 1, "Valid", "Draft (awaiting validation)")
}

# One row per cohort definition with what its evaluations show (best values over all databases and types).
summarizeCohorts <- function(cohorts, evaluations) {
  cohorts$evaluationCount <- vapply(cohorts$cohortDefinitionId, function(id) sum(evaluations$cohortDefinitionId == id), 1L)
  best <- function(column) {
    vapply(cohorts$cohortDefinitionId, function(id) {
      values <- evaluations[[column]][evaluations$cohortDefinitionId == id]
      if (length(values) == 0 || all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
    }, 1)
  }
  cohorts$bestPpv <- best("ppv")
  cohorts$bestSensitivity <- best("sensitivity")
  cohorts
}

# The evaluation columns shared by the phenotype and the cohort definition pages, as HTML (text is escaped,
# so tables built from it use escape = FALSE). The cohort column is a link, and is left out when the table is
# already about a single cohort.
evaluationTable <- function(evaluations, linkCohorts = TRUE) {
  table <- data.frame(
    Cohort = if (linkCohorts) {
      entityLink("cohort_definition", evaluations$cohortDefinitionId, evaluations$cohortDefinitionName)
    } else {
      htmltools::htmlEscape(evaluations$cohortDefinitionName)
    },
    Source = htmltools::htmlEscape(evaluations$cdSourceType),
    Database = htmltools::htmlEscape(evaluations$cdmSourceAbbreviation),
    "Evaluation type" = htmltools::htmlEscape(evaluations$evaluationType),
    Subjects = evaluations$cohortSubjects,
    "PPV % (95% CI)" = formatPercentWithInterval(evaluations$ppv, evaluations$ppvLb, evaluations$ppvUb),
    "Sensitivity % (95% CI)" = formatPercentWithInterval(
      evaluations$sensitivity, evaluations$sensitivityLb, evaluations$sensitivityUb
    ),
    "Specificity %" = round(100 * specificity(evaluations$trueNegatives, evaluations$falsePositives), 1),
    TP = evaluations$truePositives,
    FP = evaluations$falsePositives,
    FN = evaluations$falseNegatives,
    TN = evaluations$trueNegatives,
    check.names = FALSE
  )
  if (!linkCohorts) {
    table$Cohort <- NULL
  }
  table
}

metaDataLabel <- function(value) {
  if (is.null(value) || is.na(value)) return("Not recorded")
  paste0(toupper(substring(tolower(gsub("_", " ", value)), 1, 1)), substring(tolower(gsub("_", " ", value)), 2))
}

# Long format (one row per attribute) so the table reads like a fact sheet. Yes/no flags are stored as 0/1.
phenotypeMetaDataTable <- function(metaData) {
  if (is.null(metaData) || nrow(metaData) == 0) {
    return(data.frame(Attribute = character(0), Value = character(0)))
  }
  row <- metaData[1, ]
  yesNo <- function(value) if (is.na(value)) "Not recorded" else if (value == 1) "Yes" else "No"
  number <- function(value, unit) if (is.na(value)) "Not recorded" else paste(value, unit)
  data.frame(
    Attribute = c(
      "Pharmacovigilance outcome", "Clinical course", "Expected care setting", "Sex constraint",
      "Minimum age", "Maximum age", "Minimum gap between episodes",
      "Has discrete recorded symptoms", "Requires a diagnostic test or procedure",
      "Requires active treatment within 30 days", "Expects condition-specific follow-up or sequelae within 1 year"
    ),
    Value = c(
      metaDataLabel(row$isPharmacovigilanceOutcome), metaDataLabel(row$clinicalCourse),
      metaDataLabel(row$expectedCareSetting), metaDataLabel(row$sexConstraint),
      number(row$minAgeYears, "years"), number(row$maxAgeYears, "years"),
      number(row$minimumInterepisodeGapDays, "days"),
      yesNo(row$hasDiscreteRecordedSymptoms), yesNo(row$requiresDiagnosticTestOrProcedure),
      yesNo(row$requiresActiveTreatmentWithin30d), yesNo(row$expectsConditionSpecificFollowupOrSequelae1yr)
    )
  )
}

phenotypeColorByChoices <- c("Cohort definition" = "cohort", "Database" = "database", "Evaluation type" = "evaluationType")

phenotypeDetailUi <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("back"), "All phenotypes", icon = icon("arrow-left"), class = "btn-outline-primary mb-3"),
    uiOutput(ns("header")),
    bslib::navset_card_tab(
      id = ns("tabs"),
      height = "80vh",
      bslib::nav_panel(
        "Cohort comparison",
        radioButtons(ns("colorBy"), "Color by", choices = phenotypeColorByChoices, selected = "cohort", inline = TRUE),
        plotly::plotlyOutput(ns("scatter"), height = "380px"),
        DT::DTOutput(ns("evaluations"))
      ),
      bslib::nav_panel("Cohort definitions", DT::DTOutput(ns("cohorts"))),
      bslib::nav_panel("Concept sets", DT::DTOutput(ns("conceptSets"))),
      bslib::nav_panel("Person counts", DT::DTOutput(ns("personCounts"))),
      bslib::nav_panel("Draft targets", DT::DTOutput(ns("draftTargets"))),
      bslib::nav_panel("Metadata", DT::DTOutput(ns("metaData")))
    )
  )
}

phenotypeDetailServer <- function(id, connectionPool, databaseSchema, phenotypes, phenotypeId, shortlist, navigate, onRefine) {
  # Evaluate eagerly so a missing argument fails at startup instead of on the first click.
  force(onRefine)
  moduleServer(id, function(input, output, session) {
    observeEvent(input$back, navigate())
    observeEvent(input$toggleShortlist, toggleShortlist(shortlist, phenotypeId()))
    observeEvent(input$refine, onRefine(phenotypeId()))

    # Every newly opened phenotype starts on the cohort comparison, not on the previous phenotype's tab.
    observeEvent(phenotypeId(), bslib::nav_select("tabs", "Cohort comparison", session = session))

    phenotype <- reactive({
      req(phenotypeId())
      data <- phenotypes()
      data[data$phenotypeId == phenotypeId(), , drop = FALSE]
    })

    details <- reactive({
      req(phenotypeId(), nrow(phenotype()) == 1)
      tryCatch(
        withConnection(connectionPool, function(connection) {
          getPhenotypeDetails(connection, databaseSchema, phenotypeId())
        }),
        error = function(e) {
          showNotification(paste("Unable to load phenotype details:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    output$header <- renderUI({
      req(phenotypeId())
      row <- phenotype()
      if (nrow(row) != 1) {
        return(tags$div(class = "alert alert-warning", sprintf("Phenotype %d was not found.", phenotypeId())))
      }
      detail <- details()
      onShortlist <- row$phenotypeId %in% shortlist()
      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-start",
          tags$h3(row$phenotypeName, HTML(" "), HTML(statusBadge(row$status))),
          tags$div(
            class = "d-flex gap-2",
            copyLinkButton(list(page = "phenotype", id = row$phenotypeId)),
            actionButton(session$ns("refine"), "Refine this", icon = icon("pen"), class = "btn-outline-primary"),
            actionButton(
              session$ns("toggleShortlist"),
              if (onShortlist) "Remove from shortlist" else "Add to shortlist",
              icon = icon(if (onShortlist) "minus" else "plus"),
              class = if (onShortlist) "btn-secondary" else "btn-primary"
            )
          )
        ),
        tags$p(
          class = "text-muted small",
          sprintf("ID %s | Created by %s on %s UTC", row$phenotypeId, row$creator, formatTimestamp(row$timeCreated))
        ),
        if (!is.null(detail)) {
          tags$p(
            class = "small",
            "Refined from: ", entityLinkList("phenotype", detail$parents$phenotypeId, detail$parents$phenotypeName),
            " | Refined into: ", entityLinkList("phenotype", detail$children$phenotypeId, detail$children$phenotypeName)
          )
        },
        tags$div(
          class = "card card-body bg-body-tertiary mb-3",
          tags$h6("Clinical definition"),
          tags$p(class = "mb-0", style = "white-space: pre-wrap; max-height: 12em; overflow: auto;", row$clinicalDefinition)
        )
      )
    })

    showTable <- function(data, emptyMessage, ...) {
      filterableTable(
        data,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 25, language = list(emptyTable = emptyMessage)),
        ...
      )
    }

    output$scatter <- plotly::renderPlotly({
      detail <- details()
      req(detail, input$colorBy)
      buildScatter(
        detail$evaluations,
        colorBy = input$colorBy,
        showCi = TRUE,
        source = "phenotypeScatter",
        clickable = FALSE,
        emptyMessage = "No cohort definition of this phenotype has been evaluated."
      )
    })

    output$evaluations <- DT::renderDT({
      detail <- details()
      req(detail)
      showTable(
        evaluationTable(detail$evaluations),
        "No cohort definition of this phenotype has been evaluated.",
        escape = FALSE
      )
    })

    output$cohorts <- DT::renderDT({
      detail <- details()
      req(detail)
      cohorts <- summarizeCohorts(detail$cohorts, detail$evaluations)
      showTable(
        data.frame(
          Name = entityLink("cohort_definition", cohorts$cohortDefinitionId, cohorts$cohortDefinitionName),
          Source = htmltools::htmlEscape(cohorts$cdSourceType),
          Status = draftOrValid(cohorts$valid),
          Evaluations = cohorts$evaluationCount,
          "Best PPV (%)" = toPercent(cohorts$bestPpv),
          "Best sensitivity (%)" = toPercent(cohorts$bestSensitivity),
          Atlas = atlasLink(detail$atlasBaseUrl, "cohort", cohorts$atlasCohortDefinitionId),
          check.names = FALSE
        ),
        "No cohort definitions for this phenotype.",
        escape = FALSE
      )
    })

    output$conceptSets <- DT::renderDT({
      detail <- details()
      req(detail)
      conceptSets <- addPersonCountColumns(detail$conceptSets, detail$personCounts, detail$databaseNames)
      databaseColumns <- setdiff(names(conceptSets), names(detail$conceptSets))
      table <- data.frame(
        Name = entityLink("concept_set", conceptSets$conceptSetId, conceptSets$conceptSetName),
        Pertains = htmltools::htmlEscape(conceptSets$pertains),
        Category = htmltools::htmlEscape(conceptSets$category),
        Source = htmltools::htmlEscape(conceptSets$csSourceType),
        Status = draftOrValid(conceptSets$valid),
        Descendants = ifelse(conceptSets$hasDescendants == 1, "Yes", "No"),
        check.names = FALSE
      )
      for (column in databaseColumns) {
        table[[paste(column, "persons")]] <- conceptSets[[column]]
      }
      table$Atlas <- atlasLink(detail$atlasBaseUrl, "conceptSet", conceptSets$atlasConceptSetId)
      result <- showTable(table, "No concept sets for this phenotype.", escape = FALSE)
      if (length(databaseColumns) > 0) {
        result <- DT::formatRound(result, paste(databaseColumns, "persons"), digits = 0)
      }
      result
    })

    output$personCounts <- DT::renderDT({
      detail <- details()
      req(detail)
      counts <- detail$personCounts
      names <- detail$conceptSets$conceptSetName[match(counts$conceptSetId, detail$conceptSets$conceptSetId)]
      result <- showTable(
        data.frame(
          "Concept set" = entityLink("concept_set", counts$conceptSetId, names),
          Database = htmltools::htmlEscape(counts$cdmSourceAbbreviation),
          Overall = counts$overallPersons,
          Condition = counts$conditionPersons,
          Procedure = counts$procedurePersons,
          Drug = counts$drugPersons,
          Device = counts$devicePersons,
          Measurement = counts$measurementPersons,
          Observation = counts$observationPersons,
          Visit = counts$visitPersons,
          check.names = FALSE
        ),
        "No person counts are stored for this phenotype's concept sets (the concept_set_person_count table has no rows for them).",
        escape = FALSE
      )
      DT::formatRound(result, c("Overall", "Condition", "Procedure", "Drug", "Device", "Measurement", "Observation", "Visit"), digits = 0)
    })

    output$metaData <- DT::renderDT({
      detail <- details()
      req(detail)
      showTable(phenotypeMetaDataTable(detail$metaData), "No metadata is recorded for this phenotype.")
    })

    output$draftTargets <- DT::renderDT({
      detail <- details()
      req(detail)
      targets <- detail$draftTargets
      showTable(
        data.frame(
          Pertains = targets$pertains,
          Category = targets$category,
          Term = targets$term,
          "Concept ID" = targets$conceptId,
          "Concept name" = targets$conceptName,
          Domain = targets$domainId,
          Vocabulary = targets$vocabularyId,
          "Match type" = targets$matchType,
          check.names = FALSE
        ),
        "No draft targets for this phenotype."
      )
    })

  })
}
