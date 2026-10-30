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

# The shortlist is a reactiveVal of phenotype IDs shared by every module (one per user session).
# These helpers must be called from an event handler or other reactive context.
addToShortlist <- function(shortlist, ids) {
  ids <- unique(as.integer(ids[!is.na(ids)]))
  added <- setdiff(ids, shortlist())
  shortlist(c(shortlist(), added))
  invisible(length(added))
}

removeFromShortlist <- function(shortlist, ids) {
  shortlist(setdiff(shortlist(), as.integer(ids)))
  invisible(NULL)
}

# Returns TRUE when the phenotype is on the shortlist afterwards.
toggleShortlist <- function(shortlist, id) {
  id <- as.integer(id)
  if (id %in% shortlist()) {
    removeFromShortlist(shortlist, id)
    FALSE
  } else {
    addToShortlist(shortlist, id)
    TRUE
  }
}

notifyAdded <- function(count) {
  message <- if (count == 0) {
    "Those phenotypes are already on the shortlist."
  } else {
    sprintf("Added %d phenotype%s to the shortlist.", count, if (count == 1) "" else "s")
  }
  showNotification(message, type = "message")
}

# A button for use inside a DT cell. Stops the click from also selecting the row.
shortlistButton <- function(inputId, ids, inShortlist) {
  sprintf(
    '<button type="button" class="btn btn-sm %s" onclick="event.stopPropagation(); Shiny.setInputValue(\'%s\', %d, {priority: \'event\'});">%s</button>',
    ifelse(inShortlist, "btn-secondary", "btn-outline-primary"),
    inputId,
    as.integer(ids),
    ifelse(inShortlist, "Remove", "Add")
  )
}

# Spreadsheet programs run cells that start with these characters as formulas.
csvSafe <- function(data) {
  data[] <- lapply(data, function(column) {
    if (is.character(column)) {
      risky <- !is.na(column) & grepl("^[=+@\t\r-]", column)
      column[risky] <- paste0("'", column[risky])
    }
    column
  })
  data
}

shortlistPhenotypeTable <- function(phenotypes, ids) {
  data <- phenotypes[phenotypes$phenotypeId %in% ids, , drop = FALSE]
  data$timeCreated <- formatTimestamp(data$timeCreated)
  data <- data[, c("phenotypeId", "phenotypeName", "clinicalDefinition", "status", "creator", "timeCreated",
                   "conceptSetCount", "cohortCount", "evaluationCount", "databaseCount", "bestPpv",
                   "bestSensitivity")]
  names(data) <- SqlRender::camelCaseToSnakeCase(names(data))
  csvSafe(data)
}

shortlistEvaluationTable <- function(evaluations, ids) {
  data <- evaluations[evaluations$phenotypeId %in% ids, , drop = FALSE]
  names(data) <- SqlRender::camelCaseToSnakeCase(names(data))
  csvSafe(data)
}

safeFileName <- function(name) {
  name <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", name))
  name <- substr(name, 1, 80)
  ifelse(nzchar(name), name, "cohort")
}

# Writes a zip with one Circe JSON file per cohort definition plus a manifest.csv.
writeCohortZip <- function(cohorts, file) {
  folder <- tempfile("cohorts")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  fileNames <- sprintf("%d_%s.json", cohorts$cohortDefinitionId, safeFileName(cohorts$cohortDefinitionName))
  for (i in seq_len(nrow(cohorts))) {
    writeLines(cohorts$circeCohortExpression[i], file.path(folder, fileNames[i]), useBytes = TRUE)
  }
  manifest <- cohorts[, c("cohortDefinitionId", "cohortDefinitionName", "phenotypeId", "cdSourceType", "valid")]
  manifest$file <- fileNames
  names(manifest) <- SqlRender::camelCaseToSnakeCase(names(manifest))
  utils::write.csv(csvSafe(manifest), file.path(folder, "manifest.csv"), row.names = FALSE)
  zip::zipr(file, files = list.files(folder), root = folder)
  invisible(file)
}

shortlistUi <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        textOutput(ns("title"), inline = TRUE),
        tags$div(
          class = "d-flex gap-2",
          actionButton(ns("compare"), "Compare", icon = icon("code-compare"), class = "btn-outline-primary btn-sm"),
          actionButton(ns("remove"), "Remove selected", icon = icon("minus"), class = "btn-outline-secondary btn-sm"),
          actionButton(ns("clear"), "Clear all", icon = icon("trash"), class = "btn-outline-danger btn-sm")
        )
      ),
      DT::DTOutput(ns("table"))
    ),
    bslib::card(
      fill = FALSE,
      min_height = "125px",
      bslib::card_header("Export"),
      tags$div(
        class = "d-flex flex-wrap gap-2",
        downloadButton(ns("downloadPhenotypes"), "Phenotypes (CSV)"),
        downloadButton(ns("downloadEvaluations"), "Evaluations (CSV)"),
        downloadButton(ns("downloadCohorts"), "Cohort definitions (ZIP of Circe JSON)")
      )
    ),
    bslib::card(
      bslib::card_header("PPV vs sensitivity of the shortlist"),
      plotly::plotlyOutput(ns("scatter"), height = "480px")
    )
  )
}

shortlistServer <- function(id, connectionPool, databaseSchema, phenotypes, evaluations, shortlist, onOpenPhenotype,
                            onCompare = function(phenotypeIds) NULL,
                            onOpenCohortDefinition = function(cohortDefinitionId) NULL) {
  moduleServer(id, function(input, output, session) {
    scatterSource <- "shortlistScatter"

    rows <- reactive({
      data <- phenotypes()
      data <- data[data$phenotypeId %in% shortlist(), , drop = FALSE]
      data[order(data$phenotypeName), , drop = FALSE]
    })

    shortlistedEvaluations <- reactive({
      data <- evaluations()
      data[data$phenotypeId %in% shortlist(), , drop = FALSE]
    })

    output$title <- renderText({
      count <- length(shortlist())
      sprintf("%d phenotype%s on the shortlist", count, if (count == 1) "" else "s")
    })

    output$table <- DT::renderDT({
      data <- rows()
      openLink <- sprintf(
        '<a href="#" onclick="event.stopPropagation(); Shiny.setInputValue(\'%s\', %d, {priority: \'event\'}); return false;">%s</a>',
        session$ns("open"),
        as.integer(data$phenotypeId),
        htmltools::htmlEscape(data$phenotypeName)
      )
      filterableTable(
        data.frame(
          Name = openLink,
          Status = statusBadge(data$status),
          "Concept sets" = data$conceptSetCount,
          Cohorts = data$cohortCount,
          Databases = data$databaseCount,
          "Best PPV (%)" = toPercent(data$bestPpv),
          "Best sensitivity (%)" = toPercent(data$bestSensitivity),
          Creator = htmltools::htmlEscape(data$creator),
          check.names = FALSE
        ),
        escape = FALSE,
        selection = "multiple",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          language = list(emptyTable = "Your shortlist is empty. Add phenotypes from Browse or Performance.")
        )
      )
    })

    observeEvent(input$open, onOpenPhenotype(as.integer(input$open)))

    observeEvent(input$remove, {
      selected <- input$table_rows_selected
      if (length(selected) > 0) {
        removeFromShortlist(shortlist, rows()$phenotypeId[selected])
      }
    })

    observeEvent(input$clear, shortlist(integer(0)))

    # Compares the selected rows, or the whole shortlist when no row is selected.
    observeEvent(input$compare, {
      selected <- input$table_rows_selected
      ids <- if (length(selected) > 0) rows()$phenotypeId[selected] else rows()$phenotypeId
      if (length(ids) < 2 || length(ids) > maxComparedPhenotypes) {
        showNotification(
          sprintf("Select 2 to %d phenotypes to compare (or have that many on the shortlist).", maxComparedPhenotypes),
          type = "warning"
        )
        return()
      }
      onCompare(ids)
    })

    output$scatter <- plotly::renderPlotly({
      buildScatter(
        shortlistedEvaluations(),
        colorBy = "source",
        source = scatterSource,
        selectable = FALSE,
        emptyMessage = "Add phenotypes that have evaluations to compare their performance here."
      )
    })

    observeEvent(plotly::event_data("plotly_click", source = scatterSource), {
      clicked <- plotly::event_data("plotly_click", source = scatterSource)
      key <- parsePointKey(clicked$customdata[1])
      if (length(key$cohortDefinitionId) == 1 && !is.na(key$cohortDefinitionId)) {
        onOpenCohortDefinition(key$cohortDefinitionId)
      } else if (length(key$phenotypeId) == 1 && !is.na(key$phenotypeId)) {
        onOpenPhenotype(key$phenotypeId)
      }
    })

    exportName <- function(prefix, extension) {
      sprintf("pegasus_%s_%s.%s", prefix, format(Sys.Date(), "%Y%m%d"), extension)
    }

    output$downloadPhenotypes <- downloadHandler(
      filename = function() exportName("shortlist_phenotypes", "csv"),
      content = function(file) {
        utils::write.csv(shortlistPhenotypeTable(phenotypes(), shortlist()), file, row.names = FALSE)
      }
    )

    output$downloadEvaluations <- downloadHandler(
      filename = function() exportName("shortlist_evaluations", "csv"),
      content = function(file) {
        utils::write.csv(shortlistEvaluationTable(evaluations(), shortlist()), file, row.names = FALSE)
      }
    )

    output$downloadCohorts <- downloadHandler(
      filename = function() exportName("shortlist_cohorts", "zip"),
      content = function(file) {
        cohorts <- withConnection(connectionPool, function(connection) {
          getCohortExpressions(connection, databaseSchema, shortlist())
        })
        writeCohortZip(cohorts, file)
      }
    )
  })
}
