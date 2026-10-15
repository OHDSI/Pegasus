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

formatTimestamp <- function(x) {
  if (inherits(x, "POSIXt")) {
    format(x, "%Y-%m-%d %H:%M", tz = "UTC")
  } else {
    as.character(x)
  }
}

truncateText <- function(x, maxChars = 120) {
  ifelse(nchar(x) > maxChars, paste0(substr(x, 1, maxChars), "..."), x)
}

statusBadgeClasses <- c(
  "Evaluated" = "text-bg-success",
  "Cohort defined" = "text-bg-info",
  "Concept sets only" = "text-bg-secondary",
  "Definition only" = "text-bg-light border",
  "Draft (queued)" = "text-bg-warning",
  "Invalid" = "text-bg-danger"
)

# Returns an HTML string; statuses come from a fixed vocabulary so no escaping is needed.
statusBadge <- function(status) {
  sprintf('<span class="badge %s">%s</span>', unname(statusBadgeClasses[status]), status)
}

toPercent <- function(x) {
  round(100 * x, 1)
}

browseUi <- function(id) {
  ns <- NS(id)
  bslib::navset_hidden(
    id = ns("view"),
    bslib::nav_panel_hidden(
      "list",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Filters",
          width = 280,
          checkboxGroupInput(ns("status"), "Status", choices = statusLevels, selected = statusLevels),
          selectInput(ns("creator"), "Creator", choices = character(0), multiple = TRUE),
          helpText("Leave creator empty to show all creators."),
          actionButton(ns("refresh"), "Refresh", icon = icon("rotate"), class = "btn-sm")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            textOutput(ns("count"), inline = TRUE),
            actionButton(ns("addAll"), "Add all shown to shortlist", icon = icon("plus"), class = "btn-outline-primary btn-sm")
          ),
          DT::DTOutput(ns("table"))
        )
      )
    ),
    bslib::nav_panel_hidden("detail", phenotypeDetailUi(ns("detail")))
  )
}

browseServer <- function(id, connectionPool, databaseSchema, phenotypes, refreshPhenotypes, selectedId, shortlist) {
  moduleServer(id, function(input, output, session) {
    tableProxy <- DT::dataTableProxy("table")

    observeEvent(input$refresh, refreshPhenotypes())

    observe({
      updateSelectInput(
        session,
        "creator",
        choices = sort(unique(phenotypes()$creator)),
        selected = isolate(input$creator)
      )
    })

    filtered <- reactive({
      data <- phenotypes()
      data <- data[data$status %in% input$status, , drop = FALSE]
      if (length(input$creator) > 0) {
        data <- data[data$creator %in% input$creator, , drop = FALSE]
      }
      data
    })

    output$count <- renderText(sprintf("%d of %d phenotypes", nrow(filtered()), nrow(phenotypes())))

    output$table <- DT::renderDT({
      data <- filtered()
      # escape = FALSE is needed for the status badges, so free-text columns are escaped here.
      DT::datatable(
        data.frame(
          Name = htmltools::htmlEscape(data$phenotypeName),
          Status = statusBadge(data$status),
          "Concept sets" = data$conceptSetCount,
          Cohorts = data$cohortCount,
          Databases = data$databaseCount,
          "Best PPV (%)" = toPercent(data$bestPpv),
          "Best sensitivity (%)" = toPercent(data$bestSensitivity),
          Creator = htmltools::htmlEscape(data$creator),
          Created = formatTimestamp(data$timeCreated),
          Shortlist = shortlistButton(session$ns("toggleShortlist"), data$phenotypeId, data$phenotypeId %in% shortlist()),
          check.names = FALSE
        ),
        escape = FALSE,
        selection = "single",
        rownames = FALSE,
        # The table re-renders when the shortlist changes; keep page, sort and search for this browser session.
        options = list(pageLength = 25, order = list(list(0, "asc")), stateSave = TRUE, stateDuration = -1)
      )
    })

    observeEvent(input$toggleShortlist, {
      toggleShortlist(shortlist, input$toggleShortlist)
    })

    # table_rows_all holds the rows left after the table's own search, so this adds exactly what is shown.
    observeEvent(input$addAll, {
      data <- filtered()
      rowsShown <- input$table_rows_all
      rowsShown <- rowsShown[rowsShown <= nrow(data)]
      notifyAdded(addToShortlist(shortlist, data$phenotypeId[rowsShown]))
    })

    observeEvent(input$table_rows_selected, {
      row <- input$table_rows_selected
      data <- filtered()
      if (length(row) == 1 && row <= nrow(data)) {
        selectedId(data$phenotypeId[row])
      }
    })

    observeEvent(selectedId(), {
      bslib::nav_select("view", "detail", session = session)
    })

    # onBack runs inside the detail module's observer, so the session must be explicit; otherwise
    # nav_select would namespace "view" with the detail module's id and miss this navset.
    backToList <- function() {
      selectedId(NULL)
      DT::selectRows(tableProxy, NULL)
      bslib::nav_select("view", "list", session = session)
    }

    phenotypeDetailServer(
      "detail",
      connectionPool = connectionPool,
      databaseSchema = databaseSchema,
      phenotypes = phenotypes,
      selectedId = selectedId,
      shortlist = shortlist,
      onBack = backToList
    )
  })
}
