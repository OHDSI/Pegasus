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

searchUi <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Phenotypes",
        actionButton(ns("refresh"), "Refresh", icon = icon("rotate"), class = "btn-sm")
      ),
      DT::DTOutput(ns("table"))
    ),
    bslib::card(
      bslib::card_header("Definition"),
      uiOutput(ns("detail"))
    )
  )
}

searchServer <- function(id, phenotypes, refreshPhenotypes) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$refresh, refreshPhenotypes())

    output$table <- DT::renderDT({
      data <- phenotypes()
      DT::datatable(
        data.frame(
          Name = data$phenotypeName,
          Creator = data$creator,
          Created = formatTimestamp(data$timeCreated),
          Definition = truncateText(data$clinicalDefinition)
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 25, search = list(smart = TRUE))
      )
    })

    output$detail <- renderUI({
      row <- input$table_rows_selected
      data <- phenotypes()
      if (is.null(row) || row > nrow(data)) {
        return(tags$p(class = "text-muted", "Select a phenotype to see its full definition."))
      }
      phenotype <- data[row, ]
      tagList(
        tags$h4(phenotype$phenotypeName),
        tags$p(
          class = "text-muted small",
          sprintf("ID %s | Created by %s on %s UTC",
                  phenotype$phenotypeId,
                  phenotype$creator,
                  formatTimestamp(phenotype$timeCreated))
        ),
        tags$p(style = "white-space: pre-wrap;", phenotype$clinicalDefinition)
      )
    })
  })
}
