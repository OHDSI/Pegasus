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

# Empty databases / phenotypeIds mean "no restriction".
filterEvaluations <- function(data, evalTypes, sources, databases, phenotypeIds, minPpv, minSensitivity) {
  keep <- data$evaluationType %in% evalTypes &
    data$paSourceType %in% sources &
    data$ppv >= minPpv / 100 &
    data$sensitivity >= minSensitivity / 100
  if (length(databases) > 0) {
    keep <- keep & data$cdmSourceAbbreviation %in% databases
  }
  if (length(phenotypeIds) > 0) {
    keep <- keep & data$phenotypeId %in% as.integer(phenotypeIds)
  }
  data[keep, , drop = FALSE]
}

# A multi-select dropdown. The list is attached to the page body so the card does not clip it.
filterPicker <- function(inputId, label, noneSelectedText, liveSearch = FALSE) {
  shinyWidgets::pickerInput(
    inputId,
    label,
    choices = character(0),
    multiple = TRUE,
    width = "100%",
    options = shinyWidgets::pickerOptions(
      container = "body",
      liveSearch = liveSearch,
      actionsBox = TRUE,
      selectedTextFormat = "count > 2",
      noneSelectedText = noneSelectedText
    )
  )
}

performanceUi <- function(id) {
  ns <- NS(id)
  tagList(
    # All four filters are the same kind of dropdown so they line up; the thresholds and the switch sit on a second row.
    bslib::card(
      bslib::card_header("Filters"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          filterPicker(ns("phenotypes"), "Phenotypes", "All evaluated phenotypes", liveSearch = TRUE),
          filterPicker(ns("databases"), "Databases", "All databases"),
          filterPicker(ns("evalTypes"), "Evaluation type", "None selected"),
          filterPicker(ns("sources"), "Source type", "None selected")
        ),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          class = "align-items-end",
          sliderInput(ns("minPpv"), "Minimum PPV (%)", min = 0, max = 100, value = 0, step = 5, width = "100%"),
          sliderInput(ns("minSensitivity"), "Minimum sensitivity (%)", min = 0, max = 100, value = 0, step = 5, width = "100%"),
          tags$div(
            class = "pb-3",
            bslib::input_switch(ns("showCi"), "Show 95% confidence intervals", value = FALSE)
          ),
          tags$div()
        )
      )
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "PPV vs sensitivity",
        radioButtons(ns("colorBy"), "Color by", choices = colorByChoices, selected = "source", inline = TRUE),
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          textOutput(ns("summary")),
          uiOutput(ns("shortlistActions"))
        ),
        plotly::plotlyOutput(ns("scatter"), height = "560px"),
        helpText("Click a point to open its phenotype, or drag a box around points to select them for the shortlist.",
                 "Only the first three databases get their own color; the rest are grouped as Other databases.")
      ),
      bslib::nav_panel(
        "Compare",
        radioButtons(ns("groupBy"), "Group by", choices = groupByChoices, selected = "database", inline = TRUE),
        plotly::plotlyOutput(ns("comparison"), height = "520px")
      ),
      bslib::nav_panel("Table", DT::DTOutput(ns("table")))
    )
  )
}

performanceServer <- function(id, evaluations, shortlist, onOpenPhenotype) {
  moduleServer(id, function(input, output, session) {
    scatterSource <- "performanceScatter"

    databaseOrder <- reactive({
      data <- evaluations()
      unique(data$cdmSourceAbbreviation[order(data$databaseId)])
    })

    observeEvent(evaluations(), {
      data <- evaluations()
      evalTypes <- orderedLevels(data$evaluationType, evaluationTypeOrder)
      sources <- orderedLevels(data$paSourceType, sourceTypeOrder)
      shinyWidgets::updatePickerInput(session, "evalTypes", choices = evalTypes, selected = evalTypes)
      shinyWidgets::updatePickerInput(session, "sources", choices = sources, selected = sources)
      shinyWidgets::updatePickerInput(session, "databases", choices = databaseOrder(), selected = character(0))
      phenotypeNames <- data[!duplicated(data$phenotypeId), c("phenotypeId", "phenotypeName")]
      phenotypeNames <- phenotypeNames[order(phenotypeNames$phenotypeName), ]
      shinyWidgets::updatePickerInput(
        session,
        "phenotypes",
        choices = stats::setNames(phenotypeNames$phenotypeId, phenotypeNames$phenotypeName),
        selected = character(0)
      )
    })

    filtered <- reactive({
      filterEvaluations(
        evaluations(),
        evalTypes = input$evalTypes,
        sources = input$sources,
        databases = input$databases,
        phenotypeIds = input$phenotypes,
        minPpv = input$minPpv,
        minSensitivity = input$minSensitivity
      )
    })

    output$summary <- renderText({
      data <- filtered()
      sprintf(
        "%s evaluations of %s phenotypes",
        format(nrow(data), big.mark = ","),
        format(length(unique(data$phenotypeId)), big.mark = ",")
      )
    })

    output$scatter <- plotly::renderPlotly({
      req(input$colorBy)
      buildScatter(
        filtered(),
        colorBy = input$colorBy,
        showCi = isTRUE(input$showCi),
        databaseOrder = databaseOrder(),
        source = scatterSource
      )
    })

    output$comparison <- plotly::renderPlotly({
      req(input$groupBy)
      buildComparison(filtered(), groupBy = input$groupBy, databaseOrder = databaseOrder())
    })

    selectedPhenotypeIds <- reactiveVal(integer(0))

    # A new filter slice redraws the plot, which clears the box selection in the browser.
    observeEvent(filtered(), selectedPhenotypeIds(integer(0)))

    observeEvent(plotly::event_data("plotly_selected", source = scatterSource), {
      selected <- plotly::event_data("plotly_selected", source = scatterSource)
      ids <- if (is.null(selected) || NROW(selected) == 0) integer(0) else suppressWarnings(as.integer(selected$customdata))
      selectedPhenotypeIds(unique(ids[!is.na(ids)]))
    }, ignoreNULL = FALSE)

    output$shortlistActions <- renderUI({
      inView <- length(unique(filtered()$phenotypeId))
      selected <- length(selectedPhenotypeIds())
      tags$div(
        class = "d-flex gap-2",
        if (selected > 0) {
          actionButton(session$ns("addSelected"), sprintf("Add %d selected to shortlist", selected),
                       icon = icon("plus"), class = "btn-primary btn-sm")
        },
        actionButton(session$ns("addInView"), sprintf("Add all %d in view to shortlist", inView),
                     icon = icon("plus"), class = "btn-outline-primary btn-sm")
      )
    })

    observeEvent(input$addSelected, notifyAdded(addToShortlist(shortlist, selectedPhenotypeIds())))
    observeEvent(input$addInView, notifyAdded(addToShortlist(shortlist, filtered()$phenotypeId)))

    observeEvent(plotly::event_data("plotly_click", source = scatterSource), {
      clicked <- plotly::event_data("plotly_click", source = scatterSource)
      phenotypeId <- suppressWarnings(as.integer(clicked$customdata[1]))
      if (length(phenotypeId) == 1 && !is.na(phenotypeId)) {
        onOpenPhenotype(phenotypeId)
      }
    })

    output$table <- DT::renderDT({
      data <- filtered()
      DT::datatable(
        data.frame(
          Phenotype = data$phenotypeName,
          Cohort = data$cohortDefinitionName,
          Source = data$paSourceType,
          Database = data$cdmSourceAbbreviation,
          "Evaluation type" = data$evaluationType,
          Subjects = data$cohortSubjects,
          "PPV (%)" = toPercent(data$ppv),
          "Sensitivity (%)" = toPercent(data$sensitivity),
          check.names = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 25)
      )
    })
  })
}
