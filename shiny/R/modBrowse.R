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

# DataTables render function: shows the status as a badge but sorts and filters on the plain status text.
statusBadgeRender <- function() {
  DT::JS(sprintf(
    "function(data, type, row) {
       if (type !== 'display' || data === null) { return data; }
       var classes = %s;
       return '<span class=\"badge ' + (classes[data] || 'text-bg-secondary') + '\">' + data + '</span>';
     }",
    as.character(jsonlite::toJSON(as.list(statusBadgeClasses), auto_unbox = TRUE))
  ))
}

# The three levels of the Browse tab, each with its own color, from clinical intent down to vocabulary.
browseLevels <- list(
  list(page = "phenotype", title = "Phenotype", descriptor = "Clinical intent", color = ohdsiBlue),
  list(page = "cohort_definition", title = "Cohort Definition", descriptor = "Implementation logic", color = ohdsiTeal),
  list(page = "concept_set", title = "Concept Set", descriptor = "Vocabulary", color = ohdsiOrange)
)

# A bar of three side-by-side blocks that shows at which level the user is browsing: the active block is filled
# with its color and names what is open. page is the route's page (NULL for the phenotype list); name is the open
# entity's name; parent is list(id, name) of the phenotype a cohort definition belongs to, shown as a link.
levelBar <- function(page = NULL, name = NULL, parent = NULL) {
  active <- if (is.null(page)) "phenotype" else page
  blocks <- lapply(browseLevels, function(level) {
    isActive <- level$page == active
    current <- if (isActive) {
      if (is.null(page)) "All phenotypes" else if (is.null(name)) "Not found" else name
    }
    currentHtml <- if (isActive) {
      htmltools::htmlEscape(current)
    } else if (level$page == "phenotype" && !is.null(parent)) {
      entityLink("phenotype", parent$id, parent$name)
    } else {
      "&nbsp;"
    }
    tags$div(
      class = paste("level-block", if (isActive) "active"),
      style = sprintf("--level-color: %s;", level$color),
      `aria-current` = if (isActive) "page",
      tags$div(class = "level-title", level$title),
      tags$div(class = "level-descriptor", level$descriptor),
      tags$div(class = "level-current", HTML(currentHtml))
    )
  })
  tags$div(class = "level-bar", blocks)
}

browseUi <- function(id) {
  ns <- NS(id)
  tagList(
  uiOutput(ns("levelBar")),
  bslib::navset_hidden(
    id = ns("view"),
    header = tags$style(HTML("
      .level-bar { display: flex; margin-bottom: 1rem; }
      .level-block { flex: 1 1 0; min-width: 0; padding: 0.7rem 1.1rem; border: 2px solid var(--level-color);
        color: var(--level-color); background: color-mix(in srgb, var(--level-color) 9%, white); }
      .level-block:first-child { border-radius: 0.5rem 0 0 0.5rem; }
      .level-block:last-child { border-radius: 0 0.5rem 0.5rem 0; }
      .level-block + .level-block { border-left-width: 0; }
      .level-block.active { background: var(--level-color); color: #ffffff; box-shadow: 0 3px 8px rgba(0, 0, 0, 0.25); }
      .level-title { font-size: 1.25rem; font-weight: 700; line-height: 1.2; }
      .level-descriptor { font-size: 0.8rem; opacity: 0.85; }
      .level-current { font-weight: 600; margin-top: 0.25rem; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      .level-block a { color: inherit; text-decoration: underline; }
      .clickable-rows tbody tr { cursor: pointer; }
      .clickable-rows .name-cell { max-width: 420px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      .clickable-rows th.narrow-col { writing-mode: vertical-rl; vertical-align: bottom; white-space: nowrap;
        height: 150px; padding: 8px 4px; text-align: left; }
      @supports (writing-mode: sideways-lr) { .clickable-rows th.narrow-col { writing-mode: sideways-lr; } }
      .clickable-rows th.narrow-col::before, .clickable-rows th.narrow-col::after { display: none; }
      /* The range filter boxes of the narrow numeric columns need some room to be usable. */
      .clickable-rows thead td { min-width: 76px; }
      .clickable-rows td.narrow-col { width: 1%; padding-left: 4px; padding-right: 4px; text-align: center; }
    ")),
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
          tags$div(class = "clickable-rows", DT::DTOutput(ns("table")))
        )
      )
    ),
    bslib::nav_panel_hidden("phenotype", phenotypeDetailUi(ns("phenotype"))),
    bslib::nav_panel_hidden("cohort_definition", cohortUi(ns("cohort"))),
    bslib::nav_panel_hidden("concept_set", conceptSetUi(ns("conceptSet")))
  )
  )
}

browseServer <- function(id, connectionPool, databaseSchema, phenotypes, refreshPhenotypes, route, navigate, shortlist, onRefine) {
  # Evaluate eagerly so a missing argument fails at startup instead of on the first click.
  force(onRefine)
  moduleServer(id, function(input, output, session) {
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

    idColumn <- 10L # zero-based position of the hidden Id column below

    output$table <- DT::renderDT({
      data <- filtered()
      # escape = FALSE is needed for the status badges, so free-text columns are escaped here.
      filterableTable(
        data.frame(
          Name = sprintf(
            '<div class="name-cell" title="%s">%s</div>',
            htmltools::htmlEscape(data$phenotypeName, attribute = TRUE),
            htmltools::htmlEscape(data$phenotypeName)
          ),
          # Plain text, so the column filter is a dropdown of status names; the badge is drawn by the render function below.
          Status = data$status,
          "Concept sets" = data$conceptSetCount,
          Cohorts = data$cohortCount,
          Databases = data$databaseCount,
          "Best PPV (%)" = toPercent(data$bestPpv),
          "Best sensitivity (%)" = toPercent(data$bestSensitivity),
          Creator = htmltools::htmlEscape(data$creator),
          Created = formatTimestamp(data$timeCreated),
          Shortlist = shortlistButton(session$ns("toggleShortlist"), data$phenotypeId, data$phenotypeId %in% shortlist()),
          # Hidden column: the row click handler reads the phenotype ID from here, so it stays correct
          # under server-side paging, sorting and searching.
          Id = data$phenotypeId,
          check.names = FALSE
        ),
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        # Clicks on the shortlist button or on links must not open the phenotype.
        callback = DT::JS(sprintf(
          "table.on('click', 'tbody tr', function(e) {
             if ($(e.target).closest('button, a').length > 0) { return; }
             var row = table.row(this);
             if (row.any()) { Shiny.setInputValue('%s', row.data()[%d], {priority: 'event'}); }
           });",
          session$ns("openPhenotype"),
          idColumn
        )),
        # The table re-renders when the shortlist changes; keep page, sort and search for this browser session.
        options = list(
          pageLength = 25,
          order = list(list(0, "asc")),
          stateSave = TRUE,
          stateDuration = -1,
          columnDefs = list(
            list(visible = FALSE, targets = idColumn),
            list(targets = 1, render = statusBadgeRender()),
            # Concept sets, cohorts, databases, PPV and sensitivity: narrow columns with vertical labels.
            list(className = "narrow-col", targets = 2:6)
          )
        )
      )
    })

    observeEvent(input$openPhenotype, {
      navigate("phenotype", input$openPhenotype)
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

    # The route decides which page the Browse tab shows.
    observeEvent(route(), {
      bslib::nav_select("view", if (is.null(route())) "list" else route()$page, session = session)
    }, ignoreNULL = FALSE)

    # What the level bar names: the open entity and, for a cohort definition, its phenotype.
    levelContext <- reactive({
      current <- route()
      if (is.null(current)) {
        return(NULL)
      }
      if (current$page == "phenotype") {
        data <- phenotypes()
        row <- data[data$phenotypeId == current$id, , drop = FALSE]
        return(if (nrow(row) == 1) list(name = row$phenotypeName) else NULL)
      }
      tryCatch(
        withConnection(connectionPool, function(connection) getRouteLabels(connection, databaseSchema, current$page, current$id)),
        error = function(e) NULL
      )
    })

    output$levelBar <- renderUI({
      current <- route()
      context <- levelContext()
      levelBar(
        page = current$page,
        name = context$name,
        parent = if (!is.null(context$parentId)) list(id = context$parentId, name = context$parentName)
      )
    })

    # The detail modules only see the ID when their own page is the current one.
    idFor <- function(page) {
      reactive(if (!is.null(route()) && route()$page == page) route()$id else NULL)
    }

    phenotypeDetailServer(
      "phenotype",
      connectionPool = connectionPool,
      databaseSchema = databaseSchema,
      phenotypes = phenotypes,
      phenotypeId = idFor("phenotype"),
      shortlist = shortlist,
      navigate = navigate,
      onRefine = onRefine
    )
    cohortServer(
      "cohort",
      connectionPool = connectionPool,
      databaseSchema = databaseSchema,
      cohortDefinitionId = idFor("cohort_definition"),
      navigate = navigate
    )
    conceptSetServer(
      "conceptSet",
      connectionPool = connectionPool,
      databaseSchema = databaseSchema,
      conceptSetId = idFor("concept_set"),
      navigate = navigate
    )
  })
}
