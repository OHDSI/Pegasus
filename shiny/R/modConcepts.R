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

conceptsUi <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Find phenotypes by concept",
      width = 320,
      textInput(ns("term"), "Concept name or ID", placeholder = "e.g. asthma or 317009"),
      # The label changes in the browser at once; the server restores it when the search is done. A label stuck on
      # "Searching..." therefore means the server is still busy (for example waiting on a slow vocabulary query).
      actionButton(
        ns("search"), "Search", icon = icon("magnifying-glass"), class = "btn-primary",
        onclick = "this.textContent = 'Searching...';"
      ),
      uiOutput(ns("vocabularyNote")),
      helpText(
        "A concept set contains a concept when it lists the concept itself (Direct), lists an ancestor with",
        "'include descendants' (Descendant), or lists a concept it maps to (Mapped). Concept sets that exclude",
        "the concept are left out."
      )
    ),
    tagList(
      uiOutput(ns("candidates")),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          textOutput(ns("title"), inline = TRUE),
          actionButton(ns("addAll"), "Add all to shortlist", icon = icon("plus"), class = "btn-outline-primary btn-sm")
        ),
        DT::DTOutput(ns("results"))
      )
    )
  )
}

conceptsServer <- function(id, connectionPool, databaseSchema, vocabularyPool, vocabularySchema,
                           phenotypes, shortlist, onOpenPhenotype) {
  hasVocabulary <- !is.null(vocabularyPool)
  moduleServer(id, function(input, output, session) {
    candidates <- reactiveVal(emptyConcepts())
    selectedConcept <- reactiveVal(NULL)

    output$vocabularyNote <- renderUI({
      if (!hasVocabulary) {
        tags$div(
          class = "alert alert-warning",
          "No vocabulary database is configured. Only concept IDs can be searched, and only concept sets that",
          "list that exact concept are found (no descendants or mappings)."
        )
      }
    })

    # A concept ID goes straight to the lookup; a name is first resolved to candidate concepts.
    runSearch <- function() {
      term <- trimws(input$term)
      candidates(emptyConcepts())
      selectedConcept(NULL)
      if (!nzchar(term)) {
        return()
      }
      if (grepl("^[0-9]{1,9}$", term)) {
        conceptId <- as.integer(term)
        concept <- if (hasVocabulary) {
          tryCatch(
            withConnection(vocabularyPool, function(connection) getConcepts(connection, vocabularySchema, conceptId)),
            error = function(e) emptyConcepts()
          )
        } else {
          emptyConcepts()
        }
        selectedConcept(list(
          id = conceptId,
          name = if (nrow(concept) == 1) concept$conceptName else NA_character_
        ))
      } else if (!hasVocabulary) {
        showNotification("Searching by name needs a vocabulary database. Enter a concept ID instead.", type = "warning")
      } else {
        started <- Sys.time()
        elapsed <- function() as.numeric(Sys.time() - started, units = "secs")
        logEvent(sprintf("concept search for '%s': waiting for a vocabulary connection", sanitizeSearchTerm(term)))
        found <- withProgress(message = "Searching the vocabulary...", value = NULL, {
          tryCatch(
            withConnection(vocabularyPool, function(connection) {
              logEvent(sprintf("concept search: connection ready after %.1f s, running the query", elapsed()))
              searchConcepts(connection, vocabularySchema, term)
            }),
            error = function(e) {
              logEvent("concept search failed after ", sprintf("%.1f s: ", elapsed()), conditionMessage(e))
              showNotification(paste("Unable to search the vocabulary:", conditionMessage(e)), type = "error")
              emptyConcepts()
            }
          )
        })
        logEvent(sprintf("concept search: %d concepts found after %.1f s", nrow(found), elapsed()))
        if (nrow(found) == 0) {
          showNotification("No concepts found.", type = "warning")
        }
        candidates(found)
      }
    }

    observeEvent(input$search, {
      logEvent(sprintf("concept search clicked (vocabulary configured: %s)", hasVocabulary))
      tryCatch(
        runSearch(),
        finally = updateActionButton(session, "search", label = "Search", icon = icon("magnifying-glass"))
      )
    })

    output$candidates <- renderUI({
      if (nrow(candidates()) == 0) {
        return(NULL)
      }
      bslib::card(
        bslib::card_header("Choose a concept"),
        DT::DTOutput(session$ns("candidateTable"))
      )
    })

    output$candidateTable <- DT::renderDT({
      found <- candidates()
      DT::datatable(
        data.frame(
          ID = found$conceptId,
          Name = found$conceptName,
          Domain = found$domainId,
          Vocabulary = found$vocabularyId,
          Class = found$conceptClassId,
          Standard = ifelse(is.na(found$standardConcept), "", found$standardConcept)
        ),
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 10)
      )
    })

    observeEvent(input$candidateTable_rows_selected, {
      row <- input$candidateTable_rows_selected
      found <- candidates()
      if (length(row) == 1 && row <= nrow(found)) {
        selectedConcept(list(id = found$conceptId[row], name = found$conceptName[row]))
      }
    })

    matches <- reactive({
      concept <- selectedConcept()
      req(concept)
      tryCatch(
        withConnection(connectionPool, function(connection) {
          run <- function(vocabularyConnection) {
            found <- findPhenotypesByConcept(connection, databaseSchema, concept$id, vocabularyConnection, vocabularySchema)
            found$itemConceptName <- NA_character_
            if (!is.null(vocabularyConnection) && nrow(found) > 0) {
              listed <- getConcepts(vocabularyConnection, vocabularySchema, unique(found$itemConceptId))
              found$itemConceptName <- listed$conceptName[match(found$itemConceptId, listed$conceptId)]
            }
            found
          }
          if (hasVocabulary) withConnection(vocabularyPool, run) else run(NULL)
        }),
        error = function(e) {
          showNotification(paste("Unable to find phenotypes:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    output$title <- renderText({
      concept <- selectedConcept()
      if (is.null(concept)) {
        return("Search for a concept to see the phenotypes that use it")
      }
      label <- if (is.na(concept$name)) concept$id else sprintf("%s (%s)", concept$name, concept$id)
      found <- matches()
      count <- if (is.null(found)) 0 else length(unique(found$phenotypeId))
      sprintf("%d phenotype%s use %s", count, if (count == 1) "" else "s", label)
    })

    output$results <- DT::renderDT({
      found <- matches()
      req(found)
      status <- phenotypes()$status[match(found$phenotypeId, phenotypes()$phenotypeId)]
      status[is.na(status)] <- "Definition only"
      itemLabel <- ifelse(
        is.na(found$itemConceptName),
        as.character(found$itemConceptId),
        sprintf("%s (%s)", found$itemConceptName, found$itemConceptId)
      )
      openLink <- sprintf(
        '<a href="#" onclick="Shiny.setInputValue(\'%s\', %d, {priority: \'event\'}); return false;">%s</a>',
        session$ns("open"),
        as.integer(found$phenotypeId),
        htmltools::htmlEscape(found$phenotypeName)
      )
      DT::datatable(
        data.frame(
          Phenotype = openLink,
          Status = statusBadge(status),
          "Concept set" = htmltools::htmlEscape(found$conceptSetName),
          Pertains = htmltools::htmlEscape(found$pertains),
          Category = htmltools::htmlEscape(found$category),
          Match = found$matchType,
          "Listed concept" = htmltools::htmlEscape(itemLabel),
          check.names = FALSE
        ),
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        options = list(
          pageLength = 25,
          language = list(emptyTable = "No phenotype concept sets contain this concept.")
        )
      )
    })

    observeEvent(input$open, onOpenPhenotype(as.integer(input$open)))

    observeEvent(input$addAll, {
      found <- matches()
      req(found)
      notifyAdded(addToShortlist(shortlist, found$phenotypeId))
    })
  })
}
