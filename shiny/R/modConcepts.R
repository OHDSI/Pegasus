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

# Written to the activity log box in the sidebar. The browser handler is registered once per page.
activityLogScript <- "
$(function() {
  Shiny.addCustomMessageHandler('pegasusLog', function(message) {
    var element = document.getElementById(message.id);
    if (!element) { return; }
    var lines = (element.textContent + message.line + '\\n').split('\\n');
    if (lines.length > 200) { lines = lines.slice(lines.length - 200); }
    element.textContent = lines.join('\\n');
    element.scrollTop = element.scrollHeight;
  });
});
"

conceptsUi <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Find phenotypes by concept",
      width = 340,
      tags$script(HTML(activityLogScript)),
      uiOutput(ns("connection")),
      shinyWidgets::radioGroupButtons(
        ns("mode"),
        label = NULL,
        choices = c("Name (fuzzy match)" = "name", "Concept ID (direct)" = "id"),
        selected = "name",
        size = "sm",
        justified = TRUE
      ),
      textInput(ns("term"), NULL, placeholder = "e.g. asthma"),
      # The label changes in the browser at once; the server restores it when the search is done. A label stuck on
      # "Searching..." therefore means the server is still busy (for example waiting on a slow vocabulary query).
      actionButton(
        ns("search"), "Search", icon = icon("magnifying-glass"), class = "btn-primary",
        onclick = "this.textContent = 'Searching...';"
      ),
      uiOutput(ns("vocabularyNote")),
      tags$div(
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$strong(class = "small", "Activity log"),
          tags$a(
            href = "#", class = "small",
            onclick = sprintf("document.getElementById('%s').textContent = ''; return false;", ns("log")),
            "Clear"
          )
        ),
        tags$pre(
          id = ns("log"),
          class = "small border rounded p-2 mb-0 bg-body-tertiary",
          style = "height: 160px; overflow: auto; white-space: pre-wrap; font-size: 0.75rem;"
        )
      ),
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
    vocabStatus <- reactiveVal(list(state = if (hasVocabulary) "unchecked" else "none", version = NA_character_))

    # Log lines go to the sidebar box and to the server log. The box is updated with a message sent straight away, so
    # it shows progress even while the server is blocked on a slow query (output updates would wait until it finished).
    appendLog <- function(text) {
      line <- sprintf("%s  %s", format(Sys.time(), "%H:%M:%S"), text)
      logEvent("concepts: ", text)
      tryCatch(
        session$rootScope()$sendCustomMessage("pegasusLog", list(id = session$ns("log"), line = line)),
        error = function(e) NULL
      )
    }
    stopwatch <- function() {
      started <- Sys.time()
      function() as.numeric(Sys.time() - started, units = "secs")
    }

    setVocabStatus <- function(state, version = NULL) {
      previous <- vocabStatus()
      vocabStatus(list(state = state, version = if (is.null(version)) previous$version else version))
    }

    # Runs a vocabulary query on a pooled connection, logging each step and recording whether it worked.
    withVocabulary <- function(description, fun) {
      elapsed <- stopwatch()
      appendLog(sprintf("%s: waiting for a vocabulary connection...", description))
      tryCatch(
        {
          result <- withConnection(vocabularyPool, function(connection) {
            appendLog(sprintf("%s: connection ready after %.1f s, running the query...", description, elapsed()))
            fun(connection)
          })
          appendLog(sprintf("%s: done in %.1f s.", description, elapsed()))
          setVocabStatus("connected")
          list(ok = TRUE, value = result)
        },
        error = function(e) {
          appendLog(sprintf("%s: FAILED after %.1f s: %s", description, elapsed(), conditionMessage(e)))
          setVocabStatus("error")
          list(ok = FALSE, error = conditionMessage(e))
        }
      )
    }

    checkVocabulary <- function() {
      if (!hasVocabulary) {
        return(invisible(NULL))
      }
      result <- withVocabulary("Connection check", function(connection) getVocabularyVersion(connection, vocabularySchema))
      if (result$ok) {
        setVocabStatus("connected", version = result$value)
      }
      invisible(NULL)
    }

    # The first check happens after the page is shown, so a slow connection does not delay the first render.
    session$onFlushed(function() checkVocabulary(), once = TRUE)
    observeEvent(input$checkVocabulary, checkVocabulary())

    output$connection <- renderUI({
      status <- vocabStatus()
      display <- switch(
        status$state,
        none = list(color = "#898781", label = "Vocabulary not configured"),
        unchecked = list(color = "#898781", label = "Vocabulary connection not checked yet"),
        connected = list(color = "#0ca30c", label = "Vocabulary connected"),
        error = list(color = "#d03b3b", label = "Vocabulary connection failed")
      )
      tagList(
        tags$div(
          class = "d-flex align-items-center gap-2 small",
          tags$span(style = sprintf("color: %s;", display$color), icon("circle")),
          tags$span(display$label),
          if (hasVocabulary) {
            actionLink(session$ns("checkVocabulary"), NULL, icon = icon("rotate"), title = "Check the connection")
          }
        ),
        if (hasVocabulary && status$state != "none") {
          tags$div(
            class = "text-muted small",
            "Vocabulary version: ",
            if (is.na(status$version)) "unknown" else status$version
          )
        }
      )
    })

    output$vocabularyNote <- renderUI({
      if (!hasVocabulary) {
        tags$div(
          class = "alert alert-warning",
          "No vocabulary database is configured. Only concept IDs can be searched, and only concept sets that",
          "list that exact concept are found (no descendants or mappings)."
        )
      }
    })

    observeEvent(input$mode, {
      updateTextInput(session, "term", placeholder = if (identical(input$mode, "id")) "e.g. 317009" else "e.g. asthma")
    })

    # Concept ID mode looks up the ID directly; name mode first resolves a fuzzy match to candidate concepts.
    runSearch <- function() {
      term <- trimws(input$term)
      candidates(emptyConcepts())
      selectedConcept(NULL)
      if (!nzchar(term)) {
        appendLog("Nothing to search for: the search box is empty.")
        return()
      }
      if (identical(input$mode, "id")) {
        conceptId <- parseConceptId(term)
        if (is.na(conceptId)) {
          appendLog(sprintf("'%s' is not a concept ID (digits only).", term))
          showNotification("Enter a numeric concept ID, or switch to name search.", type = "warning")
          return()
        }
        appendLog(sprintf("Concept ID lookup: %d", conceptId))
        conceptName <- NA_character_
        if (hasVocabulary) {
          result <- withVocabulary("Concept lookup", function(connection) getConcepts(connection, vocabularySchema, conceptId))
          if (result$ok && nrow(result$value) == 1) {
            conceptName <- result$value$conceptName
            appendLog(sprintf("Concept %d is '%s'.", conceptId, conceptName))
          } else if (result$ok) {
            appendLog(sprintf("Concept %d is not in the vocabulary; searching the concept sets anyway.", conceptId))
          }
        } else {
          appendLog("No vocabulary configured; searching the concept sets for the exact ID only.")
        }
        selectedConcept(list(id = conceptId, name = conceptName))
      } else {
        if (!hasVocabulary) {
          appendLog("Name search needs a vocabulary database, which is not configured.")
          showNotification("Searching by name needs a vocabulary database. Use the Concept ID option instead.", type = "warning")
          return()
        }
        appendLog(sprintf("Name search for '%s'", sanitizeSearchTerm(term)))
        result <- withVocabulary("Name search", function(connection) searchConcepts(connection, vocabularySchema, term))
        if (!result$ok) {
          showNotification(paste("Unable to search the vocabulary:", result$error), type = "error")
          return()
        }
        appendLog(sprintf("%d matching concepts.", nrow(result$value)))
        if (nrow(result$value) == 0) {
          showNotification("No concepts found.", type = "warning")
        }
        candidates(result$value)
      }
    }

    observeEvent(input$search, {
      appendLog(sprintf("Search clicked (%s mode).", if (identical(input$mode, "id")) "concept ID" else "name"))
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
        appendLog(sprintf("Selected concept %d '%s'.", found$conceptId[row], found$conceptName[row]))
        selectedConcept(list(id = found$conceptId[row], name = found$conceptName[row]))
      }
    })

    matches <- reactive({
      concept <- selectedConcept()
      req(concept)
      elapsed <- stopwatch()
      appendLog(sprintf("Finding phenotypes that use concept %d...", concept$id))
      found <- tryCatch(
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
          appendLog(sprintf("Phenotype lookup FAILED after %.1f s: %s", elapsed(), conditionMessage(e)))
          showNotification(paste("Unable to find phenotypes:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      if (!is.null(found)) {
        appendLog(sprintf(
          "Found %d concept set match%s in %d phenotype%s (%.1f s).",
          nrow(found), if (nrow(found) == 1) "" else "es",
          length(unique(found$phenotypeId)), if (length(unique(found$phenotypeId)) == 1) "" else "s",
          elapsed()
        ))
      }
      found
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
