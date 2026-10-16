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
        choices = c("Name" = "name", "Concept ID" = "id", "Code" = "code"),
        selected = "name",
        size = "sm",
        justified = TRUE
      ),
      textInput(ns("term"), NULL, placeholder = "e.g. asthma"),
      # The label changes in the browser at once; the server restores it when the search is done. A label stuck on
      # "Searching..." therefore means the server is still busy (for example waiting on a slow vocabulary query).
      actionButton(
        ns("search"), "Search", icon = icon("magnifying-glass"), class = "btn-primary",
        onclick = "var label = this.querySelector('.action-label'); if (label) { label.textContent = 'Searching...'; } else { this.textContent = 'Searching...'; }"
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
        "Name: fuzzy match on the concept name. Concept ID: direct lookup. Code: concepts whose code starts with the",
        "text (for example the ICD-10 code J45); a source code is resolved to the standard concepts it maps to."
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

# vocabularyRunner is NULL when no vocabulary database is configured. Otherwise it is a function(task, args) that
# starts a vocabulary task in the background and returns a handle (see backgroundVocabularyRunner()). The app stays
# responsive while the vocabulary works, and the activity log follows the background process live.
conceptsServer <- function(id, connectionPool, databaseSchema, vocabularyRunner, phenotypes, shortlist, onOpenPhenotype,
                           jobTimeoutSeconds = 180) {
  hasVocabulary <- !is.null(vocabularyRunner)
  moduleServer(id, function(input, output, session) {
    candidates <- reactiveVal(emptyConcepts())
    selectedConcept <- reactiveVal(NULL)
    matches <- reactiveVal(NULL)
    matchState <- reactiveVal("idle") # idle, loading, ready or failed
    vocabStatus <- reactiveVal(list(state = if (hasVocabulary) "unchecked" else "none", version = NA_character_))
    activeJob <- reactiveVal(NULL)

    # Log lines go to the sidebar box and to the server log.
    appendRaw <- function(line) {
      logEvent("concepts: ", line)
      tryCatch(
        session$rootScope()$sendCustomMessage("pegasusLog", list(id = session$ns("log"), line = line)),
        error = function(e) NULL
      )
    }
    appendLog <- function(text) appendRaw(sprintf("%s  %s", format(Sys.time(), "%H:%M:%S"), text))

    resetSearchButton <- function() {
      updateActionButton(session, "search", label = "Search", icon = icon("magnifying-glass"))
    }

    setVocabStatus <- function(state, version = NULL) {
      previous <- vocabStatus()
      vocabStatus(list(state = state, version = if (is.null(version)) previous$version else version))
    }

    # One vocabulary request at a time. onDone(result) gets list(ok, value) or list(ok = FALSE, error).
    startJob <- function(description, task, args, onDone) {
      if (!is.null(activeJob())) {
        appendLog(sprintf("%s: another vocabulary request is still running; try again when it finishes.", description))
        showNotification("A vocabulary request is still running.", type = "warning")
        return(FALSE)
      }
      appendLog(sprintf("%s: starting a background request...", description))
      handle <- tryCatch(
        vocabularyRunner(task, args),
        error = function(e) {
          appendLog(sprintf("%s: could not start: %s", description, conditionMessage(e)))
          NULL
        }
      )
      if (is.null(handle)) {
        setVocabStatus("error")
        return(FALSE)
      }
      activeJob(list(handle = handle, description = description, onDone = onDone, started = Sys.time()))
      TRUE
    }

    finishJob <- function(job, outcome) {
      activeJob(NULL)
      resetSearchButton()
      seconds <- as.numeric(Sys.time() - job$started, units = "secs")
      if (isTRUE(outcome$ok)) {
        appendLog(sprintf("%s: done in %.1f s.", job$description, seconds))
        setVocabStatus("connected")
      } else {
        appendLog(sprintf("%s: FAILED after %.1f s: %s", job$description, seconds, outcome$error))
        setVocabStatus("error")
      }
      job$onDone(outcome)
    }

    # Follows the running job: relays its log lines, and finishes it when it ends or runs out of time.
    observe({
      job <- activeJob()
      req(job)
      invalidateLater(500)
      isolate({
        polled <- job$handle$poll()
        for (line in polled$lines) {
          appendRaw(line)
        }
        if (isTRUE(polled$done)) {
          finishJob(job, polled)
        } else if (as.numeric(Sys.time() - job$started, units = "secs") > jobTimeoutSeconds) {
          job$handle$kill()
          finishJob(job, list(ok = FALSE, error = sprintf("timed out after %d seconds", as.integer(jobTimeoutSeconds))))
        }
      })
    })

    # The first check is manual: use the refresh icon.
    checkVocabulary <- function() {
      if (!hasVocabulary) {
        return(invisible(NULL))
      }
      startJob("Connection check", "check", list(), function(result) {
        if (isTRUE(result$ok)) {
          setVocabStatus("connected", version = result$value)
        }
      })
      invisible(NULL)
    }
    observeEvent(input$checkVocabulary, checkVocabulary())

    output$connection <- renderUI({
      status <- vocabStatus()
      busy <- !is.null(activeJob())
      display <- switch(
        status$state,
        none = list(color = "#898781", label = "Vocabulary not configured"),
        unchecked = list(color = "#898781", label = "Vocabulary not checked yet (use the refresh icon)"),
        connected = list(color = ohdsiTeal, label = "Vocabulary connected"),
        error = list(color = ohdsiOrange, label = "Vocabulary connection failed")
      )
      tagList(
        tags$div(
          class = "d-flex align-items-center gap-2 small",
          tags$span(style = sprintf("color: %s;", display$color), icon("circle")),
          tags$span(display$label),
          if (hasVocabulary) {
            actionLink(session$ns("checkVocabulary"), NULL, icon = icon("rotate"), title = "Check the connection")
          },
          if (busy) tags$span(class = "text-muted", "working...")
        ),
        if (hasVocabulary) {
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
      updateTextInput(
        session, "term",
        placeholder = switch(input$mode, id = "e.g. 317009", code = "e.g. J45 or J45.9", "e.g. asthma")
      )
    })

    # Concept ID mode looks the ID up directly; name mode first resolves a fuzzy match to candidate concepts.
    runSearch <- function() {
      term <- trimws(input$term)
      candidates(emptyConcepts())
      selectedConcept(NULL)
      if (!nzchar(term)) {
        appendLog("Nothing to search for: the search box is empty.")
        resetSearchButton()
        return()
      }
      if (identical(input$mode, "id")) {
        conceptId <- parseConceptId(term)
        if (is.na(conceptId)) {
          appendLog(sprintf("'%s' is not a concept ID (digits only).", term))
          showNotification("Enter a numeric concept ID, or switch to name search.", type = "warning")
          resetSearchButton()
          return()
        }
        appendLog(sprintf("Concept ID lookup: %d", conceptId))
        if (!hasVocabulary) {
          appendLog("No vocabulary configured; searching the concept sets for the exact ID only.")
          selectedConcept(list(id = conceptId, name = NA_character_))
          resetSearchButton()
          return()
        }
        started <- startJob("Concept lookup", "lookup", list(conceptId = conceptId), function(result) {
          if (!isTRUE(result$ok)) {
            showNotification(paste("Unable to look up the concept:", result$error), type = "error")
            return()
          }
          conceptName <- NA_character_
          if (nrow(result$value) == 1) {
            conceptName <- result$value$conceptName
            appendLog(sprintf("Concept %d is '%s'.", conceptId, conceptName))
          } else {
            appendLog(sprintf("Concept %d is not in the vocabulary; searching the concept sets anyway.", conceptId))
          }
          selectedConcept(list(id = conceptId, name = conceptName))
        })
      } else {
        isCode <- identical(input$mode, "code")
        kind <- if (isCode) "Code" else "Name"
        if (!hasVocabulary) {
          appendLog(sprintf("%s search needs a vocabulary database, which is not configured.", kind))
          showNotification(
            sprintf("Searching by %s needs a vocabulary database. Use the Concept ID option instead.", tolower(kind)),
            type = "warning"
          )
          resetSearchButton()
          return()
        }
        if (isCode && !nzchar(sanitizeCode(term))) {
          appendLog(sprintf("'%s' has no usable code characters (letters, digits and dots).", term))
          showNotification("Enter a code such as J45 or J45.9.", type = "warning")
          resetSearchButton()
          return()
        }
        appendLog(sprintf("%s search for '%s'", kind, if (isCode) sanitizeCode(term) else sanitizeSearchTerm(term)))
        started <- startJob(
          paste(kind, "search"),
          if (isCode) "searchCode" else "search",
          if (isCode) list(code = term) else list(term = term),
          function(result) {
            if (!isTRUE(result$ok)) {
              showNotification(paste("Unable to search the vocabulary:", result$error), type = "error")
              return()
            }
            appendLog(sprintf("%d matching concepts.", nrow(result$value)))
            if (nrow(result$value) == 0) {
              showNotification("No concepts found.", type = "warning")
            }
            candidates(result$value)
          }
        )
      }
      if (!started) {
        resetSearchButton()
      }
    }

    observeEvent(input$search, {
      appendLog(sprintf("Search clicked (%s mode).", switch(input$mode, id = "concept ID", code = "code", "name")))
      runSearch()
    })

    output$candidates <- renderUI({
      if (nrow(candidates()) == 0) {
        return(NULL)
      }
      bslib::card(
        fill = FALSE,
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
          Code = found$conceptCode,
          Domain = found$domainId,
          Vocabulary = found$vocabularyId,
          Class = found$conceptClassId,
          Standard = ifelse(is.na(found$standardConcept), "", found$standardConcept)
        ),
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 11)
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

    # Finding the phenotypes: the vocabulary supplies the concept's ancestors and mappings in the background, then the
    # Pegasus database (fast) is searched for concept sets that contain it.
    computeMatches <- function(concept, context, concepts) {
      started <- Sys.time()
      found <- tryCatch(
        withConnection(connectionPool, function(connection) {
          findPhenotypesForContext(connection, databaseSchema, concept$id, context)
        }),
        error = function(e) {
          appendLog(sprintf("Phenotype lookup FAILED: %s", conditionMessage(e)))
          showNotification(paste("Unable to find phenotypes:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      if (is.null(found)) {
        matchState("failed")
        return()
      }
      found$itemConceptName <- if (is.null(concepts)) {
        rep(NA_character_, nrow(found))
      } else {
        concepts$conceptName[match(found$itemConceptId, concepts$conceptId)]
      }
      found$viaConceptName <- if (is.null(concepts)) {
        rep(NA_character_, nrow(found))
      } else {
        concepts$conceptName[match(found$viaConceptId, concepts$conceptId)]
      }
      phenotypeCount <- length(unique(found$phenotypeId))
      appendLog(sprintf(
        "Found %d concept set match%s in %d phenotype%s (%.1f s).",
        nrow(found), if (nrow(found) == 1) "" else "es",
        phenotypeCount, if (phenotypeCount == 1) "" else "s",
        as.numeric(Sys.time() - started, units = "secs")
      ))
      matches(found)
      matchState("ready")
    }

    observeEvent(selectedConcept(), {
      concept <- selectedConcept()
      matches(NULL)
      if (is.null(concept)) {
        matchState("idle")
        return()
      }
      matchState("loading")
      if (!hasVocabulary) {
        computeMatches(concept, exactConceptContext(concept$id), NULL)
        return()
      }
      started <- startJob("Concept hierarchy", "context", list(conceptId = concept$id), function(result) {
        if (!isTRUE(result$ok)) {
          matchState("failed")
          showNotification(paste("Unable to read the concept hierarchy:", result$error), type = "error")
          return()
        }
        computeMatches(concept, result$value$context, result$value$concepts)
      })
      if (!started) {
        matchState("failed")
      }
    }, ignoreNULL = FALSE)

    output$title <- renderText({
      concept <- selectedConcept()
      if (is.null(concept)) {
        return("Search for a concept to see the phenotypes that use it")
      }
      label <- if (is.na(concept$name)) concept$id else sprintf("%s (%s)", concept$name, concept$id)
      switch(
        matchState(),
        loading = sprintf("Finding phenotypes that use %s...", label),
        failed = sprintf("Could not find phenotypes for %s", label),
        {
          count <- length(unique(matches()$phenotypeId))
          sprintf("%d phenotype%s use %s", count, if (count == 1) "" else "s", label)
        }
      )
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
      table <- data.frame(
        Phenotype = openLink,
        Status = statusBadge(status),
        "Concept set" = htmltools::htmlEscape(found$conceptSetName),
        Pertains = htmltools::htmlEscape(found$pertains),
        Category = htmltools::htmlEscape(found$category),
        Match = found$matchType,
        "Listed concept" = htmltools::htmlEscape(itemLabel),
        check.names = FALSE
      )
      # Only a source concept (such as an ICD code) is matched through a standard concept it maps to.
      if (any(!is.na(found$viaConceptId))) {
        table[["Via standard concept"]] <- htmltools::htmlEscape(ifelse(
          is.na(found$viaConceptId),
          "listed directly",
          ifelse(
            is.na(found$viaConceptName),
            as.character(found$viaConceptId),
            sprintf("%s (%s)", found$viaConceptName, found$viaConceptId)
          )
        ))
      }
      DT::datatable(
        table,
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
