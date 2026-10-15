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

createUi <- function(id) {
  ns <- NS(id)
  bslib::navset_hidden(
    id = ns("wizard"),
    bslib::nav_panel_hidden(
      "start",
      bslib::card(
        bslib::card_header("Start a new phenotype"),
        uiOutput(ns("cloneHintStart")),
        textInput(ns("name"), "Phenotype name", width = "100%"),
        textAreaInput(ns("initialDefinition"), "Initial definition (optional)", rows = 5, width = "100%"),
        actionButton(ns("start"), "Start", class = "btn-primary", width = "200px")
      )
    ),
    bslib::nav_panel_hidden(
      "refine",
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            "Refine the clinical definition",
            actionButton(ns("startOver"), "Start over", icon = icon("arrow-rotate-left"), class = "btn-sm")
          ),
          uiOutput(ns("cloneHintRefine")),
          uiOutput(ns("similarWarning")),
          shinychat::chat_ui(
            ns("chat"),
            drawer = FALSE,
            show_history = FALSE,
            enable_cancel = FALSE,
            height = "600px",
            fill = FALSE
          )
        ),
        bslib::card(
          bslib::card_header("Review and save"),
          textInput(ns("saveName"), "Phenotype name", width = "100%"),
          textAreaInput(ns("definition"), "Clinical definition", rows = 12, width = "100%",
                        placeholder = "The final definition will appear here when the conversation is done. You can edit it before saving."),
          uiOutput(ns("creator")),
          actionButton(ns("save"), "Save phenotype", class = "btn-success", icon = icon("floppy-disk"))
        )
      )
    )
  )
}

createServer <- function(id, connectionPool, databaseSchema, llmConfig, systemPrompt, userName, phenotypes, refreshPhenotypes,
                         cloneSource = reactiveVal(NULL), onSaved = function(phenotypeId) NULL) {
  moduleServer(id, function(input, output, session) {
    client <- NULL
    toolCalledThisTurn <- FALSE
    submittedDefinition <- reactiveVal(NULL)
    similarNames <- reactiveVal(character(0))

    output$creator <- renderUI({
      if (is.null(userName)) {
        tags$div(class = "alert alert-danger", "You are not logged in. Saving is disabled.")
      } else {
        tags$p(class = "text-muted", paste("Will be saved as:", userName))
      }
    })

    # cloneSource is NULL for a brand-new phenotype, otherwise list(id, name, definition) of the phenotype being refined.
    cloneHint <- function(showClear) {
      source <- cloneSource()
      if (is.null(source)) {
        return(NULL)
      }
      tags$div(
        class = "alert alert-info d-flex justify-content-between align-items-center",
        tags$span(
          "Refining ", tags$strong(source$name),
          ". The result is saved as a new phenotype linked to it; the original is not changed."
        ),
        if (showClear) {
          actionButton(session$ns("clearClone"), "Start from scratch", class = "btn-sm btn-outline-secondary")
        }
      )
    }
    output$cloneHintStart <- renderUI(cloneHint(showClear = TRUE))
    output$cloneHintRefine <- renderUI(cloneHint(showClear = FALSE))

    output$similarWarning <- renderUI({
      similar <- similarNames()
      if (length(similar) == 0) {
        return(NULL)
      }
      tags$div(
        class = "alert alert-warning",
        "Similar existing phenotypes: ",
        paste(head(similar, 10), collapse = ", ")
      )
    })

    # shinychat user input is a list (text plus attachments), so contents are spliced.
    streamResponse <- function(contents) {
      toolCalledThisTurn <<- FALSE
      stream <- client$stream_async(!!!contents)
      promises::then(
        shinychat::chat_append("chat", stream),
        onFulfilled = function(text) {
          if (!toolCalledThisTurn) {
            definition <- extractFinalDefinition(text)
            if (!is.null(definition)) {
              submittedDefinition(definition)
            }
          }
        },
        # shinychat already displays the error in the chat.
        onRejected = function(error) NULL
      )
      invisible(NULL)
    }

    # keepClone restores the form to the refined phenotype's definition instead of clearing it.
    reset <- function(keepClone = FALSE) {
      initialDefinition <- if (keepClone && !is.null(cloneSource())) cloneSource()$definition else ""
      if (!keepClone) {
        cloneSource(NULL)
      }
      # The chat only exists once a conversation has started, which is not the case when refining straight away.
      if (!is.null(client)) {
        shinychat::chat_clear("chat")
      }
      client <<- NULL
      submittedDefinition(NULL)
      similarNames(character(0))
      updateTextInput(session, "name", value = "")
      updateTextAreaInput(session, "initialDefinition", value = initialDefinition)
      updateTextInput(session, "saveName", value = "")
      updateTextAreaInput(session, "definition", value = "")
      bslib::nav_select("wizard", "start")
    }

    # Refining an existing phenotype: seed the definition and leave the name empty.
    observeEvent(cloneSource(), reset(keepClone = TRUE))

    observeEvent(input$clearClone, reset())

    observeEvent(input$start, {
      name <- trimws(input$name)
      if (!nzchar(name)) {
        showNotification("Please enter a phenotype name.", type = "warning")
        return()
      }
      existingNames <- phenotypes()$phenotypeName
      if (phenotypeNameExists(name, existingNames)) {
        showNotification(sprintf("A phenotype named '%s' already exists.", name), type = "error")
        return()
      }
      similarNames(findSimilarNames(name, existingNames))
      client <<- tryCatch(
        createChatClient(llmConfig, systemPrompt, onSubmit = function(definition) {
          toolCalledThisTurn <<- TRUE
          submittedDefinition(trimws(definition))
        }),
        error = function(e) {
          showNotification(paste("Unable to create LLM client:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      if (is.null(client)) {
        return()
      }
      updateTextInput(session, "saveName", value = name)
      updateTextAreaInput(session, "definition", value = "")
      bslib::nav_select("wizard", "refine")
      prompt <- buildInitialPrompt(name, input$initialDefinition)
      shinychat::chat_append("chat", prompt, role = "user")
      streamResponse(list(prompt))
    })

    observeEvent(input$chat_user_input, {
      req(client)
      streamResponse(input$chat_user_input)
    })

    observeEvent(submittedDefinition(), {
      updateTextAreaInput(session, "definition", value = submittedDefinition())
      showNotification("The definition is ready for review.", type = "message")
    })

    observeEvent(input$startOver, reset(keepClone = TRUE))

    observeEvent(input$save, {
      if (is.null(userName)) {
        showNotification("You must be logged in to save.", type = "error")
        return()
      }
      parentPhenotypeId <- if (is.null(cloneSource())) NA_integer_ else cloneSource()$id
      savedId <- tryCatch(
        withConnection(connectionPool, function(connection) {
          savePhenotype(connection, databaseSchema, input$saveName, input$definition, userName,
                        parentPhenotypeId = parentPhenotypeId)
        }),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (!is.null(savedId)) {
        showNotification(sprintf("Saved phenotype '%s'.", trimws(input$saveName)), type = "message")
        refreshPhenotypes()
        reset()
        onSaved(savedId)
      }
    })
  })
}
