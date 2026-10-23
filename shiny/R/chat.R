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

finalDefinitionHeading <- "### Final Condition Definition"

loadSystemPrompt <- function(skillFile) {
  lines <- readLines(skillFile, encoding = "UTF-8")
  bodyStart <- which(lines == "---")[2] + 1
  skill <- paste(lines[bodyStart:length(lines)], collapse = "\n")
  override <- paste(
    "## Application environment override",
    "You are running inside a web application, not a local console. Do NOT write the final definition to a file.",
    "Instead, immediately after outputting the final definition, call the `submitDefinition` tool exactly once,",
    "passing only the definition paragraph (no heading, code fences, or commentary).",
    "The user will review, edit, and save the definition in the application.",
    sep = "\n"
  )
  paste(skill, override, sep = "\n\n")
}

createChatClient <- function(llmConfig, systemPrompt, onSubmit) {
  if (!is.null(llmConfig$provider) && llmConfig$provider == "JNJ BEDROCK") {
    client <- chat_jnj_bedrock(
      model = llmConfig$model,
      base_url = llmConfig$endpoint,
      api_key = llmConfig$apiKey,
      system_prompt = systemPrompt
    )
  } else {
    client <- ellmer::chat_azure_openai(
      endpoint = llmConfig$endpoint,
      model = llmConfig$model,
      api_version = llmConfig$apiVersion,
      credentials = function() llmConfig$apiKey,
      system_prompt = systemPrompt
    )
  }
  submitDefinition <- function(definition) {
    onSubmit(definition)
    "The definition has been placed in the application for the user to review and save."
  }
  client$register_tool(ellmer::tool(
    submitDefinition,
    name = "submitDefinition",
    description = "Submits the final clinical definition to the application so the user can review, edit, and save it.",
    arguments = list(
      definition = ellmer::type_string("The final definition paragraph only, without heading or commentary.")
    )
  ))
  client
}

buildInitialPrompt <- function(name, clinicalDefinition = NULL) {
  if (is.null(clinicalDefinition) || !nzchar(trimws(clinicalDefinition))) {
    return(name)
  }
  sprintf("Name: %s\n\nInitial definition: %s\n", name, clinicalDefinition)
}

# Fallback for when the model outputs the final definition without calling the tool.
extractFinalDefinition <- function(text) {
  if (is.null(text) || length(text) != 1 || is.na(text)) {
    return(NULL)
  }
  parts <- strsplit(text, finalDefinitionHeading, fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(NULL)
  }
  paragraphs <- strsplit(trimws(parts[length(parts)]), "\n\\s*\n")[[1]]
  definition <- trimws(paragraphs[1])
  if (is.na(definition) || !nzchar(definition)) NULL else definition
}
