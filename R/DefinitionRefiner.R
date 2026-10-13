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

#' Refine a clinical definition of a phenotype
#'
#' @param name               Name of the phenotype.
#' @param clinicalDefinition (Optional) An initial clinical definition to refine further.
#' @template LlmClient
#'
#' @returns
#' Does not return anything. Writes the definition to file.
#'
#' @export
refineDefinition <- function(name, clinicalDefinition = NULL, llmClient) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, len = 1, add = errorMessages)
  checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  skillFile <- system.file("skills", "ClinicalDefinitionRefiner.md", package = "Pegasus")
  lines <- readLines(skillFile)
  bodyStart <- which(lines == "---")[2] + 1
  systemPrompt <- paste(lines[bodyStart:length(lines)], collapse = "\n")

  llmClient$set_turns(list())
  llmClient$set_system_prompt(systemPrompt)
  llmClient <- registerFileWriteTool(llmClient)
  if (is.null(clinicalDefinition)) {
    llmClient$chat(name)
  } else {
    prompt <- sprintf("Name: %s\n\nInitial definition: %s\n",
                      name,
                      clinicalDefinition)
    llmClient$chat(prompt)
  }
  ellmer::live_console(llmClient, quiet = TRUE)
}

registerFileWriteTool <- function(llmClient) {
  saveDefinition <- function(definition, fileName) {
    writeLines(definition, fileName)
    return(paste("Successfully saved definition to", fileName))
  }

  saveTool <- ellmer::tool(
    saveDefinition,
    description = "Saves the final definition text to a specified file.",
    arguments = list(
      definition = ellmer::type_string("The full text of the definition to save."),
      fileName = ellmer::type_string("The name of the file to save to, e.g., 'output.txt'.")
    )
  )

  llmClient$register_tool(saveTool)
  return(llmClient)
}
