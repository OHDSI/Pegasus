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

bslib::page_navbar(
  title = "Pegasus",
  id = "mainNav",
  theme = pegasusTheme(),
  header = tags$head(tags$link(rel = "icon", type = "image/png", href = "LogoSmall.png")),
  bslib::nav_panel("Browse", browseUi("browse")),
  bslib::nav_panel("Performance", performanceUi("performance")),
  bslib::nav_panel("New phenotype", createUi("create")),
  bslib::nav_panel("Evaluation", evaluationUi("evaluation")),
  bslib::nav_panel("Concept Search", conceptsUi("concepts")),
  bslib::nav_panel(
    title = tagList("Shortlist ", uiOutput("shortlistCount", inline = TRUE)),
    value = "Shortlist",
    shortlistUi("shortlist")
  ),
  bslib::nav_spacer(),
  bslib::nav_item(tags$img(src = "LogoTiny.png", height = "40px", alt = "Pegasus logo", class = "navbar-logo"))
)
