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

# Format and check code:
styler::style_pkg()
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Create manual and vignettes:
unlink("extras/Pegasus.pdf")
system("R CMD Rd2pdf ./ --output=extras/Pegasus.pdf")

pkgdown::build_site()

# Add standard concept sets to package:
library(dplyr)
connection <- DatabaseConnector::connect(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
vocabDatabaseSchema <- "optum_extended_dod.cdm_optum_extended_dod_v4020"
toRow <- function(capr) {
  capr <- Capr::getConceptSetDetails(capr, connection, vocabDatabaseSchema)
  json <- Capr::toConceptSetJson(capr)
  return(tibble(conceptSetName = capr@Name,
                circeConceptSetExpression = json,
                caprConceptSetExpression = Pegasus:::convertCirceConceptSetToCapr(json, capr@Name),
                sourceType = "User"))
}
standardConceptSets <- bind_rows(
  toRow(Capr::cs(Capr::descendants(9201), name = "Inpatient visit")),
  toRow(Capr::cs(Capr::descendants(9202), name = "Outpatient visit")),
  toRow(Capr::cs(Capr::descendants(9203, 262), name = "Emergency room visit"))
)
readr::write_csv(standardConceptSets, "inst/StandardConceptSets.csv")
DatabaseConnector::disconnect(connection)
