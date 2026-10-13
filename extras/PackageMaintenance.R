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

# Sync shared assets to the Shiny app:
file.copy(list.files("inst/skills", pattern = "\\.md$", full.names = TRUE), "shiny/skills", overwrite = TRUE)
file.copy("extras/LogoSmall.png", "shiny/www/LogoSmall.png", overwrite = TRUE)

# Create manual and vignettes:
unlink("extras/Pegasus.pdf")
system("R CMD Rd2pdf ./ --output=extras/Pegasus.pdf")

pkgdown::build_site()
