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

# OHDSI visual identity (https://github.com/OHDSI/VisualIdentity, Palettes/PaletteForPlots1.pdf).
# The palette lists #336B91, #69AED5, #11A08A, #FBC511 and #EB6622, plus the logo colors #1F425A, #F18A1D and
# #FBC50F. This file is named so that it loads before the other R files, which use these constants.
ohdsiBlue <- "#336B91"
ohdsiLightBlue <- "#69AED5"
ohdsiTeal <- "#11A08A"
ohdsiYellow <- "#FBC511"
ohdsiOrange <- "#EB6622"
ohdsiNavy <- "#1F425A"

# Bootstrap theme for the whole app: the brand colors take the role of the Bootstrap colors, so buttons, links,
# badges, switches, sliders and tabs follow them. The palette has no red, so orange marks errors and invalid items.
pegasusTheme <- function() {
  bslib::bs_add_rules(
    bslib::bs_theme(
      version = 5,
      primary = ohdsiBlue,
      secondary = ohdsiNavy,
      success = ohdsiTeal,
      info = ohdsiLightBlue,
      warning = ohdsiYellow,
      danger = ohdsiOrange
    ),
    sprintf(
      ".navbar {
         background-color: %1$s !important;
         --bs-navbar-color: rgba(255, 255, 255, 0.78);
         --bs-navbar-hover-color: #ffffff;
         --bs-navbar-active-color: #ffffff;
         --bs-navbar-brand-color: #ffffff;
         --bs-navbar-brand-hover-color: #ffffff;
       }
       .navbar .nav-link.active { box-shadow: inset 0 -3px 0 %2$s; }
       .navbar .navbar-toggler { background-color: rgba(255, 255, 255, 0.85); }
       .navbar-logo { background-color: #ffffff; border-radius: 4px; padding: 2px; }",
      ohdsiNavy, ohdsiYellow
    )
  )
}
