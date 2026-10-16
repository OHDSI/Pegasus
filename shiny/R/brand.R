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

# Bootstrap theme for the whole app (the top bar stays white): the brand colors take the role of the Bootstrap
# colors, so buttons, links, badges, switches, sliders and tabs follow them. The palette has no red, so orange marks errors and invalid items.
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
    # Bootstrap picks black text on the teal and orange; labels on any dark color should be white. The
    # yellow and light blue are light, so they keep dark text.
    ".badge.text-bg-primary, .badge.text-bg-secondary, .badge.text-bg-success, .badge.text-bg-danger { color: #ffffff !important; }
     .btn-success, .btn-danger {
       --bs-btn-color: #ffffff;
       --bs-btn-hover-color: #ffffff;
       --bs-btn-active-color: #ffffff;
       --bs-btn-disabled-color: #ffffff;
     }
     .navbar .nav-link.active { box-shadow: inset 0 -3px 0 var(--bs-primary); }"
  )
}
