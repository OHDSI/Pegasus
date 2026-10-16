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

# Text columns with only a few distinct values become factors, which DT filters with a searchable multi-select
# dropdown (several values can be chosen) instead of a text box. Columns with many distinct values (names,
# dates) keep the text box, as do columns holding HTML, whose markup would show up in the dropdown.
dropdownFilterMaxValues <- 25

useDropdownFilters <- function(data, maxValues = dropdownFilterMaxValues) {
  for (column in names(data)) {
    values <- data[[column]]
    if (!is.character(values)) {
      next
    }
    distinct <- unique(values[!is.na(values)])
    if (length(distinct) > 0 && length(distinct) <= maxValues && !any(grepl("[<&]", distinct))) {
      data[[column]] <- factor(values, levels = sort(distinct))
    }
  }
  data
}

# Every table in the app is built through this wrapper so that each column has its own filter: a dropdown
# for text columns with few values, a text box for other text columns and a range slider for numbers.
filterableTable <- function(data, ...) {
  DT::datatable(useDropdownFilters(data), filter = "top", ...)
}
