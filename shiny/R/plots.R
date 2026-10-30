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

# Data colors come from the OHDSI visual identity palette (see brand.R). The first three categorical slots are blue, orange
# and teal, which stay distinguishable for color-blind readers. A fourth series is never generated: it folds
# into "Other", drawn in the logo navy with its own marker shape. Only the plot background, grid and axis lines
# are neutral grays, because the palette has no neutrals.
surfaceColor <- "#fcfcfb"
inkSecondary <- ohdsiNavy
inkMuted <- "#898781"
gridColor <- "#e1e0d9"
axisColor <- "#c3c2b7"
seriesColors <- c(ohdsiBlue, ohdsiOrange, ohdsiTeal)
# Marker shapes are a secondary encoding so identity never rests on color alone.
seriesSymbols <- c("circle", "square", "diamond")
otherColor <- ohdsiNavy
otherSymbol <- "triangle-up"
maxSeries <- length(seriesColors)

sourceTypeOrder <- c("Templates", "User", "MonoFlow", "PhenotypeDesigner")
evaluationTypeOrder <- c("KEEPER + llm", "PheValuator", "KEEPER + human")

colorByChoices <- c("Source type" = "source", "Database" = "database", "Evaluation type" = "evaluationType")
groupByChoices <- c("Database" = "database", "Source type" = "source", "Evaluation type" = "evaluationType")

chartFont <- list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif", color = inkSecondary)

# Known levels first in their fixed order, then any others alphabetically.
orderedLevels <- function(values, order) {
  values <- unique(values[!is.na(values)])
  c(order[order %in% values], sort(setdiff(values, order)))
}

# Color follows the entity: the slot comes from the entity's place in a fixed order, never from the
# currently filtered data, so filtering does not repaint the survivors.
assignSeries <- function(values, order, otherLabel = "Other") {
  rank <- match(values, order)
  slot <- ifelse(is.na(rank) | rank > maxSeries, NA_integer_, rank)
  list(label = ifelse(is.na(slot), otherLabel, values), slot = slot)
}

# The slot order lists the known values first and any other values alphabetically, so a value that is not
# in the built-in lists (a different spelling, a new source type) still gets its own color.
seriesGroups <- function(data, colorBy, databaseOrder, sourceLevels, evaluationTypeLevels, cohortLevels = character(0)) {
  switch(
    colorBy,
    source = assignSeries(data$cdSourceType, sourceLevels, "Other source"),
    database = assignSeries(data$cdmSourceAbbreviation, databaseOrder, "Other databases"),
    evaluationType = assignSeries(data$evaluationType, evaluationTypeLevels, "Other evaluation type"),
    cohort = assignSeries(data$cohortDefinitionName, cohortLevels, "Other cohorts"),
    stop("Unknown colorBy: ", colorBy)
  )
}

# Names come from the database and the tooltip accepts a small HTML subset, so they are escaped.
hoverText <- function(data) {
  sprintf(
    "<b>PPV %.1f%% | Sensitivity %.1f%%</b><br>%s<br>%s<br>%s | %s<br>%s subjects",
    100 * data$ppv,
    100 * data$sensitivity,
    htmltools::htmlEscape(data$phenotypeName),
    htmltools::htmlEscape(data$cohortDefinitionName),
    htmltools::htmlEscape(data$cdmSourceAbbreviation),
    htmltools::htmlEscape(data$evaluationType),
    format(data$cohortSubjects, big.mark = ",", trim = TRUE)
  )
}

# The explicit empty trace avoids plotly's "No trace type specified" warnings, and registering the events
# keeps event_data() from warning while there is nothing to show.
emptyPlot <- function(message, source = "emptyPlot", events = character(0)) {
  plot <- plotly::plot_ly(x = numeric(0), y = numeric(0), type = "scatter", mode = "markers", source = source)
  plot <- plotly::layout(
    plot,
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    annotations = list(list(
      text = message, showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5,
      font = list(color = inkSecondary, size = 14)
    )),
    paper_bgcolor = surfaceColor,
    plot_bgcolor = surfaceColor,
    font = chartFont
  )
  plot <- plotly::config(plot, displayModeBar = FALSE)
  for (event in events) {
    plot <- plotly::event_register(plot, event)
  }
  plot
}

axisStyle <- function(title) {
  list(
    title = title,
    range = c(-2, 102),
    gridcolor = gridColor,
    gridwidth = 1,
    zeroline = FALSE,
    linecolor = axisColor,
    tickfont = list(color = inkMuted)
  )
}

# Each point carries its phenotype and cohort definition as "phenotypeId:cohortDefinitionId".
pointKey <- function(phenotypeId, cohortDefinitionId) {
  paste0(phenotypeId, ":", cohortDefinitionId)
}

# The ids behind plotly customdata made by pointKey, as integer vectors (NA where missing).
parsePointKey <- function(keys) {
  keys <- as.character(keys)
  list(
    phenotypeId = suppressWarnings(as.integer(sub(":.*$", "", keys))),
    cohortDefinitionId = suppressWarnings(as.integer(sub("^[^:]*:?", "", keys)))
  )
}

# PPV (y) against sensitivity (x), one point per cohort/database/evaluation.
buildScatter <- function(data,
                         colorBy = "source",
                         showCi = FALSE,
                         databaseOrder = unique(data$cdmSourceAbbreviation[order(data$databaseId)]),
                         sourceLevels = orderedLevels(data$cdSourceType, sourceTypeOrder),
                         evaluationTypeLevels = orderedLevels(data$evaluationType, evaluationTypeOrder),
                         cohortLevels = orderedLevels(data$cohortDefinitionName, character(0)),
                         source = "performanceScatter",
                         clickable = TRUE,
                         selectable = clickable,
                         emptyMessage = "No evaluations match the current filters.") {
  events <- c(if (clickable) "plotly_click", if (selectable) "plotly_selected")
  # Points without a PPV or sensitivity cannot be placed (plotly would drop them with a warning).
  data <- data[!is.na(data$ppv) & !is.na(data$sensitivity), , drop = FALSE]
  if (nrow(data) == 0) {
    return(emptyPlot(emptyMessage, source = source, events = events))
  }
  for (column in c("ppvLb", "ppvUb", "sensitivityLb", "sensitivityUb")) {
    estimate <- if (grepl("^ppv", column)) data$ppv else data$sensitivity
    data[[column]] <- ifelse(is.na(data[[column]]), estimate, data[[column]])
  }
  groups <- seriesGroups(data, colorBy, databaseOrder, sourceLevels, evaluationTypeLevels, cohortLevels)
  data$group <- groups$label
  data$slot <- groups$slot
  data$hover <- hoverText(data)
  labels <- unique(data$group[order(data$slot)])

  plot <- plotly::plot_ly(source = source)
  for (label in labels) {
    rows <- data[data$group == label, , drop = FALSE]
    slot <- rows$slot[1]
    color <- if (is.na(slot)) otherColor else seriesColors[slot]
    symbol <- if (is.na(slot)) otherSymbol else seriesSymbols[slot]
    args <- list(
      p = plot,
      x = 100 * rows$sensitivity,
      y = 100 * rows$ppv,
      type = "scattergl",
      mode = "markers",
      name = label,
      customdata = pointKey(rows$phenotypeId, rows$cohortDefinitionId),
      text = rows$hover,
      hoverinfo = "text",
      marker = list(color = color, symbol = symbol, size = 9, opacity = 0.85, line = list(color = surfaceColor, width = 2))
    )
    if (showCi) {
      args$error_x <- list(
        type = "data", symmetric = FALSE, color = color, thickness = 1, width = 0,
        array = 100 * (rows$sensitivityUb - rows$sensitivity),
        arrayminus = 100 * (rows$sensitivity - rows$sensitivityLb)
      )
      args$error_y <- list(
        type = "data", symmetric = FALSE, color = color, thickness = 1, width = 0,
        array = 100 * (rows$ppvUb - rows$ppv),
        arrayminus = 100 * (rows$ppv - rows$ppvLb)
      )
    }
    plot <- do.call(plotly::add_trace, args)
  }
  plot <- plotly::layout(
    plot,
    xaxis = axisStyle("Sensitivity (%)"),
    yaxis = axisStyle("PPV (%)"),
    showlegend = length(labels) > 1,
    legend = list(orientation = "h", x = 0, y = 1.1, font = list(color = inkSecondary)),
    hovermode = "closest",
    hoverdistance = 24,
    dragmode = if (selectable) "select" else "zoom",
    paper_bgcolor = surfaceColor,
    plot_bgcolor = surfaceColor,
    font = chartFont
  )
  plot <- plotly::config(plot, displaylogo = FALSE, scrollZoom = TRUE)
  for (event in events) {
    plot <- plotly::event_register(plot, event)
  }
  plot
}

# PPV and sensitivity side by side (same 0-100% scale, so no dual axis), grouped by one dimension.
# The group is named on the x axis, so every box uses the same single color.
buildComparison <- function(data,
                            groupBy = "database",
                            databaseOrder = unique(data$cdmSourceAbbreviation[order(data$databaseId)])) {
  if (nrow(data) == 0) {
    return(emptyPlot("No evaluations match the current filters.", source = "comparison"))
  }
  values <- switch(
    groupBy,
    database = data$cdmSourceAbbreviation,
    source = data$cdSourceType,
    evaluationType = data$evaluationType,
    stop("Unknown groupBy: ", groupBy)
  )
  fixedOrder <- switch(groupBy, database = databaseOrder, source = sourceTypeOrder, evaluationType = evaluationTypeOrder)
  groupLevels <- orderedLevels(values, fixedOrder)
  counts <- as.integer(table(factor(values, levels = groupLevels)))
  categoryLabels <- sprintf("%s<br>n = %d", htmltools::htmlEscape(groupLevels), counts)
  x <- categoryLabels[match(values, groupLevels)]
  hover <- hoverText(data)

  panel <- function(metric) {
    plotly::plot_ly(
      x = x,
      y = 100 * data[[metric]],
      text = hover,
      type = "box",
      boxpoints = "all",
      jitter = 0.5,
      pointpos = 0,
      fillcolor = "rgba(51, 107, 145, 0.10)", # ohdsiBlue at 10% opacity
      line = list(color = seriesColors[1], width = 1.5),
      marker = list(color = seriesColors[1], size = 8, opacity = 0.55)
    )
  }
  plot <- plotly::subplot(panel("ppv"), panel("sensitivity"), nrows = 1, shareY = TRUE, margin = 0.04)
  categoryAxis <- list(
    categoryorder = "array",
    categoryarray = categoryLabels,
    linecolor = axisColor,
    tickfont = list(color = inkSecondary)
  )
  plotly::config(
    plotly::layout(
      plot,
      xaxis = categoryAxis,
      xaxis2 = categoryAxis,
      yaxis = axisStyle("Percent (%)"),
      showlegend = FALSE,
      annotations = list(
        list(text = "PPV", x = 0.24, y = 1, xref = "paper", yref = "paper", xanchor = "center",
             yanchor = "bottom", showarrow = FALSE, font = list(color = inkSecondary, size = 14)),
        list(text = "Sensitivity", x = 0.76, y = 1, xref = "paper", yref = "paper", xanchor = "center",
             yanchor = "bottom", showarrow = FALSE, font = list(color = inkSecondary, size = 14))
      ),
      paper_bgcolor = surfaceColor,
      plot_bgcolor = surfaceColor,
      font = chartFont
    ),
    displaylogo = FALSE
  )
}
