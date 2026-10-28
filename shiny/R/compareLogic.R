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

# The logic behind the Compare tab: what differs between phenotypes (their definition text and their concept sets)
# and how their performance differs. Everything here is plain data in, plain data out, so it can be tested and
# also feeds the downloadable report.

maxComparedPhenotypes <- 4

# ---- Definition text ----------------------------------------------------------------------------------------

tokenizeWords <- function(text) {
  if (is.null(text) || length(text) != 1 || is.na(text) || !nzchar(trimws(text))) {
    return(character(0))
  }
  strsplit(trimws(text), "\\s+")[[1]]
}

# A difference between two sequences (longest common subsequence). Returns a data frame with one row per item and
# op "same", "add" (only in new) or "del" (only in old), or NULL when a sequence is longer than maxItems.
sequenceDiff <- function(a, b, maxItems) {
  if (length(a) > maxItems || length(b) > maxItems) {
    return(NULL)
  }
  n <- length(a)
  m <- length(b)
  common <- matrix(0L, nrow = n + 1, ncol = m + 1)
  for (i in rev(seq_len(n))) {
    for (j in rev(seq_len(m))) {
      common[i, j] <- if (a[i] == b[j]) common[i + 1, j + 1] + 1L else max(common[i + 1, j], common[i, j + 1])
    }
  }
  items <- character(0)
  ops <- character(0)
  i <- 1
  j <- 1
  while (i <= n && j <= m) {
    if (a[i] == b[j]) {
      items <- c(items, a[i])
      ops <- c(ops, "same")
      i <- i + 1
      j <- j + 1
    } else if (common[i + 1, j] >= common[i, j + 1]) {
      items <- c(items, a[i])
      ops <- c(ops, "del")
      i <- i + 1
    } else {
      items <- c(items, b[j])
      ops <- c(ops, "add")
      j <- j + 1
    }
  }
  if (i <= n) {
    items <- c(items, a[i:n])
    ops <- c(ops, rep("del", n - i + 1))
  }
  if (j <= m) {
    items <- c(items, b[j:m])
    ops <- c(ops, rep("add", m - j + 1))
  }
  data.frame(item = items, op = ops, stringsAsFactors = FALSE)
}

# Word by word; the column holding the words is called token.
wordDiff <- function(old, new, maxTokens = 800) {
  diff <- sequenceDiff(tokenizeWords(old), tokenizeWords(new), maxTokens)
  if (!is.null(diff)) {
    names(diff)[1] <- "token"
  }
  diff
}

# The new text with its differences from the old one marked: added words in <ins>, removed words in <del>.
diffHtml <- function(old, new) {
  diff <- wordDiff(old, new)
  if (is.null(diff)) {
    return(paste0(
      "<em>The definitions are too long to compare word by word.</em><br>",
      htmltools::htmlEscape(if (is.na(new)) "" else new)
    ))
  }
  escaped <- htmltools::htmlEscape(diff$token)
  marked <- ifelse(
    diff$op == "add", paste0('<ins class="diff-add">', escaped, "</ins>"),
    ifelse(diff$op == "del", paste0('<del class="diff-del">', escaped, "</del>"), escaped)
  )
  paste(marked, collapse = " ")
}

# ---- Cohort definitions -------------------------------------------------------------------------------------

maxCohortLines <- 800

cohortLines <- function(text) {
  lines <- if (is.null(text) || is.na(text)) character(0) else sub("\\s+$", "", strsplit(text, "\r?\n")[[1]])
  lines[nzchar(trimws(lines))]
}

# Line by line difference of two cohort descriptions; NULL when they are too long to compare.
cohortLineDiff <- function(old, new) {
  diff <- sequenceDiff(cohortLines(old), cohortLines(new), maxCohortLines)
  if (!is.null(diff)) {
    names(diff)[1] <- "line"
  }
  diff
}

# The readable text of the chosen cohort definition of each phenotype. expressions has the columns of
# getCohortExpressions(); choices maps each phenotype ID (as text) to the cohort definition ID chosen. render turns
# Circe JSON into readable text and may fail (it needs CirceR and Java); the pretty-printed JSON is then used, and
# readable says which one it is. A phenotype without a cohort definition is left out.
cohortTextsFor <- function(expressions, choices, render) {
  texts <- list()
  for (phenotypeId in names(choices)) {
    row <- expressions[expressions$cohortDefinitionId == choices[[phenotypeId]] &
                         expressions$phenotypeId == as.integer(phenotypeId), , drop = FALSE]
    if (nrow(row) != 1) {
      next
    }
    json <- row$circeCohortExpression
    rendered <- tryCatch(render(json), error = function(e) NULL)
    texts[[phenotypeId]] <- list(
      cohortDefinitionId = row$cohortDefinitionId,
      name = row$cohortDefinitionName,
      text = if (is.null(rendered)) prettyJson(json) else rendered,
      readable = !is.null(rendered)
    )
  }
  texts
}

# HTML for a line diff: added lines green with +, removed lines red with -, unchanged lines plain. With
# contextLines, unchanged lines further than that from a change are replaced by a count.
lineDiffHtml <- function(diff, contextLines = NULL) {
  if (is.null(diff)) {
    return("<em>The cohort definitions are too long to compare line by line.</em>")
  }
  if (nrow(diff) == 0) {
    return("<em>Nothing to show.</em>")
  }
  changed <- diff$op != "same"
  near <- if (is.null(contextLines)) {
    rep(TRUE, nrow(diff))
  } else {
    positions <- which(changed)
    keep <- rep(FALSE, nrow(diff))
    for (position in positions) {
      keep[max(1, position - contextLines):min(nrow(diff), position + contextLines)] <- TRUE
    }
    keep
  }
  html <- character(0)
  hidden <- 0
  flush <- function() {
    if (hidden > 0) {
      html <<- c(html, sprintf('<div class="line-skip">%d unchanged line%s hidden</div>', hidden, if (hidden == 1) "" else "s"))
      hidden <<- 0
    }
  }
  for (i in seq_len(nrow(diff))) {
    if (!near[i]) {
      hidden <- hidden + 1
      next
    }
    flush()
    line <- htmltools::htmlEscape(diff$line[i])
    html <- c(html, switch(
      diff$op[i],
      add = sprintf('<div class="line-add">+ %s</div>', line),
      del = sprintf('<div class="line-del">- %s</div>', line),
      sprintf('<div class="line-same">&nbsp; %s</div>', line)
    ))
  }
  flush()
  if (!any(changed)) {
    html <- c('<div class="line-skip">The two cohort definitions read the same.</div>', html)
  }
  paste(html, collapse = "")
}

# ---- Concept sets -------------------------------------------------------------------------------------------

emptyConceptSetExpressions <- function() {
  data.frame(
    phenotypeId = integer(0), conceptSetId = integer(0), conceptSetName = character(0), pertains = character(0),
    category = character(0), circeConceptSetExpression = character(0), stringsAsFactors = FALSE
  )
}

# The concept sets of several phenotypes with their Circe expressions.
getPhenotypeConceptSetExpressions <- function(connection, databaseSchema, phenotypeIds) {
  if (length(phenotypeIds) == 0) {
    return(emptyConceptSetExpressions())
  }
  sql <- "SELECT pcs.phenotype_id,
    cs.concept_set_id,
    cs.concept_set_name,
    pcs.pertains,
    pcs.category,
    cs.circe_concept_set_expression
  FROM @database_schema.phenotype_to_concept_set pcs
  INNER JOIN @database_schema.concept_set cs
    ON cs.concept_set_id = pcs.concept_set_id
  WHERE pcs.phenotype_id IN (@phenotype_ids)
  ORDER BY pcs.phenotype_id, pcs.pertains, pcs.category, cs.concept_set_name;"
  queryPegasus(connection, databaseSchema, sql, phenotype_ids = as.integer(phenotypeIds))
}

# One text key per concept in a set: the concept and its three flags.
conceptKeys <- function(items) {
  sprintf("%d|%d|%d|%d", items$conceptId, items$isExcluded, items$includeDescendants, items$includeMapped)
}

conceptLabel <- function(items) {
  ifelse(is.na(items$conceptName), as.character(items$conceptId), sprintf("%s (%d)", items$conceptName, items$conceptId))
}

# At most a few names, then how many more.
shortList <- function(labels, limit = 5) {
  if (length(labels) <= limit) {
    return(paste(labels, collapse = ", "))
  }
  paste0(paste(labels[seq_len(limit)], collapse = ", "), sprintf(" and %d more", length(labels) - limit))
}

# What differs between two versions of a concept set, in words.
describeItemChanges <- function(baseItems, otherItems) {
  baseItems <- baseItems[!is.na(baseItems$conceptId), , drop = FALSE]
  otherItems <- otherItems[!is.na(otherItems$conceptId), , drop = FALSE]
  added <- otherItems[!otherItems$conceptId %in% baseItems$conceptId, , drop = FALSE]
  removed <- baseItems[!baseItems$conceptId %in% otherItems$conceptId, , drop = FALSE]
  shared <- intersect(baseItems$conceptId, otherItems$conceptId)
  baseShared <- baseItems[match(shared, baseItems$conceptId), , drop = FALSE]
  otherShared <- otherItems[match(shared, otherItems$conceptId), , drop = FALSE]
  flagged <- conceptKeys(baseShared) != conceptKeys(otherShared)
  flagText <- function(name, base, other) {
    ifelse(base != other, sprintf("%s %s", name, ifelse(other == 1, "on", "off")), NA_character_)
  }
  changes <- vapply(which(flagged), function(i) {
    flags <- c(
      flagText("excluded", baseShared$isExcluded[i], otherShared$isExcluded[i]),
      flagText("descendants", baseShared$includeDescendants[i], otherShared$includeDescendants[i]),
      flagText("mapped", baseShared$includeMapped[i], otherShared$includeMapped[i])
    )
    sprintf("%s: %s", conceptLabel(otherShared[i, , drop = FALSE]), paste(flags[!is.na(flags)], collapse = ", "))
  }, "")
  parts <- c(
    if (nrow(added) > 0) paste0("Added: ", shortList(conceptLabel(added))),
    if (nrow(removed) > 0) paste0("Removed: ", shortList(conceptLabel(removed))),
    if (length(changes) > 0) paste0("Flags changed: ", shortList(changes))
  )
  paste(parts, collapse = ". ")
}

emptyConceptSetChanges <- function() {
  data.frame(
    change = character(0), pertains = character(0), category = character(0), baselineName = character(0),
    otherName = character(0), details = character(0), stringsAsFactors = FALSE
  )
}

# Sets with identical concepts and flags are "Same" (the name may differ); then sets that share a name but not
# their content are "Changed"; the rest exist on one side only. base and other have the columns of
# getPhenotypeConceptSetExpressions().
compareConceptSets <- function(base, other) {
  baseItems <- lapply(base$circeConceptSetExpression, parseConceptSetItems)
  otherItems <- lapply(other$circeConceptSetExpression, parseConceptSetItems)
  signature <- function(items) paste(sort(unique(conceptKeys(items))), collapse = ";")
  baseSignature <- vapply(baseItems, signature, "")
  otherSignature <- vapply(otherItems, signature, "")
  baseUsed <- rep(FALSE, nrow(base))
  otherUsed <- rep(FALSE, nrow(other))
  rows <- list()
  addRow <- function(change, pertains, category, baselineName, otherName, details = "") {
    rows[[length(rows) + 1]] <<- data.frame(
      change = change, pertains = pertains, category = category, baselineName = baselineName,
      otherName = otherName, details = details, stringsAsFactors = FALSE
    )
  }
  valueOrNa <- function(x) if (is.na(x)) "" else x
  for (j in seq_len(nrow(other))) {
    candidates <- which(!baseUsed & nzchar(baseSignature) & baseSignature == otherSignature[j])
    if (length(candidates) == 0) {
      next
    }
    sameName <- candidates[tolower(base$conceptSetName[candidates]) == tolower(other$conceptSetName[j])]
    i <- if (length(sameName) > 0) sameName[1] else candidates[1]
    baseUsed[i] <- TRUE
    otherUsed[j] <- TRUE
    renamed <- tolower(base$conceptSetName[i]) != tolower(other$conceptSetName[j])
    addRow("Same", valueOrNa(other$pertains[j]), valueOrNa(other$category[j]), base$conceptSetName[i],
           other$conceptSetName[j], if (renamed) "Same concepts under a different name" else "")
  }
  for (j in which(!otherUsed)) {
    candidates <- which(!baseUsed & tolower(base$conceptSetName) == tolower(other$conceptSetName[j]))
    if (length(candidates) == 0) {
      next
    }
    i <- candidates[1]
    baseUsed[i] <- TRUE
    otherUsed[j] <- TRUE
    addRow("Changed", valueOrNa(other$pertains[j]), valueOrNa(other$category[j]), base$conceptSetName[i],
           other$conceptSetName[j], describeItemChanges(baseItems[[i]], otherItems[[j]]))
  }
  for (i in which(!baseUsed)) {
    addRow("Only in baseline", valueOrNa(base$pertains[i]), valueOrNa(base$category[i]), base$conceptSetName[i], "")
  }
  for (j in which(!otherUsed)) {
    addRow("Only here", valueOrNa(other$pertains[j]), valueOrNa(other$category[j]), "", other$conceptSetName[j])
  }
  if (length(rows) == 0) emptyConceptSetChanges() else do.call(rbind, rows)
}

# ---- Performance --------------------------------------------------------------------------------------------

negativePredictiveValue <- function(trueNegatives, falseNegatives) {
  negatives <- trueNegatives + falseNegatives
  ifelse(is.na(negatives) | negatives == 0, NA_real_, trueNegatives / negatives)
}

# The database and evaluation type that cover the most of the given phenotypes, as a starting choice.
defaultMetricChoice <- function(evaluations) {
  if (nrow(evaluations) == 0) {
    return(list(database = NULL, evaluationType = NULL))
  }
  pairs <- unique(evaluations[, c("phenotypeId", "cdmSourceAbbreviation", "evaluationType")])
  counts <- table(paste(pairs$cdmSourceAbbreviation, pairs$evaluationType, sep = "\r"))
  best <- names(counts)[which.max(counts)]
  parts <- strsplit(best, "\r", fixed = TRUE)[[1]]
  list(database = parts[1], evaluationType = parts[2])
}

# The rows of the performance table. A phenotype with several cohort definitions that have results for the
# database and evaluation type gets one row per cohort definition plus a summary row with the median of each
# measure; a phenotype with one cohort definition has just that row, and one with none a row of NAs. Summary rows
# (isSummary) stand for the phenotype as a whole. Measures are fractions; the deltas are percentage points against
# the baseline phenotype's summary row, and are NA on the baseline's own rows.
comparisonMetrics <- function(evaluations, phenotypes, phenotypeIds, baselineId, database, evaluationType) {
  measures <- c("ppv", "sensitivity", "specificity", "npv")
  naRow <- function(id, name) {
    data.frame(
      phenotypeId = id, phenotypeName = name, cohortDefinitionName = NA_character_, isSummary = TRUE, ppv = NA_real_,
      sensitivity = NA_real_, specificity = NA_real_, npv = NA_real_, stringsAsFactors = FALSE
    )
  }
  medianOrNa <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
  rows <- lapply(phenotypeIds, function(id) {
    found <- evaluations[
      evaluations$phenotypeId == id & evaluations$cdmSourceAbbreviation %in% database &
        evaluations$evaluationType %in% evaluationType, , drop = FALSE
    ]
    found <- found[order(found$cohortDefinitionName), , drop = FALSE]
    name <- phenotypes$phenotypeName[match(id, phenotypes$phenotypeId)]
    if (nrow(found) == 0) {
      return(naRow(id, name))
    }
    cohorts <- data.frame(
      phenotypeId = id, phenotypeName = name, cohortDefinitionName = found$cohortDefinitionName,
      isSummary = nrow(found) == 1, ppv = found$ppv, sensitivity = found$sensitivity,
      specificity = specificity(found$trueNegatives, found$falsePositives),
      npv = negativePredictiveValue(found$trueNegatives, found$falseNegatives),
      stringsAsFactors = FALSE
    )
    if (nrow(cohorts) == 1) {
      return(cohorts)
    }
    summary <- naRow(id, name)
    summary$cohortDefinitionName <- sprintf("Median of %d cohort definitions", nrow(cohorts))
    for (measure in measures) {
      summary[[measure]] <- medianOrNa(cohorts[[measure]])
    }
    rbind(summary, cohorts)
  })
  metrics <- do.call(rbind, rows)
  reference <- metrics[metrics$phenotypeId == baselineId & metrics$isSummary, , drop = FALSE][1, ]
  for (measure in measures) {
    delta <- if (is.na(reference[[measure]])) rep(NA_real_, nrow(metrics)) else 100 * (metrics[[measure]] - reference[[measure]])
    delta[metrics$phenotypeId == baselineId] <- NA_real_
    metrics[[paste0(measure, "Delta")]] <- delta
  }
  metrics
}

# ---- The whole comparison -----------------------------------------------------------------------------------

signedPoints <- function(x) sprintf("%+.1f pts", x)

# Everything the Compare tab and the report show. phenotypeIds are in display order; the baseline is one of them.
assembleComparison <- function(phenotypes, evaluations, conceptSetExpressions, phenotypeIds, baselineId, database, evaluationType,
                               cohortTexts = list()) {
  phenotypeIds <- as.integer(phenotypeIds)
  selected <- phenotypes[match(phenotypeIds, phenotypes$phenotypeId), , drop = FALSE]
  base <- selected[selected$phenotypeId == baselineId, , drop = FALSE]
  baseText <- base$clinicalDefinition
  others <- setdiff(phenotypeIds, baselineId)

  definitionHtml <- setNames(lapply(phenotypeIds, function(id) {
    text <- selected$clinicalDefinition[selected$phenotypeId == id]
    if (id == baselineId) htmltools::htmlEscape(text) else diffHtml(baseText, text)
  }), phenotypeIds)

  baseSets <- conceptSetExpressions[conceptSetExpressions$phenotypeId == baselineId, , drop = FALSE]
  changeTables <- lapply(others, function(id) {
    changes <- compareConceptSets(baseSets, conceptSetExpressions[conceptSetExpressions$phenotypeId == id, , drop = FALSE])
    if (nrow(changes) == 0) {
      return(NULL)
    }
    cbind(phenotypeId = rep(id, nrow(changes)), changes)
  })
  changeTables <- changeTables[!vapply(changeTables, is.null, TRUE)]
  conceptChanges <- if (length(changeTables) == 0) {
    cbind(phenotypeId = integer(0), emptyConceptSetChanges())
  } else {
    do.call(rbind, changeTables)
  }
  conceptChanges$phenotypeName <- selected$phenotypeName[match(conceptChanges$phenotypeId, selected$phenotypeId)]

  metrics <- comparisonMetrics(evaluations, phenotypes, phenotypeIds, baselineId, database, evaluationType)

  baseCohort <- cohortTexts[[as.character(baselineId)]]
  cohortDiffs <- setNames(lapply(others, function(id) {
    other <- cohortTexts[[as.character(id)]]
    if (is.null(baseCohort) || is.null(other)) {
      return(list(available = FALSE, baselineName = if (is.null(baseCohort)) NA_character_ else baseCohort$name,
                  otherName = if (is.null(other)) NA_character_ else other$name))
    }
    list(
      available = TRUE, baselineName = baseCohort$name, otherName = other$name,
      readable = baseCohort$readable && other$readable, diff = cohortLineDiff(baseCohort$text, other$text)
    )
  }), others)

  notes <- vapply(others, function(id) {
    name <- selected$phenotypeName[selected$phenotypeId == id]
    parts <- character(0)
    row <- metrics[metrics$phenotypeId == id & metrics$isSummary, , drop = FALSE][1, ]
    if (is.na(row$cohortDefinitionName)) {
      parts <- c(parts, "no evaluation for the chosen database and evaluation type")
    } else if (all(is.na(c(row$ppvDelta, row$sensitivityDelta)))) {
      parts <- c(parts, "no baseline evaluation to compare with")
    } else {
      measured <- sum(metrics$phenotypeId == id & !metrics$isSummary)
      parts <- c(parts, paste0(
        "PPV ", signedPoints(row$ppvDelta), ", sensitivity ", signedPoints(row$sensitivityDelta),
        if (measured > 1) sprintf(" (median of %d cohort definitions)", measured) else ""
      ))
    }
    changes <- conceptChanges[conceptChanges$phenotypeId == id, , drop = FALSE]
    counts <- table(factor(changes$change, levels = c("Only here", "Only in baseline", "Changed")))
    parts <- c(parts, sprintf(
      "concept sets: %d added, %d removed, %d changed", counts[["Only here"]], counts[["Only in baseline"]], counts[["Changed"]]
    ))
    diff <- wordDiff(baseText, selected$clinicalDefinition[selected$phenotypeId == id])
    if (!is.null(diff)) {
      parts <- c(parts, sprintf("definition text: %d words added, %d removed", sum(diff$op == "add"), sum(diff$op == "del")))
    }
    cohortDiff <- cohortDiffs[[as.character(id)]]
    if (isTRUE(cohortDiff$available) && !is.null(cohortDiff$diff)) {
      parts <- c(parts, sprintf(
        "cohort definition: %d lines added, %d removed", sum(cohortDiff$diff$op == "add"), sum(cohortDiff$diff$op == "del")
      ))
    }
    sprintf("%s: %s", name, paste(parts, collapse = "; "))
  }, "")

  list(
    phenotypes = selected, baselineId = baselineId, database = database, evaluationType = evaluationType,
    definitionHtml = definitionHtml, conceptChanges = conceptChanges, metrics = metrics, cohortDiffs = cohortDiffs,
    notes = unname(notes)
  )
}

# ---- Report -------------------------------------------------------------------------------------------------

percentText <- function(x) ifelse(is.na(x), "", sprintf("%.1f", 100 * x))
deltaText <- function(x) ifelse(is.na(x), "", sprintf("%+.1f", x))

htmlTable <- function(data) {
  htmltools::tags$table(
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(data), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(data)), function(i) {
      htmltools::tags$tr(lapply(data[i, ], function(cell) htmltools::tags$td(as.character(cell))))
    }))
  )
}

# A single self-contained HTML page with the whole comparison, for sharing or printing.
comparisonReport <- function(comparison) {
  tags <- htmltools::tags
  baselineName <- comparison$phenotypes$phenotypeName[comparison$phenotypes$phenotypeId == comparison$baselineId]
  metrics <- comparison$metrics
  metricsTable <- data.frame(
    Phenotype = metrics$phenotypeName,
    "Cohort definition" = ifelse(is.na(metrics$cohortDefinitionName), "No evaluation", metrics$cohortDefinitionName),
    "PPV (%)" = percentText(metrics$ppv), "PPV vs baseline" = deltaText(metrics$ppvDelta),
    "Sensitivity (%)" = percentText(metrics$sensitivity), "Sensitivity vs baseline" = deltaText(metrics$sensitivityDelta),
    "Specificity (%)" = percentText(metrics$specificity), "NPV (%)" = percentText(metrics$npv),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  changes <- comparison$conceptChanges
  changeTable <- data.frame(
    Phenotype = changes$phenotypeName, Change = changes$change, Pertains = changes$pertains, Category = changes$category,
    "Baseline concept set" = changes$baselineName, "Concept set here" = changes$otherName, Details = changes$details,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  style <- "
    body { font-family: Arial, Helvetica, sans-serif; color: #222; max-width: 1100px; margin: 2rem auto; padding: 0 1rem; }
    h1 { font-size: 1.6rem; } h2 { font-size: 1.2rem; margin-top: 2rem; border-bottom: 1px solid #ccc; padding-bottom: .2rem; }
    table { border-collapse: collapse; width: 100%; font-size: .85rem; }
    th, td { border: 1px solid #ccc; padding: .3rem .5rem; text-align: left; vertical-align: top; }
    th { background: #f2f2f2; }
    .definition { border: 1px solid #ccc; padding: .6rem; margin-bottom: 1rem; line-height: 1.5; }
    ins.diff-add { background: #d4edda; text-decoration: none; }
    del.diff-del { background: #f8d7da; }
    .muted { color: #666; font-size: .85rem; }
    .cohort-diff { font-family: monospace; font-size: .8rem; border: 1px solid #ccc; margin-bottom: 1rem; }
    .cohort-diff div { padding: 0 .4rem; white-space: pre-wrap; }
    .line-add { background: #d4edda; } .line-del { background: #f8d7da; } .line-skip { color: #666; background: #f2f2f2; font-style: italic; }"
  page <- tags$html(
    tags$head(tags$meta(charset = "utf-8"), tags$title("Pegasus phenotype comparison"), tags$style(htmltools::HTML(style))),
    tags$body(
      tags$h1("Phenotype comparison"),
      tags$p(class = "muted", sprintf(
        "Created %s. Baseline: %s. Performance: %s, %s.", format(Sys.time(), "%Y-%m-%d %H:%M"), baselineName,
        if (length(comparison$database) == 0) "no database" else comparison$database,
        if (length(comparison$evaluationType) == 0) "no evaluation type" else comparison$evaluationType
      )),
      tags$h2("Summary of differences from the baseline"),
      if (length(comparison$notes) > 0) tags$ul(lapply(comparison$notes, tags$li)) else tags$p("Nothing to compare."),
      tags$h2("Performance"),
      htmlTable(metricsTable),
      tags$p(class = "muted", "A phenotype with several cohort definitions shows their median. Differences are percentage points against the baseline's median."),
      tags$h2("Definitions"),
      tags$p(class = "muted", "Compared with the baseline: added words are highlighted green, removed words red and struck through."),
      lapply(comparison$phenotypes$phenotypeId, function(id) {
        name <- comparison$phenotypes$phenotypeName[comparison$phenotypes$phenotypeId == id]
        htmltools::tagList(
          tags$h3(if (id == comparison$baselineId) paste0(name, " (baseline)") else name),
          tags$div(class = "definition", htmltools::HTML(comparison$definitionHtml[[as.character(id)]]))
        )
      }),
      tags$h2("Cohort definitions"),
      tags$p(class = "muted", "Compared line by line with the baseline's cohort definition: green lines (+) were added, red lines (-) removed."),
      if (length(comparison$cohortDiffs) == 0) tags$p("Nothing to compare."),
      lapply(names(comparison$cohortDiffs), function(id) {
        entry <- comparison$cohortDiffs[[id]]
        name <- comparison$phenotypes$phenotypeName[comparison$phenotypes$phenotypeId == as.integer(id)]
        htmltools::tagList(
          tags$h3(name),
          if (!isTRUE(entry$available)) {
            tags$p("A cohort definition is missing for this comparison.")
          } else {
            htmltools::tagList(
              tags$p(class = "muted", sprintf("%s compared with %s.%s", entry$otherName, entry$baselineName,
                                              if (isTRUE(entry$readable)) "" else " The readable description was not available, so the Circe JSON is compared.")),
              tags$div(class = "cohort-diff", htmltools::HTML(lineDiffHtml(entry$diff, contextLines = 2)))
            )
          }
        )
      }),
      tags$h2("Concept sets"),
      if (nrow(changeTable) > 0) htmlTable(changeTable) else tags$p("No concept set differences.")
    )
  )
  paste0("<!DOCTYPE html>\n", as.character(page))
}
