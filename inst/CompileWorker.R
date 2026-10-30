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

# Credential-less Capr compile worker (Option A1 + compile-worker split).
#
# This file is the ONLY place client-authored text is interpreted. It accepts a Capr
# cohort-definition as R *text*, validates it against a strict allow-list by walking the
# parse tree (it never eval()s the raw string), evaluates only allow-listed Capr calls in
# an environment that contains nothing else, and returns the compiled Circe JSON.
#
# SECURITY CONTRACT:
#   * This file MUST NOT load DatabaseConnector / keyring / any credential, and MUST NOT
#     open network or database connections. It is invoked in a separate, fresh R process
#     (see compileCaprViaWorker() in server.R) so client code never runs in the process
#     that holds the CDM credentials.
#   * The R-level allow-list below is defense-in-depth, NOT a complete sandbox. In
#     production (RStudio Connect) run this worker in an OS sandbox: no network egress,
#     read-only filesystem, non-root UID, dropped capabilities, and CPU/memory/wall-clock
#     limits. The credential-holding "CDM runner" must only ever receive the compiled JSON
#     (data), never client code.

# Allow-list of function names the client expression may call. Kept in sync with
# CAPR_REFERENCE.md; anything not listed here is rejected before evaluation.
caprAllowedFunctions <- c(
  # top-level assembly
  "cohort", "entry", "attrition", "exit", "era",
  "continuousObservation", "censoringEvents",
  # end strategies
  "observationExit", "fixedExit", "drugExit",
  # concept sets
  "cs", "descendants", "exclude",
  # domain queries
  "conditionOccurrence", "conditionEra", "drugExposure", "drugEra", "doseEra",
  "measurement", "observation", "procedure", "visit", "visitDetail",
  "deviceExposure", "specimen", "death", "observationPeriod",
  # criteria
  "exactly", "atLeast", "atMost",
  # groups
  "withAll", "withAny", "withAtLeast", "withAtMost",
  # nested (correlated) criteria
  "nestedWithAll", "nestedWithAny", "nestedWithAtLeast", "nestedWithAtMost",
  # apertures / windows
  "duringInterval", "eventStarts", "eventEnds",
  # comparison operators
  "lt", "lte", "gt", "gte", "eq", "bt", "nbt",
  # numeric / date / logic attributes
  "age", "daysOfSupply", "drugRefills", "drugQuantity", "valueAsNumber",
  "rangeHigh", "rangeLow", "rangeHighRatio", "occurrenceCount", "eraLength", "doseValue",
  "startDate", "endDate", "dateAdjustment", "firstOccurrence",
  # type / status / source / value attributes
  "conditionType", "conditionStatus", "drugType", "visitType", "measurementType",
  "observationType", "procedureType", "deathType", "deviceType", "specimenType",
  "observationPeriodType", "conditionTypeExclude", "measurementTypeExclude",
  "deathTypeExclude", "specimenTypeExclude",
  "conditionSourceConcept", "drugSourceConcept", "procedureSourceConcept",
  "observationSourceConcept", "measurementSourceConcept", "visitSourceConcept",
  "visitDetailSourceConcept",
  "valueAsConcept", "valueAsConceptSet", "valueAsString", "measurementUnit",
  "providerSpecialtyConcepts",
  # demographics
  "male", "female", "genderConcepts",
  # base helpers needed to build argument values (literals only, see validator)
  "c", "list", "as.Date", "as.integer", "as.numeric"
)

# Operators/grouping permitted (e.g. `-Inf`, negative literals, grouping parens).
caprAllowedOperators <- c("-", "+", "(")

# Symbols permitted as bare values.
caprAllowedSymbols <- c("Inf", "NaN", "NA", "TRUE", "FALSE", "T", "F",
                        "NA_integer_", "NA_real_", "NA_character_")

# Recursively validate a parsed expression against the allow-list. Throws on the first
# disallowed construct. Parsing (done by the caller) does not execute code; this walk runs
# before any evaluation.
validateCaprAst <- function(node) {
  if (is.call(node)) {
    head <- node[[1]]
    if (!is.symbol(head)) {
      stop("Only direct named function calls are allowed; got: ", deparse(head))
    }
    fname <- as.character(head)
    if (!(fname %in% c(caprAllowedFunctions, caprAllowedOperators))) {
      stop("Disallowed function call: ", fname)
    }
    for (arg in as.list(node)[-1]) validateCaprAst(arg)
  } else if (is.symbol(node)) {
    sym <- as.character(node)
    if (nzchar(sym) && !(sym %in% caprAllowedSymbols)) {
      stop("Disallowed symbol: ", sym)
    }
  } else if (is.numeric(node) || is.character(node) || is.logical(node) || is.null(node)) {
    # literal / NULL — allowed
  } else {
    stop("Disallowed token of type: ", typeof(node))
  }
  invisible(TRUE)
}

# Build the restricted evaluation environment: only allow-listed names are bound, and
# parent = emptyenv() so nothing else (system, eval, get, ...) can resolve even if the
# validator missed something.
buildCaprEvalEnv <- function() {
  caprNs <- asNamespace("Capr")
  bindings <- list()
  for (fn in caprAllowedFunctions) {
    if (exists(fn, envir = caprNs, inherits = FALSE)) {
      bindings[[fn]] <- get(fn, envir = caprNs, inherits = FALSE)
    } else if (exists(fn, envir = baseenv(), inherits = FALSE)) {
      bindings[[fn]] <- get(fn, envir = baseenv(), inherits = FALSE)
    }
  }
  bindings[["-"]] <- base::`-`
  bindings[["+"]] <- base::`+`
  bindings[["("]] <- base::`(`
  bindings[["Inf"]] <- Inf
  list2env(bindings, parent = emptyenv())
}

# Public entry point. Validate + compile a single Capr cohort expression to Circe JSON.
validateAndCompileCapr <- function(caprCode, maxChars = 100000L) {
  if (!is.character(caprCode) || length(caprCode) != 1L || is.na(caprCode)) {
    stop("caprCode must be a single non-NA string")
  }
  if (nchar(caprCode) > maxChars) {
    stop("caprCode exceeds the ", maxChars, "-character size limit")
  }
  exprs <- parse(text = caprCode, keep.source = FALSE)
  if (length(exprs) != 1L) {
    stop("Submit exactly one Capr expression: a single cohort(...) call with concept sets ",
         "inlined and no assignments.")
  }
  expr <- exprs[[1]]
  validateCaprAst(expr)

  cohortObj <- eval(expr, envir = buildCaprEvalEnv())
  if (!methods::is(cohortObj, "Cohort")) {
    stop("Expression did not evaluate to a Capr Cohort object")
  }
  Capr::compile(cohortObj)
}

# Public entry point for one or more standalone Capr concept-set expressions.
validateAndCompileConceptSets <- function(caprCode, maxChars = 100000L) {
  if (!is.character(caprCode) || length(caprCode) < 1L || anyNA(caprCode)) {
    stop("caprCode must contain one or more non-NA strings")
  }
  if (any(nchar(caprCode) > maxChars)) {
    stop("A caprCode value exceeds the ", maxChars, "-character size limit")
  }

  lapply(caprCode, function(code) {
    exprs <- parse(text = code, keep.source = FALSE)
    if (length(exprs) != 1L) {
      stop("Each value must contain exactly one Capr cs(...) expression")
    }
    expr <- exprs[[1]]
    validateCaprAst(expr)

    conceptSet <- eval(expr, envir = buildCaprEvalEnv())
    if (!methods::is(conceptSet, "ConceptSet")) {
      stop("Expression did not evaluate to a Capr ConceptSet object")
    }
    list(
      name = conceptSet@Name,
      json = Capr::toConceptSetJson(conceptSet)
    )
  })
}
