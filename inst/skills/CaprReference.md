# Capr 2.1.1 R Package — LLM Reference

Capr serializes OHDSI cohort definitions to Atlas-compatible JSON. 

## 1. Core Architecture & Building Blocks

```text
Cohort()
├── CohortEntry()     <- Index event(s) + prior/post observation limits
├── CohortAttrition() <- Named inclusion/exclusion rule groups
├── CohortExit()      <- End strategy + optional censoring
└── CohortEra()       <- Era-collapse padding + study window
```

* **Query**: Event from a CDM domain (e.g., `conditionOccurrence(cs)`). Used for entry, inside criteria, or censoring.
* **Criteria**: Query + counting logic + time window (`atLeast(1, query, aperture)`).
* **Group**: Logical wrapper for Criteria/Groups (`withAll()`, `withAny()`). *Attrition rules must be Groups.*
* **EventAperture**: Time window for Criteria (`duringInterval(eventStarts(a, b))`).
* **Concept Sets**: *Must* be generated via the `create_concept_set` tool. 

## 2. Hard Rules & Agent Workflow

* **Explicit Defaults**: Always define `primaryCriteriaLimit` in `entry()`, `expressionLimit` in `attrition()` (matches `primaryCriteriaLimit` unless specifically requested otherwise), and `exit()` (even if just `observationExit()`).
* **Define Index in Entry, Screen in Attrition**: Filter the *qualifying index event* inside `entry()` using attributes or `nestedWithAll()`. Use `attrition()` only to screen candidates based on history/demographics. Applying index filters in `attrition()` with `primaryCriteriaLimit="First"` drops patients instead of finding their first qualifying event.
* **Out-of-Scope (Propose SQL instead)**: Capr *cannot* do: Cohort intersections/unions, cross-event math (dose changes, value comparisons), aggregate math (sums/averages), or complex ordinal sequences (> 1st occurrence). 
* **UI Hydration**: Concept/ID attributes optionally take `connection` & `vocabularyDatabaseSchema` to fetch names for the Atlas UI. Recommend this to users with DB access.

## 3. API Reference

### Top-Level & Components

| Function | Notes |
| :--- | :--- |
| `cohort(entry, attrition, exit, era)` | Returns `Cohort`. Pass explicit defaults. |
| `entry(..., observationWindow, primaryCriteriaLimit)` | `...` = OR'd `Query` objects. Limit = `"First"`, `"All"`, `"Last"`. Avoid `additionalCriteria`/`qualifiedLimit`. |
| `continuousObservation(priorDays, postDays)` | Returns `ObservationWindow`. Default `0L`. |
| `attrition(..., expressionLimit)` | `...` = named `Group` objects (e.g., `'Rule 1' = withAll(...)`). |
| `exit(endStrategy, censor)` | Default `observationExit()`. `censor` = `censoringEvents(...)`. |
| `era(eraDays, studyStartDate, studyEndDate)` | Default `0L`. Dates must be `Date` objects. |

### End Strategies (for `exit`)

* `observationExit()`: End of continuous observation.
* `fixedExit(index="startDate"|"endDate", offsetDays)`: Fixed days after index.
* `drugExit(conceptSet, persistenceWindow=0L, surveillanceWindow=0L, daysSupplyOverride=NULL)`: End of drug era.

### Queries (Requires `ConceptSet` unless noted)

`conditionOccurrence`, `drugExposure`, `measurement`, `observation`, `procedure`, `visit` (ConceptSet defines visit *type/setting*, e.g., inpatient), `deviceExposure`, `specimen`, `conditionEra`, `drugEra`, `doseEra`.
*Special:* `death(conceptSet=NULL, ...)`, `observationPeriod(...)` (No ConceptSet).

### Criteria & Assessment Windows

* **Criteria**: `exactly(x, query, aperture)`, `atLeast(...)`, `atMost(...)`. (Use `exactly(0, ...)` for exclusions).
* **Aperture**: `duringInterval(startWindow, endWindow, restrictVisit=F, ignoreObservationPeriod=F)`
* **Windows**: `eventStarts(a, b, index="startDate"|"endDate")` / `eventEnds(...)`
  * *Signs*: `-Inf` (all time before), `<0` (before), `0` (index date), `>0` (after), `Inf` (all time after).
  * *Overlap Index*: `duringInterval(eventStarts(-Inf, 0), eventEnds(0, Inf))`

### Groups & Nested Criteria

* **Groups (Attrition/Logic)**: `withAll(...)`, `withAny(...)`, `withAtLeast(x, ...)`, `withAtMost(x, ...)`.
* **Nested (Correlated to Query Event Date)**: `nestedWithAll(...)`, `nestedWithAny(...)` etc. Pass as attribute in Query.

### Operators & Attributes

* **Operators**: `lt(x)`, `lte(x)`, `gt(x)`, `gte(x)`, `eq(x)`, `bt(x, y)`, `nbt(x, y)`. Types strict (`18L` integer, `18.0` numeric, `as.Date("...")`).
* **Numeric/Date Attributes**: `age(op)`, `daysOfSupply(op)`, `valueAsNumber(op)`, `startDate(op)`, `endDate(op)`.
* **Misc Attributes**:
  * `firstOccurrence()` (Incident/new-user cohort).
  * `valueAsConcept(ids)`, `valueAsConceptSet(cs)`, `valueAsString(text, op="contains")`.
  * `measurementUnit(ids)`: Accepts exactly one `ids` argument containing an integer vector, not multiple positional IDs, strings, or ConceptSets. Use `measurementUnit(c(0L, 8523L, 32912L))`, not `measurementUnit(0L, 8523L, 32912L)`. Extra positional arguments may be matched to optional database-connection parameters and cause misleading errors such as `dbIsValid` for an integer.
* **Provenance/Type IDs**: `conditionType(ids)`, `visitType(ids)` (Warning: on non-visit domains, filters the linked visit's setting; on `visit()` it filters provenance, NOT setting). Exclude via `...TypeExclude(exclude=TRUE)`.
* **Source Concepts**: `conditionSourceConcept(cs)`, `drugSourceConcept(cs)`, etc.
* **Demographics (Use directly in Group)**: `male()`, `female()`, `genderConcepts(ids)`, `age(op)`.

### Measurement Attribute Example

```r
measurement(
  cs_inr,
  valueAsNumber(gte(1.5)),
  measurementUnit(c(0L, 8523L, 32912L))
)
```

When filtering measurements:

* Keep numeric thresholds in `valueAsNumber()`, not in `measurementUnit()`.
* Pass unit concept IDs as one integer vector. Preserve the `L` suffix so IDs are integers.
* Use unit concept IDs observed in the target database, for example from `describeMeasurementValues`; do not infer IDs from unit names.
* A missing unit is represented by concept ID `0L` when the database diagnostics justify including it.

## 4. Anti-Patterns (Compiles, but silently wrong)

1. **Bare Criteria in `attrition()`**: Will vanish in UI. MUST wrap in a Group (`'rule' = withAll(atLeast(...))`).
2. **Qualifying Index in `attrition()`**: With `primaryLimit="First"`, dropping failing events in attrition removes the patient. Apply index filters via Query attributes/`nestedWithAll`. 
3. **Using `visitType()` for Care Setting on `visit()`**: `visit_type_concept_id` is provenance. Put care setting in the ConceptSet. (Allowed on `conditionOccurrence`, etc., where it joins to `visit_concept_id`).

## 5. Quick Reference & Execution

```r
library(Capr)

cd <- cohort(
  entry = entry(
    conditionOccurrence(cs_t2dm, firstOccurrence()), # incident 
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    "Adult Males" = withAll(male(), age(gte(18L))),
    "No prior T1DM" = withAll(
      exactly(0, conditionOccurrence(cs_t1dm), duringInterval(eventStarts(-Inf, 0)))
    ),
    "Has Lab tied to Drug" = withAll(
      atLeast(1, drugExposure(cs_metformin, 
        nestedWithAll(atLeast(1, measurement(cs_hba1c), aperture = duringInterval(eventStarts(-30, 0))))
      ), duringInterval(eventStarts(-Inf, Inf)))
    ),
    expressionLimit = "First"
  ),
  exit = exit(endStrategy = observationExit())
)
```
