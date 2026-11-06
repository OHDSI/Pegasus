---
name: cohort-developer
description: >
  Given a clinical definition of a phenotype, develop an executable cohort definition in an iterative process of conceptual design, implementation in Capr, and evaluation. 
  Use when the user asks to develop a phenotype algorithm or cohort definition.
---

# Develop a cohort for a given phenotype

Based on the Clinical Definition of a phenotype, develop an operational definition that can be executed against a database in the OMOP Common Data Model (CDM). The implementation uses the Capr R package. The `validateCapr`, `generateCohort`, and `convertCaprToJson` tools compile the Capr definition to OMOP JSON **server-side**, so you submit Capr R code — not JSON — to them.

Your aim is to achieve greater than 80% PPV and sensitivity as measured by KEEPER.

## Background
Data in observational healthcare databases (insurance claims, electronic health records) are not collected for research purposes. Important variables, such as health outcomes of interest, must be inferred using operational definitions. These definitions contain concept sets (OMOP vocabulary concept IDs) for diagnoses, procedures, measurements, drugs, and visit types, along with the temporal logic to combine them.  

## Prerequisites
- Requires a **Clinical Definition** of the phenotype, describing the clinical intent (the "what"). If no clinical definition is present, invoke the  `clinical-definition-refiner` skill. Resume this workflow only after it returns a clinical definition. Do not independently invent missing clinical criteria.
- Requires the **phenotype name**. If not provided, derive it from the clinical definition.
- **Reference Material:** Read `CAPR_REFERENCE.md` (in the same folder as this file) to understand the exact structure and syntax of Capr cohort definitions in R.

## Agent Workflow

### Phase 1: Conceptual Design
1. **Retrieve available concept sets:** Use the `listConceptSets` tool to identify relevant OMOP concept sets for the phenotype and their person counts. Concept sets with 0 counts are likely unhelpful. Some concept sets are marked as not with descendants. These usually contain a broader concept than the phenotype itself, and might be used to capture the disease before its subtype is diagnosed. 
2. **Apply Clinical & Database Knowledge:** Develop an initial best-guess cohort definition based on clinical reality and your knowledge of EHRs and Claims data. Outline the required concept sets and temporal logic. Use `getConceptSetsCapr` to fetch Capr `cs(...)` code and per-domain person counts for the pre-created concept sets you need. Use the `describeMeasurementValues` tool to fetch units and corresponding value distributions for measurements.

### Phase 2: Implementation, Generation and Diagnostics (Unlimited Attempts)
*You may iterate through this phase as many times as needed to get reasonable diagnostics before proceeding to KEEPER evaluations.*
0. **Temporal Concept Set Overlap:** When further refining an *existing* cohort definition, use the `countConceptSetPersonOverlap` tool to assess the potential impact of adding or removing concept sets from the cohort definition.
1. **Implementation:** Write the R code using the Capr package to define the cohort (see `CAPR_REFERENCE.md`). 
	- **Submission format (required):** `caprCode` must be a **single `cohort(...)` expression** with every concept set **inlined** as the first argument of its domain query, and **no assignments or helper variables**. The tool compiles it in an isolated sandbox that rejects anything outside the documented Capr API. Do **not** pass JSON. You can use the `validateCapr` tool to validate the code if needed.
	- Each `cs(...)` snippet from `getConceptSetsCapr` needs a `name = "..."` added when you inline it.
2. **Cohort Generation:** Pass the **Capr cohort definition as R code** to the `generateCohort` tool to instantiate the cohort in the database. This tool returns a **cohort ID**.
3. **Count Verification:** Call the `getCohortCount` tool using the returned cohort ID to get cohort counts (split by inclusion rules). Verify whether the counts are reasonable (e.g. overall count is not 0, and attrition is as expected). 
4. **Incidence Rate Verification:** Call the `computeIncidenceRate` tool to compute the cohort incidence rate (both unstratified and stratified by age or sex).  Verify it meets expectations for the phenotype.
5. **Temporal Concept Set Overlap:** Use the `countConceptSetPersonOverlap` tool to assess concept sets whose inclusion, exclusion, domain, or temporal role remains uncertain.

Before calling a diagnostic tool, state the expected direction or plausible qualitative range and its rationale. Do not invent precise epidemiologic benchmarks unless they are supplied or retrieved from an authoritative source.

### Phase 3: KEEPER Evaluation (MAXIMUM 3 ATTEMPTS)
*To strictly avoid overfitting to the reference set, you are limited to a maximum of 3 calls to `evaluateCohort`.*

Proceed to KEEPER evaluation only after:
- The generated cohort has a clinically plausible count and attrition profile.
- Important concept-set choices have been examined with temporally aligned overlap windows.
- The incidence rates (stratified and unstratified) are within expectations.
- Obvious concept-set, domain, and timing problems have been addressed without reference to KEEPER labels.

1. **Evaluate:** Call the `evaluateCohort` tool using the cohort ID to get a summary of the cohort's operating characteristics against the KEEPER reference set. 
2. **Patient Profiling:** To understand the performance, call the `samplePatientProfile` tool to review individual patient profiles.
3. **Refine or Terminate:** Adjust the cohort definition based on evaluation results. Return to Phase 2 to regenerate the cohort. You must **STOP** after evaluateCohort has been called three times in total during the skill invocation.

## Final Output
Present the user with the final Capr R code and a summary of the evaluation results. If the user wants the OMOP JSON, produce it with the `convertCaprToJson` tool, saving it to file immediately. Do *not* verify the content of the JSON file.

## Heuristics for Initial Design
Think about how the phenotype plays out in a real-world healthcare setting:
* **EHR vs. Claims:** How does the data capture differ? (e.g., Claims will have precise billing diagnoses but may lack lab results; EHRs will have rich clinical measurements but may have missing data if the patient went out of network). Build logic that bridges these gaps.
* **Patient Journey:** What interactions would the patient have with the healthcare system before, during, and after onset? 
* **Operational Accuracy:** What operational definition would accurately reflect the phenotype as described in the Clinical Definition? Leverage Capr's structure to balance logic. 
* **Only necessary constraints:** Only include criteria - especially prior/post-observation windows, washout periods, or age/enrollment - that are grounded in the clinical definition or user request.

filters 

## Capr Rules
1. **Use only functions and arguments documented in `CAPR_REFERENCE.md`.** If something seems missing, say so — do not improvise API.
2. **Never write a concept ID from memory.** This includes clinical concepts and type/unit/status/provider-specialty IDs. Use the Hecate tools if needed to find individual concepts.
3. Concept sets **must** be constructed using the `getConceptSetsCapr` tool. This tool generates Capr code that can be added to the overall code. 
4. **Say so when the cohort is not expressible in Capr/Circe.** Check every request against the wrong-tool signals in `CAPR_REFERENCE.md` before writing code. A definition that compiles but means something different from what the user asked for is worse than no code — never deliver a silent approximation; state the mismatch and propose the decomposition pattern instead.
