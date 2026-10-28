CREATE SCHEMA @new_schema;

CREATE TABLE @new_schema.phenotype (
  phenotype_id INT PRIMARY KEY,
  phenotype_name VARCHAR NOT NULL UNIQUE,
  clinical_definition VARCHAR NOT NULL,
  creator VARCHAR NOT NULL,
  time_created TIMESTAMP NOT NULL,
  draft INT NOT NULL,
  valid INT NOT NULL
);

-- Records that a phenotype was created by cloning (refining) another one
CREATE TABLE @new_schema.phenotype_ancestor (
  phenotype_id INT NOT NULL,
  parent_phenotype_id INT NOT NULL
);

CREATE TABLE @new_schema.phenotype_meta_data (
  phenotype_id INT PRIMARY KEY,
  is_pharmacovigilance_outcome VARCHAR, -- 'CONDITIONAL', 'NO', or 'YES'
  clinical_course VARCHAR, -- 'TRANSIENT_RECURRENT', 'PERSISTENT_STABLE', 'PERSISTENT_RELAPSING', or 'TRANSIENT_SINGLE'
  expected_care_setting VARCHAR, --'ACUTE_CARE_COMMON', 'OUTPATIENT_EXPECTED', or 'ACUTE_CARE_EXPECTED'
  sex_constraint VARCHAR, -- 'BOTH_SEXES', 'FEMALE_ONLY', or 'MALE_ONLY'
  min_age_years INT,
  max_age_years INT,
  minimum_interepisode_gap_days INT,
  has_discrete_recorded_symptoms INT,
  requires_diagnostic_test_or_procedure INT,
  requires_active_treatment_within_30d INT,
  expects_condition_specific_followup_or_sequelae_1yr INT
);

CREATE TABLE @new_schema.draft_concept_set_target (
  phenotype_id INT NOT NULL,
  pertains VARCHAR NOT NULL, -- 'doi' or 'alternative diagnoses'
  category VARCHAR NOT NULL, -- 'doi', 'symptom', 'diagnostic procedure', etc.
  term VARCHAR NOT NULL,
  concept_id INT NOT NULL,
  concept_name VARCHAR NOT NULL,
  domain_id VARCHAR NOT NULL,
  concept_class_id VARCHAR NOT NULL,
  vocabulary_id VARCHAR NOT NULL,
  match_type VARCHAR NOT NULL -- verbatim or vector
);

-- May still change
CREATE TABLE @new_schema.draft_target_to_concept_set (
  phenotype_id INT NOT NULL,
  pertains VARCHAR NOT NULL, -- 'doi' or 'alternative diagnoses'
  category VARCHAR NOT NULL, -- 'doi', 'symptom', 'diagnostic procedure', etc.
  concept_id INT NOT NULL,
  concept_set_id INT NOT NULL
);

CREATE TABLE @new_schema.concept_set (
  concept_set_id INT PRIMARY KEY,
  concept_set_name VARCHAR NOT NULL,
  circe_concept_set_expression VARCHAR NOT NULL,
  capr_concept_set_expression VARCHAR,
  has_descendants INT NOT NULL,
  clinical_definition VARCHAR,
  concept_id INT,
  valid INT NOT NULL,
  atlas_concept_set_id INT,
  cs_source_type VARCHAR -- 'From human definition', 'From AI definition', 'Concept + descendants', 'Concept', or 'User'
);

-- Flattened (unexpanded) items of the Circe concept set expressions, used to find concept sets by concept
CREATE TABLE @new_schema.concept_set_item (
  concept_set_id INT NOT NULL,
  concept_id INT NOT NULL,
  is_excluded INT NOT NULL,
  include_descendants INT NOT NULL,
  include_mapped INT NOT NULL
);

CREATE TABLE @new_schema.phenotype_to_concept_set (
  phenotype_id INT NOT NULL,
  pertains VARCHAR NOT NULL, -- 'Disease of interest' or 'Alternative diagnoses'
  category VARCHAR NOT NULL, -- 'Hypernym', 'Disease of interest', 'Clinical presentation', etc.
  concept_set_id INT NOT NULL
);

CREATE TABLE @new_schema.concept_set_person_count (
  concept_set_id INT NOT NULL,
  database_id INT NOT NULL,
  condition_persons INT NOT NULL,
  procedure_persons INT NOT NULL,
  drug_persons INT NOT NULL,
  device_persons INT NOT NULL,
  measurement_persons INT NOT NULL,
  observation_persons INT NOT NULL,
  visit_persons INT NOT NULL,
  overall_persons INT NOT NULL
);

CREATE TABLE @new_schema.cohort_definition (
  cohort_definition_id INT PRIMARY KEY,
  cohort_definition_name VARCHAR NOT NULL UNIQUE,
  phenotype_id INT NOT NULL,
  circe_cohort_expression VARCHAR NOT NULL,
  capr_cohort_expression VARCHAR,
  cd_source_type VARCHAR NOT NULL, -- 'Templates', 'User', 'MonoFlow', 'PhenotypeDesigner'
  valid INT NOT NULL,
  atlas_cohort_definition_id INT
);

CREATE TABLE @new_schema.cohort_definition_to_concept_set (
  cohort_definition_id INT NOT NULL,
  concept_set_id INT NOT NULL
);

CREATE TABLE @new_schema.cohort_definition_evaluation (
  cohort_definition_id INT NOT NULL,
  database_id INT NOT NULL,
  cohort_entries INT NOT NULL,
  cohort_subjects INT NOT NULL,
  generation_seconds INT NOT NULL,
  true_positives INT NOT NULL,
  false_positives INT NOT NULL,
  true_negatives INT NOT NULL,
  false_negatives INT NOT NULL,
  ppv FLOAT,
  ppv_lb FLOAT,
  ppv_ub FLOAT,
  sensitivity FLOAT,
  sensitivity_lb FLOAT,
  sensitivity_ub FLOAT,
  specificity FLOAT,
  specificity_lb FLOAT,
  specificity_ub FLOAT,
  npv FLOAT,
  npv_lb FLOAT,
  npv_ub FLOAT,
  evaluation_type VARCHAR -- 'KEEPER + llm', 'PheValuator', or 'KEEPER + human'
);

-- Evaluation runs requested through the app. A request is complete once a matching
-- cohort_definition_evaluation row (same cohort, database and evaluation type) exists.
CREATE TABLE @new_schema.evaluation_request (
  evaluation_request_id INT PRIMARY KEY,
  cohort_definition_id INT NOT NULL,
  database_id INT NOT NULL,
  evaluation_type VARCHAR NOT NULL, -- 'KEEPER + llm', 'PheValuator', or 'KEEPER + human'
  requested_by VARCHAR NOT NULL,
  time_requested TIMESTAMP NOT NULL,
  notes VARCHAR
);

CREATE TABLE @new_schema.database_meta_data (
  database_id INT PRIMARY KEY,
  cdm_source_name VARCHAR NOT NULL,
  cdm_source_abbreviation VARCHAR NOT NULL,
  cdm_release_date DATE NOT NULL,
  cdm_release_identifier VARCHAR,
  vocabulary_version VARCHAR(20) NOT NULL,
  cdm_database_schema VARCHAR,
  result_database_schema VARCHAR
);

-- Has one row
CREATE TABLE @new_schema.atlas_instance (
  base_url VARCHAR NOT NuLL
);

-- ---------------------------------------------------------------------------------------------------------------
-- Part 2: copy the tables of the original model.
-- ---------------------------------------------------------------------------------------------------------------

INSERT INTO @new_schema.phenotype
SELECT phenotype_id, phenotype_name, clinical_definition, creator, time_created, draft, valid
FROM @old_schema.phenotype;

INSERT INTO @new_schema.concept_set
SELECT concept_set_id,
  concept_set_name,
  circe_concept_set_expression,
  capr_concept_set_expression,
  has_descendants,
  clinical_definition,
  concept_id,
  valid,
  atlas_concept_set_id,
  UPPER(SUBSTRING(cs_source_type, 1, 1)) || SUBSTRING(cs_source_type, 2)
FROM @old_schema.concept_set;

INSERT INTO @new_schema.phenotype_to_concept_set
SELECT phenotype_id,
  CASE LOWER(pertains)
    WHEN 'doi' THEN 'Disease of interest'
    WHEN 'alternative diagnoses' THEN 'Alternative diagnoses'
    ELSE UPPER(SUBSTRING(pertains, 1, 1)) || SUBSTRING(pertains, 2)
  END,
  CASE LOWER(category)
    WHEN 'doi' THEN 'Disease of interest'
    ELSE UPPER(SUBSTRING(REPLACE(category, '_', ' '), 1, 1)) || LOWER(SUBSTRING(REPLACE(category, '_', ' '), 2))
  END,
  concept_set_id
FROM @old_schema.phenotype_to_concept_set;

INSERT INTO @new_schema.draft_concept_set_target
SELECT phenotype_id,
  CASE LOWER(pertains)
    WHEN 'doi' THEN 'Disease of interest'
    WHEN 'alternative diagnoses' THEN 'Alternative diagnoses'
    ELSE UPPER(SUBSTRING(pertains, 1, 1)) || SUBSTRING(pertains, 2)
  END,
  CASE LOWER(category)
    WHEN 'doi' THEN 'Disease of interest'
    ELSE UPPER(SUBSTRING(REPLACE(category, '_', ' '), 1, 1)) || LOWER(SUBSTRING(REPLACE(category, '_', ' '), 2))
  END,
  term, concept_id, concept_name, domain_id, concept_class_id, vocabulary_id, match_type
FROM @old_schema.draft_concept_set_target;

INSERT INTO @new_schema.draft_target_to_concept_set
SELECT phenotype_id,
  CASE LOWER(pertains)
    WHEN 'doi' THEN 'Disease of interest'
    WHEN 'alternative diagnoses' THEN 'Alternative diagnoses'
    ELSE UPPER(SUBSTRING(pertains, 1, 1)) || SUBSTRING(pertains, 2)
  END,
  CASE LOWER(category)
    WHEN 'doi' THEN 'Disease of interest'
    ELSE UPPER(SUBSTRING(REPLACE(category, '_', ' '), 1, 1)) || LOWER(SUBSTRING(REPLACE(category, '_', ' '), 2))
  END,
  concept_id, concept_set_id
FROM @old_schema.draft_target_to_concept_set;

INSERT INTO @new_schema.concept_set_person_count
SELECT concept_set_id, database_id, condition_persons, procedure_persons, drug_persons, device_persons,
  measurement_persons, observation_persons, visit_persons, overall_persons
FROM @old_schema.concept_set_person_count;

INSERT INTO @new_schema.cohort_definition
SELECT cohort_definition_id,
  cohort_definition_name,
  phenotype_id,
  circe_cohort_expression,
  capr_cohort_expression,
  CASE LOWER(pa_source_type)
    WHEN 'template' THEN 'Templates'
    WHEN 'ai' THEN 'MonoFlow'
    WHEN 'user' THEN 'User'
    WHEN 'human' THEN 'User'
    ELSE pa_source_type
  END,
  valid,
  atlas_cohort_definition_id
FROM @old_schema.cohort_definition;

INSERT INTO @new_schema.cohort_definition_to_concept_set
SELECT cohort_definition_id, concept_set_id
FROM @old_schema.cohort_definition_to_concept_set;

INSERT INTO @new_schema.cohort_definition_evaluation
SELECT cohort_definition_id, database_id, cohort_entries, cohort_subjects, generation_seconds,
  true_positives, false_positives, true_negatives, false_negatives,
  ppv, ppv_lb, ppv_ub, sensitivity, sensitivity_lb, sensitivity_ub,
  NULL, NULL, NULL, NULL, NULL, NULL,
  CASE LOWER(evaluation_type)
    WHEN 'keeper + llm' THEN 'KEEPER + llm'
    WHEN 'phevaluator' THEN 'PheValuator'
    WHEN 'keeper + human' THEN 'KEEPER + human'
    ELSE evaluation_type
  END
FROM @old_schema.cohort_definition_evaluation;

INSERT INTO @new_schema.database_meta_data
SELECT database_id, cdm_source_name, cdm_source_abbreviation, cdm_release_date, cdm_release_identifier,
  vocabulary_version, cdm_database_schema, result_database_schema
FROM @old_schema.database_meta_data;

INSERT INTO @new_schema.atlas_instance
SELECT base_url
FROM @old_schema.atlas_instance;

SELECT 'phenotype' AS table_name, (SELECT COUNT(*) FROM @old_schema.phenotype) AS old_rows, (SELECT COUNT(*) FROM @new_schema.phenotype) AS new_rows
UNION ALL SELECT 'concept_set', (SELECT COUNT(*) FROM @old_schema.concept_set), (SELECT COUNT(*) FROM @new_schema.concept_set)
UNION ALL SELECT 'phenotype_to_concept_set', (SELECT COUNT(*) FROM @old_schema.phenotype_to_concept_set), (SELECT COUNT(*) FROM @new_schema.phenotype_to_concept_set)
UNION ALL SELECT 'draft_concept_set_target', (SELECT COUNT(*) FROM @old_schema.draft_concept_set_target), (SELECT COUNT(*) FROM @new_schema.draft_concept_set_target)
UNION ALL SELECT 'draft_target_to_concept_set', (SELECT COUNT(*) FROM @old_schema.draft_target_to_concept_set), (SELECT COUNT(*) FROM @new_schema.draft_target_to_concept_set)
UNION ALL SELECT 'concept_set_person_count', (SELECT COUNT(*) FROM @old_schema.concept_set_person_count), (SELECT COUNT(*) FROM @new_schema.concept_set_person_count)
UNION ALL SELECT 'cohort_definition', (SELECT COUNT(*) FROM @old_schema.cohort_definition), (SELECT COUNT(*) FROM @new_schema.cohort_definition)
UNION ALL SELECT 'cohort_definition_to_concept_set', (SELECT COUNT(*) FROM @old_schema.cohort_definition_to_concept_set), (SELECT COUNT(*) FROM @new_schema.cohort_definition_to_concept_set)
UNION ALL SELECT 'cohort_definition_evaluation', (SELECT COUNT(*) FROM @old_schema.cohort_definition_evaluation), (SELECT COUNT(*) FROM @new_schema.cohort_definition_evaluation)
UNION ALL SELECT 'database_meta_data', (SELECT COUNT(*) FROM @old_schema.database_meta_data), (SELECT COUNT(*) FROM @new_schema.database_meta_data)
UNION ALL SELECT 'atlas_instance', (SELECT COUNT(*) FROM @old_schema.atlas_instance), (SELECT COUNT(*) FROM @new_schema.atlas_instance);
