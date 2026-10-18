DROP TABLE IF EXISTS @database_schema.phenotype;
DROP TABLE IF EXISTS @database_schema.draft_concept_set_target;
DROP TABLE IF EXISTS @database_schema.draft_target_to_concept_set;
DROP TABLE IF EXISTS @database_schema.concept_set;
DROP TABLE IF EXISTS @database_schema.concept_set_person_count;
DROP TABLE IF EXISTS @database_schema.phenotype_to_concept_set;
DROP TABLE IF EXISTS @database_schema.cohort_definition;
DROP TABLE IF EXISTS @database_schema.cohort_definition_to_concept_set;
DROP TABLE IF EXISTS @database_schema.cohort_definition_evaluation;
DROP TABLE IF EXISTS @database_schema.database_meta_data;
DROP TABLE IF EXISTS @database_schema.atlas_instance;

CREATE TABLE @database_schema.phenotype (
  phenotype_id INT PRIMARY KEY,
  phenotype_name VARCHAR NOT NULL UNIQUE,
  clinical_definition VARCHAR NOT NULL,
  creator VARCHAR NOT NULL,
  time_created DATETIME NOT NULL,
  draft INT NOT NULL,
  valid INT NOT NULL
);

CREATE TABLE @database_schema.draft_concept_set_target (
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
CREATE TABLE @database_schema.draft_target_to_concept_set (
  phenotype_id INT NOT NULL,
  pertains VARCHAR NOT NULL, -- 'doi' or 'alternative diagnoses'
  category VARCHAR NOT NULL, -- 'doi', 'symptom', 'diagnostic procedure', etc.
  concept_id INT NOT NULL,
  concept_set_id INT NOT NULL
);

CREATE TABLE @database_schema.concept_set (
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

CREATE TABLE @database_schema.phenotype_to_concept_set (
  phenotype_id INT NOT NULL,
  pertains VARCHAR NOT NULL, -- 'Disease of interest' or 'Alternative diagnoses'
  category VARCHAR NOT NULL, -- 'Hypernym', 'Disease of interest', 'Clinical presentation', etc.
  concept_set_id INT NOT NULL
);

CREATE TABLE @database_schema.concept_set_person_count (
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

CREATE TABLE @database_schema.cohort_definition (
  cohort_definition_id INT PRIMARY KEY,
  cohort_definition_name VARCHAR NOT NULL UNIQUE,
  phenotype_id INT NOT NULL,
  circe_cohort_expression VARCHAR NOT NULL,
  capr_cohort_expression VARCHAR,
  cd_source_type VARCHAR NOT NULL, -- 'Templates', 'User', 'MonoFlow', 'PhenotypeDesigner'
  valid INT NOT NULL,
  atlas_cohort_definition_id INT
);

CREATE TABLE @database_schema.cohort_definition_to_concept_set (
  cohort_definition_id INT NOT NULL,
  concept_set_id INT NOT NULL
);

CREATE TABLE @database_schema.cohort_definition_evaluation (
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

CREATE TABLE @database_schema.database_meta_data (
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
CREATE TABLE @database_schema.atlas_instance (
  base_url VARCHAR NOT NuLL
);

CREATE TABLE @database_schema.phenotype_meta_data (
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

