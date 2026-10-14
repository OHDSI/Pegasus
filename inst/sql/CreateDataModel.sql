DROP TABLE IF EXISTS @database_schema.phenotype;
DROP TABLE IF EXISTS @database_schema.phenotype_ancestor;
DROP TABLE IF EXISTS @database_schema.draft_concept_set_target;
DROP TABLE IF EXISTS @database_schema.draft_target_to_concept_set;
DROP TABLE IF EXISTS @database_schema.concept_set;
DROP TABLE IF EXISTS @database_schema.concept_set_item;
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

-- Records that a phenotype was created by cloning (refining) another one
CREATE TABLE @database_schema.phenotype_ancestor (
  phenotype_id INT NOT NULL,
  parent_phenotype_id INT NOT NULL
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
  concept_set_name VARCHAR NOT NULL UNIQUE,
  circe_concept_set_expression VARCHAR NOT NULL,
  capr_concept_set_expression VARCHAR,
  has_descendants INT NOT NULL,
  clinical_definition VARCHAR,
  concept_id INT,
  valid INT NOT NULL,
  atlas_concept_set_id INT,
  cs_source_type VARCHAR -- 'from human definition', 'from AI definition', 'concept + descendants', 'concept', or 'user'
);

-- Flattened (unexpanded) items of the Circe concept set expressions, used to find concept sets by concept
CREATE TABLE @database_schema.concept_set_item (
  concept_set_id INT NOT NULL,
  concept_id INT NOT NULL,
  is_excluded INT NOT NULL,
  include_descendants INT NOT NULL,
  include_mapped INT NOT NULL
);

CREATE TABLE @database_schema.phenotype_to_concept_set (
  phenotype_id INT NOT NULL,
  pertains VARCHAR NOT NULL, -- 'doi' or 'alternative diagnoses'
  category VARCHAR NOT NULL, -- 'doi', 'symptom', 'diagnostic procedure', etc.
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
  pa_source_type VARCHAR NOT NULL, -- 'template', 'ai', or 'user'
  valid INT NOT NULL,
  atlas_cohort_definition_id INT
);

-- Do we really need this?
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
  ppv FLOAT NOT NULL,
  ppv_lb FLOAT NOT NULL,
  ppv_ub FLOAT NOT NULL,
  sensitivity FLOAT NOT NULL,
  sensitivity_lb FLOAT NOT NULL,
  sensitivity_ub FLOAT NOT NULL,
  evaluation_type VARCHAR -- 'keeper + llm', 'phevaluator', or 'keeper + human'
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
