-- Development seed data for the Pegasus data model. NOT part of the installed package.
-- Run after createDataModel(). Parameter: @database_schema.
--
-- Covers every derived status the Shiny app should show:
--   Evaluated          : 1 Asthma, 2 Type 2 diabetes mellitus, 3 COPD, 4 Atrial fibrillation (several cohorts, databases, source types)
--   Cohort defined     : 5 Acute liver injury
--   Concept sets only  : 6 Psoriasis
--   Draft (queued)     : 7 Chronic kidney disease, 8 Asthma with eosinophilic phenotype (clone of 1, via phenotype_ancestor)
--   Invalid            : 9 Gout (retired)
-- Concept IDs are from memory and meant for UI development only; verify against your vocabulary
-- before relying on them (4345213 in particular is a placeholder). Evaluation numbers are fabricated.

INSERT INTO @database_schema.phenotype (phenotype_id, phenotype_name, clinical_definition, creator, time_created, draft, valid) VALUES
  (1, 'Asthma', 'A chronic inflammatory airway disease with recurrent wheeze, cough, and breathlessness, confirmed by a clinician diagnosis and treated with bronchodilators or inhaled corticosteroids.', 'dev_user', '2026-09-01 00:00:00', 0, 1),
  (2, 'Type 2 diabetes mellitus', 'Persistent hyperglycemia due to insulin resistance and relative insulin deficiency, diagnosed by a clinician and managed with lifestyle change or glucose-lowering drugs; excludes type 1 and gestational diabetes.', 'dev_user', '2026-09-01 00:00:00', 0, 1),
  (3, 'Chronic obstructive pulmonary disease', 'Progressive airflow limitation from emphysema or chronic bronchitis, typically in current or former smokers, diagnosed by a clinician and treated with long-acting bronchodilators.', 'dev_user', '2026-09-01 00:00:00', 0, 1),
  (4, 'Atrial fibrillation', 'An irregular, often rapid supraventricular rhythm confirmed on ECG or by a clinician diagnosis, usually managed with rate or rhythm control and anticoagulation.', 'dev_user', '2026-09-01 00:00:00', 0, 1),
  (5, 'Acute liver injury', 'Abrupt hepatocellular or cholestatic injury marked by a rise in liver enzymes and clinician diagnosis, from drugs, viral infection, or other causes, in the absence of chronic liver disease.', 'dev_user', '2026-09-02 00:00:00', 0, 1),
  (6, 'Psoriasis', 'A chronic immune-mediated skin disease with scaly erythematous plaques, diagnosed by a clinician or dermatologist.', 'dev_user', '2026-09-02 00:00:00', 0, 1),
  (7, 'Chronic kidney disease', 'Sustained reduction in kidney function or markers of kidney damage for at least three months, diagnosed by a clinician and staged by eGFR and albuminuria.', 'dev_user', '2026-10-10 09:30:00', 1, 0),
  (8, 'Asthma with eosinophilic phenotype', 'Asthma diagnosed by a clinician with documented elevated blood eosinophils, managed with inhaled corticosteroids or biologic therapy.', 'dev_user', '2026-10-12 14:00:00', 1, 0),
  (9, 'Gout (retired)', 'Recurrent inflammatory arthritis from monosodium urate crystal deposition. Superseded; kept for reference.', 'dev_user', '2026-09-03 00:00:00', 0, 0);

INSERT INTO @database_schema.phenotype_ancestor (phenotype_id, parent_phenotype_id) VALUES
  (8, 1);

INSERT INTO @database_schema.phenotype_meta_data (phenotype_id, is_pharmacovigilance_outcome, clinical_course, expected_care_setting, sex_constraint, min_age_years, max_age_years, minimum_interepisode_gap_days, has_discrete_recorded_symptoms, requires_diagnostic_test_or_procedure, requires_active_treatment_within_30d, expects_condition_specific_followup_or_sequelae_1yr) VALUES
  (1, 'NO', 'PERSISTENT_RELAPSING', 'OUTPATIENT_EXPECTED', 'BOTH_SEXES', 0, NULL, 30, 1, 1, 1, 1),
  (2, 'NO', 'PERSISTENT_STABLE', 'OUTPATIENT_EXPECTED', 'BOTH_SEXES', 18, NULL, NULL, 0, 1, 1, 1),
  (5, 'YES', 'TRANSIENT_SINGLE', 'ACUTE_CARE_EXPECTED', 'BOTH_SEXES', 0, NULL, 90, 1, 1, 0, 1);

INSERT INTO @database_schema.concept_set (concept_set_id, concept_set_name, circe_concept_set_expression, capr_concept_set_expression, has_descendants, clinical_definition, concept_id, valid, atlas_concept_set_id, cs_source_type) VALUES
  (1, 'Asthma', '{"items": [{"concept": {"CONCEPT_ID": 317009, "CONCEPT_NAME": "Asthma"}, "isExcluded": false, "includeDescendants": true, "includeMapped": false}]}', 'cs(descendants(317009), name = "Asthma")', 1, NULL, 317009, 1, NULL, 'concept + descendants'),
  (2, 'Asthma (exact)', '{"items": [{"concept": {"CONCEPT_ID": 317009, "CONCEPT_NAME": "Asthma"}, "isExcluded": false, "includeDescendants": false, "includeMapped": false}]}', 'cs(317009, name = "Asthma (exact)")', 0, NULL, 317009, 1, NULL, 'concept'),
  (3, 'Type 2 diabetes mellitus', '{"items": [{"concept": {"CONCEPT_ID": 201826, "CONCEPT_NAME": "Type 2 diabetes mellitus"}, "isExcluded": false, "includeDescendants": true, "includeMapped": false}]}', 'cs(descendants(201826), name = "Type 2 diabetes mellitus")', 1, NULL, 201826, 1, NULL, 'concept + descendants'),
  (4, 'COPD', '{"items": [{"concept": {"CONCEPT_ID": 255573, "CONCEPT_NAME": "Chronic obstructive lung disease"}, "isExcluded": false, "includeDescendants": true, "includeMapped": false}]}', 'cs(descendants(255573), name = "COPD")', 1, NULL, 255573, 1, NULL, 'concept + descendants'),
  (5, 'Atrial fibrillation', '{"items": [{"concept": {"CONCEPT_ID": 313217, "CONCEPT_NAME": "Atrial fibrillation"}, "isExcluded": false, "includeDescendants": true, "includeMapped": false}]}', 'cs(descendants(313217), name = "Atrial fibrillation")', 1, NULL, 313217, 1, NULL, 'from AI definition'),
  (6, 'Acute liver injury', '{"items": [{"concept": {"CONCEPT_ID": 4345213, "CONCEPT_NAME": "Acute liver injury"}, "isExcluded": false, "includeDescendants": true, "includeMapped": false}]}', 'cs(descendants(4345213), name = "Acute liver injury")', 1, NULL, 4345213, 1, NULL, 'from human definition'),
  (7, 'Psoriasis', '{"items": [{"concept": {"CONCEPT_ID": 140168, "CONCEPT_NAME": "Psoriasis"}, "isExcluded": false, "includeDescendants": true, "includeMapped": false}]}', 'cs(descendants(140168), name = "Psoriasis")', 1, NULL, 140168, 1, NULL, 'concept + descendants'),
  (8, 'Asthma with exclusion of COPD', '{"items": [{"concept": {"CONCEPT_ID": 317009, "CONCEPT_NAME": "Asthma"}, "isExcluded": false, "includeDescendants": true, "includeMapped": false}, {"concept": {"CONCEPT_ID": 255573, "CONCEPT_NAME": "Chronic obstructive lung disease"}, "isExcluded": true, "includeDescendants": true, "includeMapped": false}]}', 'cs(descendants(317009), exclude(descendants(255573)), name = "Asthma with exclusion of COPD")', 1, NULL, 317009, 1, NULL, 'user');

INSERT INTO @database_schema.concept_set_item (concept_set_id, concept_id, is_excluded, include_descendants, include_mapped) VALUES
  (1, 317009, 0, 1, 0),
  (2, 317009, 0, 0, 0),
  (3, 201826, 0, 1, 0),
  (4, 255573, 0, 1, 0),
  (5, 313217, 0, 1, 0),
  (6, 4345213, 0, 1, 0),
  (7, 140168, 0, 1, 0),
  (8, 317009, 0, 1, 0),
  (8, 255573, 1, 1, 0);

INSERT INTO @database_schema.phenotype_to_concept_set (phenotype_id, pertains, category, concept_set_id) VALUES
  (1, 'doi', 'doi', 1),
  (1, 'doi', 'doi', 2),
  (2, 'doi', 'doi', 3),
  (3, 'doi', 'doi', 4),
  (4, 'doi', 'doi', 5),
  (5, 'doi', 'doi', 6),
  (6, 'doi', 'doi', 7),
  (8, 'doi', 'doi', 8);

INSERT INTO @database_schema.draft_concept_set_target (phenotype_id, pertains, category, term, concept_id, concept_name, domain_id, concept_class_id, vocabulary_id, match_type) VALUES
  (7, 'doi', 'doi', 'chronic kidney disease', 46271022, 'Chronic kidney disease', 'Condition', 'Clinical Finding', 'SNOMED', 'verbatim'),
  (7, 'alternative diagnoses', 'symptom', 'proteinuria', 435515, 'Proteinuria', 'Condition', 'Clinical Finding', 'SNOMED', 'vector'),
  (8, 'doi', 'doi', 'asthma', 317009, 'Asthma', 'Condition', 'Clinical Finding', 'SNOMED', 'verbatim');

INSERT INTO @database_schema.database_meta_data (database_id, cdm_source_name, cdm_source_abbreviation, cdm_release_date, cdm_release_identifier, vocabulary_version) VALUES
  (1, 'Dev Claims Database', 'DEVCLM', '2025-06-30', 'v5.4', 'v5.0 24-JUN-2025'),
  (2, 'Dev EHR Database', 'DEVEHR', '2025-12-31', 'v5.4', 'v5.0 24-JUN-2025'),
  (3, 'Dev Registry', 'DEVREG', '2024-12-31', 'v5.3', 'v5.0 22-DEC-2024');

INSERT INTO @database_schema.atlas_instance (base_url) VALUES ('https://atlas.example.org/atlas');

INSERT INTO @database_schema.concept_set_person_count (concept_set_id, database_id, condition_persons, procedure_persons, drug_persons, device_persons, measurement_persons, observation_persons, visit_persons, overall_persons) VALUES
  (1, 1, 167315, 74362, 130134, 3718, 92953, 55771, 148724, 185906),
  (1, 2, 92916, 41296, 72268, 2064, 51620, 30972, 82592, 103240),
  (1, 3, 28629, 12724, 22267, 636, 15905, 9543, 25448, 31811),
  (2, 1, 261126, 116056, 203098, 5802, 145070, 87042, 232112, 290141),
  (2, 2, 191712, 85205, 149109, 4260, 106507, 63904, 170411, 213014),
  (2, 3, 44711, 19871, 34775, 993, 24839, 14903, 39743, 49679),
  (3, 1, 88904, 39513, 69148, 1975, 49391, 29634, 79026, 98783),
  (3, 2, 64992, 28885, 50549, 1444, 36107, 21664, 57771, 72214),
  (3, 3, 13202, 5867, 10268, 293, 7334, 4400, 11735, 14669),
  (4, 1, 131416, 58407, 102212, 2920, 73009, 43805, 116814, 146018),
  (4, 2, 67062, 29805, 52159, 1490, 37257, 22354, 59611, 74514),
  (4, 3, 16934, 7526, 13171, 376, 9408, 5644, 15052, 18816),
  (5, 1, 2618, 1163, 2036, 58, 1454, 872, 2327, 2909),
  (5, 2, 1831, 814, 1424, 40, 1017, 610, 1628, 2035),
  (5, 3, 343, 152, 267, 7, 191, 114, 305, 382),
  (6, 1, 48021, 21342, 37349, 1067, 26678, 16007, 42685, 53357),
  (6, 2, 34051, 15134, 26484, 756, 18917, 11350, 30268, 37835),
  (6, 3, 9549, 4244, 7427, 212, 5305, 3183, 8488, 10611),
  (7, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  (7, 2, 0, 0, 0, 0, 0, 0, 0, 0),
  (7, 3, 0, 0, 0, 0, 0, 0, 0, 0),
  (8, 1, 132617, 58941, 103147, 2947, 73676, 44205, 117882, 147353),
  (8, 2, 111136, 49394, 86439, 2469, 61742, 37045, 98788, 123485),
  (8, 3, 22254, 9890, 17308, 494, 12363, 7418, 19781, 24727);

INSERT INTO @database_schema.cohort_definition (cohort_definition_id, cohort_definition_name, phenotype_id, circe_cohort_expression, capr_cohort_expression, pa_source_type, valid, atlas_cohort_definition_id) VALUES
  (1, 'Asthma - template', 1, '{"ConceptSets": [], "cohortName": "Asthma - template", "note": "dev placeholder"}', NULL, 'template', 1, NULL),
  (2, 'Asthma - AI', 1, '{"ConceptSets": [], "cohortName": "Asthma - AI", "note": "dev placeholder"}', NULL, 'ai', 1, NULL),
  (3, 'Asthma - user', 1, '{"ConceptSets": [], "cohortName": "Asthma - user", "note": "dev placeholder"}', NULL, 'user', 1, NULL),
  (4, 'T2DM - template', 2, '{"ConceptSets": [], "cohortName": "T2DM - template", "note": "dev placeholder"}', NULL, 'template', 1, NULL),
  (5, 'T2DM - AI', 2, '{"ConceptSets": [], "cohortName": "T2DM - AI", "note": "dev placeholder"}', NULL, 'ai', 1, NULL),
  (6, 'COPD - template', 3, '{"ConceptSets": [], "cohortName": "COPD - template", "note": "dev placeholder"}', NULL, 'template', 1, NULL),
  (7, 'COPD - AI', 3, '{"ConceptSets": [], "cohortName": "COPD - AI", "note": "dev placeholder"}', NULL, 'ai', 1, NULL),
  (8, 'AFib - template', 4, '{"ConceptSets": [], "cohortName": "AFib - template", "note": "dev placeholder"}', NULL, 'template', 1, NULL),
  (9, 'Acute liver injury - AI', 5, '{"ConceptSets": [], "cohortName": "Acute liver injury - AI", "note": "dev placeholder"}', NULL, 'ai', 1, NULL),
  (10, 'Gout - template', 9, '{"ConceptSets": [], "cohortName": "Gout - template", "note": "dev placeholder"}', NULL, 'template', 1, NULL);

INSERT INTO @database_schema.cohort_definition_to_concept_set (cohort_definition_id, concept_set_id) VALUES
  (1, 1),
  (2, 1),
  (3, 2),
  (4, 3),
  (5, 3),
  (6, 4),
  (7, 4),
  (8, 5),
  (9, 6);

INSERT INTO @database_schema.cohort_definition_evaluation (cohort_definition_id, database_id, cohort_entries, cohort_subjects, generation_seconds, true_positives, false_positives, true_negatives, false_negatives, ppv, ppv_lb, ppv_ub, sensitivity, sensitivity_lb, sensitivity_ub, evaluation_type) VALUES
  (1, 1, 200, 190, 167, 180, 20, 770, 30, 0.9, 0.851, 0.934, 0.857, 0.803, 0.898, 'keeper + llm'),
  (1, 2, 185, 175, 573, 160, 25, 780, 35, 0.865, 0.808, 0.907, 0.821, 0.761, 0.868, 'keeper + llm'),
  (1, 3, 95, 90, 140, 85, 10, 885, 20, 0.895, 0.817, 0.942, 0.81, 0.724, 0.873, 'phevaluator'),
  (2, 1, 200, 190, 604, 150, 50, 785, 15, 0.75, 0.686, 0.805, 0.909, 0.855, 0.944, 'keeper + llm'),
  (2, 2, 185, 175, 335, 140, 45, 795, 20, 0.757, 0.69, 0.813, 0.875, 0.815, 0.918, 'keeper + llm'),
  (2, 1, 200, 190, 593, 155, 45, 780, 20, 0.775, 0.712, 0.827, 0.886, 0.83, 0.925, 'phevaluator'),
  (3, 1, 132, 125, 855, 120, 12, 808, 60, 0.909, 0.848, 0.947, 0.667, 0.595, 0.731, 'keeper + llm'),
  (4, 1, 330, 313, 718, 300, 30, 625, 45, 0.909, 0.873, 0.936, 0.87, 0.83, 0.901, 'keeper + llm'),
  (4, 2, 305, 289, 205, 270, 35, 645, 50, 0.885, 0.845, 0.916, 0.844, 0.8, 0.879, 'keeper + llm'),
  (4, 3, 98, 93, 125, 90, 8, 880, 22, 0.918, 0.847, 0.958, 0.804, 0.72, 0.867, 'keeper + human'),
  (5, 1, 380, 361, 615, 310, 70, 590, 30, 0.816, 0.774, 0.852, 0.912, 0.877, 0.937, 'keeper + llm'),
  (5, 2, 355, 337, 604, 280, 75, 613, 32, 0.789, 0.743, 0.828, 0.897, 0.859, 0.926, 'keeper + llm'),
  (6, 1, 150, 142, 674, 90, 60, 780, 70, 0.6, 0.52, 0.675, 0.562, 0.485, 0.637, 'keeper + llm'),
  (6, 2, 135, 128, 212, 80, 55, 800, 65, 0.593, 0.508, 0.672, 0.552, 0.47, 0.63, 'keeper + llm'),
  (7, 1, 150, 142, 401, 110, 40, 800, 50, 0.733, 0.657, 0.798, 0.688, 0.612, 0.754, 'keeper + llm'),
  (8, 1, 80, 76, 119, 75, 5, 880, 40, 0.938, 0.862, 0.973, 0.652, 0.561, 0.733, 'keeper + llm'),
  (8, 2, 76, 72, 580, 70, 6, 886, 38, 0.921, 0.838, 0.963, 0.648, 0.554, 0.732, 'keeper + llm');
