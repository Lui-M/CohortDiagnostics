{DEFAULT @domain_table = "domain_table"}
{DEFAULT @domain_concept_id = "domain_concept_id"}
{DEFAULT @scratch = "scratch.dbo"}
{DEFAULT @cdm_schema = "cdm_schema.dbo"}
{DEFAULT @concept_id = "concept_id"}
{DEFAULT @cohort_id = "cohort_id"}

SELECT DISTINCT d.person_id 
FROM @cdm_schema.@domain_table d
JOIN @scratch s ON d.person_id = s.subject_id
WHERE s.cohort_definition_id = @cohort_id
AND d.@domain_concept_id = @concept_id;
