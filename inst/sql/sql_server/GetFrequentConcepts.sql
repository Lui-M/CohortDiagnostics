{DEFAULT @domain_table = "condition_occurrence"}
{DEFAULT @domain_concept_id = "condition_concept_id"}
{DEFAULT @cohort_database_schema = scratch.dbo}
{DEFAULT @cdm_database_schema = CDM_jmdc_v1063.dbo}
{DEFAULT @cohort_id = 5665}
{DEFAULT @min_freq = 5}

SELECT a.concept_name, b.concept_id, b.pop, b.pop_perc  
FROM (SELECT @domain_concept_id concept_id, 
             COUNT(DISTINCT h.person_id) pop, 
             100 * (CONVERT(numeric, COUNT(DISTINCT h.person_id)) / 
             (SELECT COUNT(DISTINCT subject_id) FROM  
              @cohort_database_schema WHERE cohort_definition_id = @cohort_id)) pop_perc 
      FROM @cdm_database_schema.@domain_table h 
      JOIN @cohort_database_schema ON h.person_id = @cohort_database_schema.subject_id
      WHERE @cohort_database_schema.cohort_definition_id = @cohort_id
      GROUP BY concept_id) b 
JOIN @cdm_database_schema.concept a ON b.concept_id = a.concept_id 
WHERE pop_perc > @min_freq 
ORDER BY pop_perc DESC;
