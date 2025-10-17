#' Prevalence changes between the cohort and the general population for the most 
#' frequent codes
#'
#' @description Create a table with the codes that has at least a prevalence 
#' change of 5. The table includes codes from all the domains (Concept id, 
#' Concept Name, Domain), the proportion of the code within the whole population, 
#' the proportion of the code within the cohort and the prevalence change result. 
#'  
#' @param cohort_id atlas cohort id of the phenotype to be evaluated
#' @param scratch scratch space where the cohort is available
#' @param cdm_schema cdm schema name from the dataset
#' @param concept_sets concepts from the concept set with at least the following
#' columns (Id = concept_id, Name = concept_name, Domain = as retrieved from ATLAS)
#' 
#' @details 
#'  
#' ## Required packages
#' * data.table
#' 
#' ## Required functions
#' * table_freq_concepts
#' * table_freq_spcf_concepts
#' 
#' ## Other requirements
#' * The connection to the dataset will have to be created before running the function.
#' 
#' @author Luisa Martínez (15SEP2024)
getPrevalenceChanges <- function(connectionDetails = NULL,
                                 connection = NULL,
                                 cohortDatabaseSchema,
                                 cohortIds,
                               scratch,
                               cdmSchema,
                               conceptSets) {
  
  start <- Sys.time()
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  # Create vectors for the different domains in the omop cdm
  # This could be included in a library
  # Table name
  dom_name <- c("condition_occurrence",
                "drug_exposure",
                "procedure_occurrence",
                "device_exposure",
                "measurement",
                "observation")
  # Concept identification per table
  dom_concept <- c("condition_concept_id",
                   "drug_concept_id",
                   "procedure_concept_id",
                   "device_concept_id",
                   "measurement_concept_id",
                   "observation_concept_id")
  # Indication in atlas that correspond to table name
  dom_atlas <- c("Condition",
                 "Drug",
                 "Procedure",
                 "Device",
                 "Measurement",
                 "Observation")
  
  complete_table <- NULL
  
  # Iterate over all the domains from the omop cdm
  for (n in 1:length(dom_name)) {
    # Select the most frequent codes with at least 5% of patients from the 
    # specific domain
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetFrequentConcepts.sql",
        packageName = utils::packageName(),
        dbms = connection@dbms,
        domain_table = dom_name[n],
        domain_concept_id = dom_concept[n],
        cohort_database_schema = cohortDatabaseSchema,
        cdm_database_schema = cdmSchema,
        cohort_id = cohortIds,
        min_freq = 5
      )
    
    freq_con_cohort <- DatabaseConnector::querySql(connection, 
                                                   sql, 
                                                   snakeCaseToCamelCase = TRUE)
    
    # For the most frequent codes in the cohort, calculate the frequency in the 
    # general population
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "CreateConceptFrequencyTable.sql",
        packageName = utils::packageName(),
        dbms = connection@dbms,
        domain_table = dom_name[n],
        domain_concept_id = dom_concept[n],
        input_concepts = paste0(freq_con_cohort$concept_id, collapse = ","),
        cdm_schema = cdmSchema
      )
    
    freq_con_total <- DatabaseConnector::querySql(connection, 
                                                  sql, 
                                                  snakeCaseToCamelCase = TRUE)
    
    # Merge the frequency in the cohort with the frequency in the general population
    freq_table <- as.data.table(merge(freq_con_cohort,
                                      freq_con_total,
                                      by = "concept_id"))
    
    # Include the domain for the final table
    freq_table[, domain := dom_atlas[n]]
    # Calculate the prevalence change and keep PC higher than 5
    freq_table[, comparator := pop_perc/total_pop_perc]
    freq_table <- freq_table[comparator > 5]
    
    # Join tables by domain
    complete_table <- rbind(complete_table,
                            freq_table)
    
  }
  
  # Clean the table to have specific format to be shown
  table_to_print <- complete_table[!(concept_id %in% concept_sets$Id)
                                   , .('Concept id' = concept_id,
                                       'Concept Name' = concept_name,
                                       'Domain' = domain,
                                       'Proportion within the cohort (Pc)' = pop_perc,
                                       'Proportion within the total population (Pt)' = total_pop_perc,
                                       'Pc/Pt' = comparator)]
  
  return(table_to_print)
  
}

#' Correlation analysis for code recommendation when evaluating a phenotype
#'
#' @description Create a list of tables with the result of the quantitative correlation
#'  between the appearance of the codes from the concept set and the 30 most 
#'  frequent codes per domain between the 6 domains from the omop cdm (Condition, 
#'  Drug, Procedure, Device, Measurement, Observation).
#'  
#' @param cohort_id atlas cohort id of the phenotype to be evaluated
#' @param scratch scratch space where the cohort is available
#' @param cdm_schema cdm schema name from the dataset
#' @param concept_sets concepts from the concept set with at least the following
#' columns (Id = concept_id, Name = concept_name, Domain = as retrieved from ATLAS)
#' 
#' @details 
#'  
#' ## Required packages
#' * data.table
#' 
#' ## Required functions
#' * table_freq_concepts
#' * query_patientxconcept
#' 
#' ## Other requirements
#' * The connection to the dataset will have to be created before running the function.
#' 
#' @author Luisa Martínez (10SEP2024)
correlation_analysis <- function(cohort_id,
                                 scratch,
                                 cdm_schema,
                                 concept_sets) {
  
  # Create vectors for the different domains in the omop cdm
  # This could be included in a library
  # Table name
  dom_name <- c("condition_occurrence",
                "drug_exposure",
                "procedure_occurrence",
                "device_exposure",
                "measurement",
                "observation")
  # Concept identificator per table
  dom_concept <- c("condition_concept_id",
                   "drug_concept_id",
                   "procedure_concept_id",
                   "device_concept_id",
                   "measurement_concept_id",
                   "observation_concept_id")
  # Indication in atlas that correspond to table name
  dom_atlas <- c("Condition",
                 "Drug",
                 "Procedure",
                 "Device",
                 "Measurement",
                 "Observation")

  # Create an empty list to store the tables
  output_tables <- list()
  # 6 tables will be generated, one per domain. Iterate over the domains to create them.
  for (n in 1:length(dom_name)) {
    
    # Initialize the complete correlations table 
    comp_cor <- NULL
    # Select the 30 most frequent codes with at least 30% of patients from the 
    # specific domain
    query <- paste0(table_freq_concepts(dom_name[n], 
                                        dom_concept[n], 
                                        scratch, 
                                        cdm_schema,
                                        cohort_id, 
                                        1), " LIMIT 30")
    
    freq_concpt <- as.data.table(dbGetQuery(conn, query))
    freq_concpt <- freq_concpt[!(concept_id %in% concept_sets$Id)]
    # Store the most frequent concepts in the list for future use
    output_tables[[paste0(dom_name[n], "_freq_concepts")]] <- freq_concpt
    
    # Iterate over the concepts from the concept_set
    for (c in 1:length(concept_sets$Id)) {
      
      # Determine Domain from the concept
      dom <- which(dom_atlas == concept_sets$Domain[c])
      # Generate a query to extract the patients id containing the concept from omop cdm
      query_vector <- query_patientxconcept(dom_name[dom], 
                                            dom_concept[dom],
                                            scratch, 
                                            cdm_schema,
                                            concept_sets$Id[c],
                                            cohort_id)
      
      new_vector <- dbGetQuery(conn, query_vector)
      
      # If there is at least one patient with the code, include it in the final table
      # where there is a row per patient and a binary column per code and the 
      # first code is the one to be compare with the others
      if(nrow(new_vector) > 0) {
        # Create binary column
        new_vector$concept_id <- 1
        colnames(new_vector) <- c("person_id", concept_sets$Name[c])
        patient <- new_vector
        
        # Iterate over the most frequent concepts to complete the correlations table
        for (code in 1:nrow(freq_concpt)) {
          
          # Generate a query to extract the patients id containing the concept from omop cdm
          query_vector <- query_patientxconcept(dom_name[n], 
                                                dom_concept[n], 
                                                scratch, 
                                                cdm_schema,
                                                freq_concpt[code, concept_id],
                                                cohort_id)
          
          new_vector <- dbGetQuery(conn, query_vector)
          # Create binary column
          new_vector$concept_id <- 1
          colnames(new_vector) <- c("person_id", freq_concpt[code, concept_name])
          
          # Marge each freq code with the "target" code of interest (from the concept set)
          # to create the table for the correlation analysis
          patient <- merge(patient,
                           new_vector,
                           all.x = TRUE,
                           all.y = TRUE,
                           by = "person_id")
          
        }
        
        # Transform NA into 0 to complete the binary columns
        patient[is.na(patient)] <- 0
        # Analyze the correlations between the codes (excluding patient id column)
        cset_corr <- cor(patient[,-1])
        
        corr_table <- as.data.frame(cbind(rownames(as.data.frame(cset_corr[1,]))[1],
                                          rownames(as.data.frame(cset_corr[1,-1])),
                                          cset_corr[1,-1]))
        
        rownames(corr_table) <- NULL
        corr_table$V3 <- as.numeric(corr_table$V3)
        
        comp_cor <- rbind(comp_cor,
                          corr_table)
      }
    }
    
    output_tables[[paste0(dom_name[n], "_correlations")]] <- comp_cor[]

  }
  
  return(output_tables)
}
