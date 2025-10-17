#' Create a table to visualize the number of patients recruited per time period.
#'
#' @description Generate a table to plot the number of patients recruited into
#' the cohort per time period
#'  
#' @param cohort_id atlas cohort id of the phenotype to be evaluated.
#' @param scratch.table space and table where the cohort is available.
#' 
#' @details 
#'  
#' ## Required packages
#' * data.table
#' * DBI
#' 
#' @author Luisa Martínez (08APR2025)
patient_recruitment <- function(cohort_id,
                                scratch.table) {
  
  start_date <- as.data.table(dbGetQuery(conn, 
                                       paste0("SELECT cohort_start_date, COUNT(*) AS count FROM ",
                                              scratch.table, " WHERE cohort_definition_id = ",
                                              cohort_id, " GROUP BY cohort_start_date"))
  )
  
  start_date[, year_month := as.Date(format(cohort_start_date, "%Y-%m-01"))]
  to_plot <- start_date[, .(total_count = sum(count)), by = year_month]

  return(to_plot)
  
}

#' Create a table to visualize the number of patients in the cohort per time period.
#'
#' @description Generate a table to plot the number of patients included in the
#' cohort per time window.
#'  
#' @param cohort_id atlas cohort id of the phenotype to be evaluated.
#' @param scratch.table space and table where the cohort is available.
#' 
#' @details 
#'  
#' ## Required packages
#' * data.table
#' * DBI
#' 
#' @author Luisa Martínez (08APR2025)
patient_x_time <- function(cohort_id,
                           scratch.table) {
  
  complete <- as.data.table(dbGetQuery(conn, 
                                       paste0("SELECT subject_id, cohort_start_date, cohort_end_date FROM ",
                                              scratch.table, 
                                              " WHERE cohort_definition_id = ", 
                                              cohort_id))
  )
  
  start_seq <- seq.Date(complete[, min(cohort_start_date)],
                        complete[, max(cohort_end_date)], by = "month")
  
  c_tbl <- NULL
  
  for (i in 1:length(start_seq)) {
    
    c_tbl <- c(c_tbl,
               nrow(complete[cohort_start_date <= start_seq[i] & 
                               cohort_end_date > start_seq[i]]))
    
  }
  
  to_plot <- data.frame(year_month = start_seq, 
                        total_count = as.numeric(c_tbl))

return(to_plot)
  
}

#' Create a table to visualize the number of patients exiting the cohort per time period.
#'
#' @description Generate a table to plot the number of patients exiting the cohort 
#' per time period
#'  
#' @param cohort_id atlas cohort id of the phenotype to be evaluated
#' @param scratch.table space and table where the cohort is available
#' 
#' @details 
#'  
#' ## Required packages
#' * data.table
#' *DBI
#' 
#' @author Luisa Martínez (08APR2025)
patient_eofup <- function(cohort_id,
                          scratch.table) {
  
  end_date <- as.data.table(dbGetQuery(conn, 
                                         paste0("  SELECT cohort_end_date, COUNT(*) AS count FROM ",
                                                scratch.table, " WHERE cohort_definition_id = ", 
                                                cohort_id, " GROUP BY cohort_end_date"))
  )
  
  end_date[, year_month := as.Date(format(cohort_end_date, "%Y-%m-01"))]
  to_plot <- end_date[, .(total_count = sum(count)), by = year_month]
  
  return(to_plot)
  
}

#' Create a table to visualize the number times a concept is reported per time.
#'
#' @description Generate a table to plot the number of times a concept is reported
#' per time window.
#'  
#' @param concept_id from ATLAS to visualize.
#' @param cdm_schema general database cdm schema.
#' @param domain from the cdm where the code is found options are "Condition",
#' "Drug", "Procedure", "Device", "Measurement" or "Observation"
#' 
#' @details 
#'  
#' ## Required packages
#' * data.table
#' * DBI
#' 
#' @author Luisa Martínez (08APR2025)
code_x_time <- function(concept_id,
                        cdm_schema,
                        domain) {
  
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
  # Concept start date per table
  dom_start <- c("condition_start_date",
                   "drug_exposure_start_date",
                   "procedure_date",
                   "device_exposure_start_date",
                   "measurement_date",
                   "observation_date")
  # Indication in atlas that correspond to table name
  dom_atlas <- c("Condition",
                 "Drug",
                 "Procedure",
                 "Device",
                 "Measurement",
                 "Observation")
  
  # Determine Domain from the concept
  dom <- which(dom_atlas == domain)
  
  time_dep <- as.data.table(dbGetQuery(conn, paste0("SELECT ", dom_start[dom],
  " AS start_date, COUNT (*) AS count FROM (SELECT DISTINCT ", dom_start[dom],", person_id ", 
  "FROM ", cdm_schema,".", dom_name[dom], " WHERE ",
  dom_concept[dom], " = '", concept_id,"') GROUP BY ", dom_start[dom]))
  )
  
  if (nrow(time_dep) > 0) {
    
    time_dep[, year_month := as.Date(format(start_date, "%Y-%m-01"))]
    to_plot <- time_dep[, .(total_count = sum(count)), by = year_month]
    
    return(to_plot)
    
  }
  
}
