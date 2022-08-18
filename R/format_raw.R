#' Add missing columns
#'
#' Ensure that the minimum set of required columns is present, even if a simulation
#' run isn't performed (i.e. when there is no transmission of a species in a
#' location, or no implementation of an intervention in a location).
#'
#' @param x malariasimulation output
#' @param species species code. Can be falicparum "pf" or vivax "pf".
#'
#' @return data.frame with all required columns.
add_missing_columns <- function(x, species = "pf"){
  if(species == "pf"){
    required <- c("n_detect_730_3649", "n_730_3649",
                  "n_inc_clinical_0_1824", "n_inc_clinical_1825_5474",  "n_inc_clinical_5475_36499",
                  "n_inc_severe_0_1824",  "n_inc_severe_1825_5474", "n_inc_severe_5475_36499",
                  "n_age_0_1824", "n_age_1825_5474", "n_age_5475_36499",
                  "n_rtss_epi_dose_1", "n_rtss_epi_dose_2", "n_rtss_epi_dose_3", "n_rtss_epi_booster_1",
                  "n_pmc_treated", "n_smc_treated")
  }
  if(species == "pv"){
    required <- c("n_detect_365_36499", "n_365_36499",
                  "n_inc_clinical_0_1824", "n_inc_clinical_1825_5474",  "n_inc_clinical_5475_36499",
                  "n_inc_severe_0_1824",  "n_inc_severe_1825_5474", "n_inc_severe_5475_36499")
  }

  missing <- setdiff(required, colnames(x))
  x[,missing] <- 0
  return(x)
}
