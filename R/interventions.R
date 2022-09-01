#' Add nets
#'
#' @param x output
#' @param iso3c Country ISO3c code
#' @param country_ur Use country-level usage rate estimates
#'
#' @return output with net numbers
#' @export
add_nets <- function(x, iso3c, country_ur = FALSE){
  # Usage rate
  urd <- netz::get_usage_rate_data()
  # Fixed WHO value
  ur <- 0.88
  if(country_ur){
    if(iso3c %in% urd$iso3){
      ur <- urd[urd$iso3 == iso3c, "usage_rate"]
    } else {
      ur = stats::median(urd$usage_rate)
    }
  }
  # Retention half life
  hld <- netz::get_halflife_data()
  if(iso3c %in% hld$iso3){
    hl <- hld[hld$iso3 == iso3c, "half_life"]
  } else {
    hl = stats::median(hld$half_life)
  }

  x |>
    dplyr::mutate(
      access = netz::usage_to_access(usage = .data$itn_use, use_rate = ur),
      access = ifelse(is.na(.data$access), 1, .data$access),
      crop = netz::access_to_crop(.data$access, type = "loess_extrapolate"),
      commodity_nets_distributed = netz::crop_to_distribution(crop = .data$crop, distribution_freq = 3 * 365, half_life = hl) * .data$par) |>
    dplyr::select(-c(.data$access, .data$crop))
}

#' Add net costs
#'
#' @param x output
#'
#' @return output with net costs
#' @export
add_net_cost <- function(x){
  x |>
    dplyr::mutate(cost_itn = dplyr::case_when(
      net_type == "pyrethroid_only" ~ treasure::cost_llin(.data$commodity_nets_distributed),
      net_type == "pyrethroid_pbo" ~ treasure::cost_pbo_itn(.data$commodity_nets_distributed),
      # Assume these nets are 10% more costly than pyrethroid PBO
      net_type == "pyrethroid_pyrrole" ~ treasure::cost_pbo_itn(.data$commodity_nets_distributed, pbo_itn_unit_cost = 3.51 * 1.1)
    ))
}

#' IRS commodities
#'
#' @param x output
#'
#' @return output with number of people protected and number of households sprayed
#' @export
add_irs <- function(x){
  x |>
    dplyr::mutate(commodity_irs_people_protected = round(.data$irs_cov *  .data$par),
                  commodity_irs_households_sprayed = round(.data$commodity_irs_people_protected / .data$hh_size))
}

#' IRS costs
#'
#' @param x output
#'
#' @return output with irs costs
#' @export
add_irs_cost <- function(x){
  x |>
    dplyr::mutate(cost_irs = treasure::cost_ll_irs_person(.data$commodity_irs_people_protected))
}

#' Add PMC
#'
#' @param x output
#' @param modelled_population_size modelled population size
#' @param doses_per_child Number of PMC doses per child per year
#'
#' @return output with pmc doses and children protected
add_pmc <- function(x, modelled_population_size, doses_per_child = 3){
  x |>
    dplyr::mutate(commodity_pmc_doses = round((.data$n_pmc_treated / modelled_population_size) * .data$par_pf),
                  commodity_pmc_children_protected = round(.data$commodity_pmc_doses / doses_per_child))
}

#' Add PMC cost
#'
#' @param x output
#'
#' @return output with PMC costs
#' @export
add_pmc_cost <- function(x){
  x |>
    dplyr::mutate(cost_pmc = treasure::cost_ipti(n_doses = .data$commodity_pmc_doses))
}

#' Add SMC
#'
#' @param x output
#' @param modelled_population_size modelled population size
#'
#' @return output with pmc doses and children protected
add_smc <- function(x, modelled_population_size){
  x |>
    dplyr::mutate(commodity_smc_doses = round((.data$n_smc_treated / modelled_population_size) * .data$par_pf),
                  commodity_smc_children_protected = round(.data$commodity_smc_doses / .data$smc_n_rounds))
}

#' Add SMC cost
#'
#' @param x output
#'
#' @return output with SMC costs
#' @export
add_smc_cost <- function(x){
  x |>
    dplyr::mutate(cost_smc = treasure::cost_smc(n_doses = .data$commodity_smc_doses))
}

#' Add RTS,S
#'
#' @param x output
#' @param modelled_population_size modelled population size
#' @param rtss_doses_per_child Number of doses per child
#'
#' @return output with rtss doses and rtss protected
add_rtss <- function(x, modelled_population_size, rtss_doses_per_child = 4){
  x |>
    dplyr::mutate(commodity_rtss_doses = round((.data$n_rtss_doses / modelled_population_size) * .data$par_pf),
                  commodity_rtss_children_protected = round(.data$commodity_rtss_doses / rtss_doses_per_child))
}

#' Add rtss cost
#'
#' @param x output
#'
#' @return output with rtss costs
#' @export
add_rtss_cost <- function(x){
  x |>
    dplyr::mutate(cost_rtss = treasure::cost_rtss(n_doses = .data$commodity_rtss_doses))
}

