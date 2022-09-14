link_pop <- function(x, pop){
  x |>
    left_join(pop, by = "year") |>
    mutate(par = round(.data$par * .data$prop))
}

link_tx <- function(x, tx){
  x |>
    left_join(tx, by = "year") |>
    mutate(tx_cov_lower = beta_coverage_ci(.data$tx_cov, q = 0.025, average_sample = 1473),
           tx_cov_upper = beta_coverage_ci(.data$tx_cov, q = 0.975, average_sample = 1473))
}

link_interventions <- function(x, interventions){
  interventions <- interventions |>
    select(iso3c, contains("name"), urban_rural, year,
           itn_use, net_type,
           contains("irs"), hh_size,
           contains("smc"),
           rtss_cov,
           contains("pmc"))
  x |>
    left_join(interventions, by = "year")
}
