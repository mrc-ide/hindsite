run_site <- function(site, population_size = 1000){
  output <- list()

  sp <- c("pf", "pv")

  epi <- list()
  prev <- list()
  for(i in seq_along(sp)){
    species <- sp[i]
    eir <- unlist(site$eir[site$eir$spp == species, "eir"])

    if(eir > 0){
      p <- site::site_parameters(interventions = site$interventions,
                                 demography = site$demography,
                                 vectors = site$vectors,
                                 seasonality = site$seasonality,
                                 eir = eir,
                                 species = species,
                                 overrides = list(
                                   human_population = population_size,
                                   individual_mosquitoes = FALSE
                                 ))
      sim_out <- malariasimulation::run_simulation(p$timesteps, p)
    } else {
      # If species not present return blank output template
      sim_out <- data.frame(timesteps = 1:(diff(range(site$interventions$year)) + 1) * 365)
    }

    sim_out <- sim_out |>
      add_missing_columns(species = species) |>
      add_year()

    if(i == 1){
      output$ages <- format_age(sim_out, population_size)
    }

    # Formatting EPI this is the same process for either species
    epi[[i]] <- format_epi(sim_out, species)

    prev[[i]] <- format_prev(sim_out, species)

    # prevalence
    if(species == "pf"){
      output$interventions <- format_interventions(sim_out)
    }
  }

  output$prevalence <- dplyr::left_join(prev[[1]], prev[[2]], by = "year")
  output$epi <- dplyr::left_join(epi[[1]], epi[[2]], by = c("year", "age_l", "age_u"))
  output$model_population <- population_size
  return(output)
}

get_age_disaggregated_output <- function(output_epi,
                                         output_ages,
                                         output_prevalence,
                                         site_population,
                                         site_interventions,
                                         model_pop_size,
                                         life_expectancy = 65){
  output_epi |>
    link_ages(output_ages) |>
    add_rates(model_pop_size) |>
    link_pop(site_population) |>
    link_tx_interventions(site_interventions) |>
    mortality_rate() |>
    cases() |>
    severe_cases() |>
    deaths() |>
    yll(life_expectancy = life_expectancy) |>
    yld() |>
    dalys() |>
    non_malarial_fevers() |>
    link_prevalence(prevalence = output_prevalence) |>
    add_tx() |>
    add_tx_costs()
}

get_age_aggregated_output <- function(age_disaggregated_output,
                                      output_prevalence,
                                      output_interventions,
                                      site_interventions,
                                      site_iso3c,
                                      model_pop_size){
  age_disaggregated_output |>
    aggregate_age() |>
    link_prevalence(prevalence = output_prevalence) |>
    link_interventions(modelled_interventions = output_interventions,
                       input_interventions = site_interventions) |>
    add_facility_visits() |>
    add_facility_costs(iso3c = site_iso3c) |>
    add_irs() |>
    add_irs_cost() |>
    add_pmc(modelled_population_size = model_pop_size) |>
    add_pmc_cost() |>
    add_smc(modelled_population_size = model_pop_size) |>
    add_smc_cost() |>
    add_rtss(modelled_population_size = model_pop_size) |>
    add_rtss_cost() |>
    add_nets(iso3c = site_iso3c) |>
    add_net_cost() |>
    add_surveillance_cost() |>
    add_case_detection_cost()
}
