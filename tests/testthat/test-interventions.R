test_that("nets", {
  iso3c = "BFA"
  x <- data.frame(itn_use = c(0, 0.5),
                  par = c(100, 100))
  y <- add_nets(x, iso3c = iso3c, country_ur = TRUE)

  urs <- netz::get_usage_rate_data()
  ur <- urs[urs$iso3 == iso3c,2]
  hls <- netz::get_halflife_data()
  hl <- hls[hls$iso3 == iso3c,2]
  access = netz::usage_to_access(usage = x$itn_use, use_rate = ur)
  crop = netz::access_to_crop(access, type = "loess_extrapolate")
  commodity_nets_distributed = netz::crop_to_distribution(crop = crop, distribution_freq = 3 * 365, half_life = hl) * x$par

  expect_equal(y$commodity_nets_distributed, commodity_nets_distributed)

  y <- add_nets(x, iso3c = iso3c, country_ur = FALSE)
  ur <- 0.88
  access = netz::usage_to_access(usage = x$itn_use, use_rate = ur)
  crop = netz::access_to_crop(access, type = "loess_extrapolate")
  commodity_nets_distributed = netz::crop_to_distribution(crop = crop, distribution_freq = 3 * 365, half_life = hl) * x$par

  expect_equal(y$commodity_nets_distributed, commodity_nets_distributed)

  y <- add_nets(x, iso3c = "X", country_ur = TRUE)
  ur <- median(urs$usage_rate)
  hl <- median(hls$half_life)
  access = netz::usage_to_access(usage = x$itn_use, use_rate = ur)
  crop = netz::access_to_crop(access, type = "loess_extrapolate")
  commodity_nets_distributed = netz::crop_to_distribution(crop = crop, distribution_freq = 3 * 365, half_life = hl) * x$par

  expect_equal(y$commodity_nets_distributed, commodity_nets_distributed)
})

test_that("net costs", {
  x <- data.frame(
    net_type = c("pyrethroid_only", "pyrethroid_pbo", "pyrethroid_pyrrole"),
    commodity_nets_distributed = 1
  )
  y <- add_net_cost(x)
  expect_equal(y$cost_itn, c(treasure::cost_llin(1), treasure::cost_pbo_itn(1), treasure::cost_pbo_itn(1, pbo_itn_unit_cost = 3.51 * 1.1)))
})

test_that("irs", {
  x <- data.frame(
    irs_cov = c(0, 0.8),
    par = c(100, 100),
    hh_size = c(2, 2)
  )
  y <- add_irs(x)
  expect_equal(y$commodity_irs_people_protected, x$irs_cov * x$par)
  expect_equal(y$commodity_irs_households_sprayed, x$irs_cov * x$par / x$hh_size)
})

test_that("irs costs", {
  x <- data.frame(
    irs_cov = c(0, 0.8),
    par = c(100, 100),
    commodity_irs_people_protected = c(2, 2)
  )
  y <- add_irs_cost(x)
  expect_equal(y$cost_irs, treasure::cost_ll_irs_person(x$commodity_irs_people_protected))
})

test_that("pmc", {
  x <- data.frame(
    n_pmc_treated = c(0, 10),
    par_pf = c(100, 100)
  )
  y <- add_pmc(x, modelled_population_size = 10)
  expect_equal(y$commodity_pmc_doses, x$par_pf / 10 * x$n_pmc_treated)
  expect_equal(y$commodity_pmc_children_protected, round(x$par_pf / 10 * x$n_pmc_treated / 3))
})

test_that("pmc costs", {
  x <- data.frame(
    commodity_pmc_doses = c(0, 1000)
  )
  y <- add_pmc_cost(x)
  expect_equal(y$cost_pmc, treasure::cost_ipti(x$commodity_pmc_doses))
})

test_that("smc", {
  x <- data.frame(
    n_smc_treated = c(0, 10),
    par_pf = c(100, 100),
    smc_n_rounds = 4
  )
  y <- add_smc(x, modelled_population_size = 10)
  expect_equal(y$commodity_smc_doses, x$par_pf / 10 * x$n_smc_treated)
  expect_equal(y$commodity_smc_children_protected, round(x$par_pf / 10 * x$n_smc_treated / x$smc_n_rounds))
})

test_that("smc costs", {
  x <- data.frame(
    commodity_smc_doses = c(0, 1000)
  )
  y <- add_smc_cost(x)
  expect_equal(y$cost_smc, treasure::cost_smc(x$commodity_smc_doses))
})

test_that("rtss", {
  x <- data.frame(
    n_rtss_doses = c(0, 10),
    par_pf = c(100, 100)
  )
  y <- add_rtss(x, modelled_population_size = 10)
  expect_equal(y$commodity_rtss_doses, x$par_pf / 10 * x$n_rtss_doses)
  expect_equal(y$commodity_rtss_children_protected, round(x$par_pf / 10 * x$n_rtss_doses / 4))
})

test_that("rtss costs", {
  x <- data.frame(
    commodity_rtss_doses = c(0, 1000)
  )
  y <- add_rtss_cost(x)
  expect_equal(y$cost_rtss, treasure::cost_rtss(x$commodity_rtss_doses))
})
