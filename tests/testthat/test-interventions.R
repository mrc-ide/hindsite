test_that("nets", {
  iso3c = "BFA"
  x <- data.frame(itn_use = c(0, 0.5),
                  par = c(100, 100))
  y <- add_nets(x, iso3c = iso3c)

  urd <- netz::get_usage_rate_data()
  ur <- urd[urd$iso3 == iso3c, "usage_rate"]
  ur_l <- stats::quantile(urd$usage_rate, 0.025)
  ur_u <- stats::quantile(urd$usage_rate, 0.975)

  hld <- netz::get_halflife_data()
  hl <- hld[hld$iso3 == iso3c, "half_life"]
  hl_l <- stats::quantile(hld$half_life, 0.025)
  hl_u <- stats::quantile(hld$half_life, 0.975)

  access = netz::usage_to_access(usage = x$itn_use, use_rate = ur)
  crop = netz::access_to_crop(access, type = "loess_extrapolate")
  commodity_nets_distributed = round(netz::crop_to_distribution_dynamic(crop = crop, net_loss_function = net_loss_map, half_life = hl) * x$par)

  expect_equal(y$commodity_nets_distributed, commodity_nets_distributed)

  access = netz::usage_to_access(usage = x$itn_use, use_rate = ur_u)
  crop = netz::access_to_crop(access, type = "loess_extrapolate")
  commodity_nets_distributed = round(netz::crop_to_distribution_dynamic(crop = crop, net_loss_function = net_loss_map, half_life = hl_u) * x$par)
  expect_equal(y$commodity_nets_distributed_lower, commodity_nets_distributed)

  access = netz::usage_to_access(usage = x$itn_use, use_rate = ur_l)
  crop = netz::access_to_crop(access, type = "loess_extrapolate")
  commodity_nets_distributed = round(netz::crop_to_distribution_dynamic(crop = crop, net_loss_function = net_loss_map, half_life = hl_l) * x$par)
  expect_equal(y$commodity_nets_distributed_upper, commodity_nets_distributed)

  test_bounds(y)

  y <- add_nets(x, iso3c = "X")
  ur <- median(urd$usage_rate)
  hl <- median(hld$half_life)
  access = netz::usage_to_access(usage = x$itn_use, use_rate = ur)
  crop = netz::access_to_crop(access, type = "loess_extrapolate")
  commodity_nets_distributed = round(netz::crop_to_distribution_dynamic(crop = crop, net_loss_function = net_loss_map, half_life = hl) * x$par)

  expect_equal(y$commodity_nets_distributed, commodity_nets_distributed)
  test_bounds(y)
})

test_that("net costs", {
  x <- data.frame(
    net_type = c("pyrethroid_only", "pyrethroid_pbo", "pyrethroid_pyrrole"),
    commodity_nets_distributed = 1
  )
  y <- add_net_cost(x)
  expect_equal(y$cost_itn, round(c(treasure::cost_llin(1), treasure::cost_pbo_itn(1), treasure::cost_pbo_itn(1, pbo_itn_unit_cost = 3.51 * 1.1))))
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

  expect_equal(y$commodity_irs_people_protected_lower, round(beta_coverage_ci(x$irs_cov, 0.025, 9720) * x$par))
  expect_equal(y$commodity_irs_households_sprayed_lower, round(beta_coverage_ci(x$irs_cov, 0.025, 9720) * x$par/ x$hh_size))

  expect_equal(y$commodity_irs_people_protected_upper, round(beta_coverage_ci(x$irs_cov, 0.975, 9720) * x$par))
  expect_equal(y$commodity_irs_households_sprayed_upper, round(beta_coverage_ci(x$irs_cov, 0.975, 9720) * x$par/ x$hh_size))

  test_bounds(y)
})

test_that("irs costs", {
  x <- data.frame(
    irs_cov = c(0, 0.8),
    par = c(100, 100),
    commodity_irs_people_protected = c(2, 2)
  )
  y <- add_irs_cost(x)
  expect_equal(y$cost_irs, round(treasure::cost_ll_irs_person(x$commodity_irs_people_protected)))
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
  expect_equal(y$cost_pmc, round(treasure::cost_ipti(x$commodity_pmc_doses)))
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

  test_bounds(y)
})

test_that("smc costs", {
  x <- data.frame(
    commodity_smc_doses = c(0, 1000)
  )
  y <- add_smc_cost(x)
  expect_equal(y$cost_smc, round(treasure::cost_smc(x$commodity_smc_doses)))
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
  expect_equal(y$cost_rtss, round(treasure::cost_rtss(x$commodity_rtss_doses)))
})
