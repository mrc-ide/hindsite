test_that("dx tx commodities", {
  x <- data.frame(
    cases_pf = c(0, 100),
    cases_pv = c(0, 100),
    pfpr_2_10 = c(0.06, 0.06),
    tx_cov = c(0.5, 0.5),
    prop_public = c(0.2, 0.2),
    prop_act = c(0.6, 0.6),
    non_malarial_fevers = c(100, 100))

  y <- add_tx(x)

  expect_equal(y$commodity_act_public, x$prop_public * x$prop_act * x$cases_pf * x$tx_cov)
  expect_equal(y$commodity_act_private,  (1 - x$prop_public) * x$prop_act * x$cases_pf * x$tx_cov)
  expect_equal(y$commodity_non_act_public, x$prop_public * (1 - x$prop_act) * x$cases_pf * x$tx_cov)
  expect_equal(y$commodity_non_act_private, (1 - x$prop_public) * (1 - x$prop_act) * x$cases_pf * x$tx_cov)
  expect_equal(y$commodity_primaquine_public, x$prop_public * x$cases_pv * x$tx_cov)
  expect_equal(y$commodity_primaquine_private, (1 - x$prop_public) * x$cases_pv * x$tx_cov)
  expect_equal(y$commodity_rdt_public, x$prop_public * x$prop_act * x$cases_pf * x$tx_cov)
  expect_equal(y$commodity_rdt_private, (1 - x$prop_public) * x$prop_act * x$cases_pf * x$tx_cov)
  expect_equal(y$commodity_microscopy_public, (x$prop_public * (1 - x$prop_act) * x$cases_pf * x$tx_cov) +
                 (x$prop_public * x$cases_pv * x$tx_cov))
  expect_equal(y$commodity_microscopy_private, ((1 - x$prop_public) * (1 - x$prop_act) * x$cases_pf * x$tx_cov) +
                 ((1 - x$prop_public) * x$cases_pv * x$tx_cov))
  expect_equal(y$commodity_nmf_rdt_public, x$prop_public * x$tx_cov * x$non_malarial_fevers * x$prop_act)
  expect_equal(y$commodity_nmf_rdt_private, (1 - x$prop_public) * x$tx_cov * x$non_malarial_fevers * x$prop_act)
  expect_equal(y$commodity_nmf_act_public, round(y$commodity_nmf_rdt_public * x$pfpr_2_10))
  expect_equal(y$commodity_nmf_act_private, round(y$commodity_nmf_rdt_private * x$pfpr_2_10))
})

test_that("dx tx costs", {
  x <- data.frame(
    age_l = c(0, 0),
    commodity_act_public = c(0, 200),
    commodity_nmf_act_public = c(0, 200),
    commodity_act_private = c(0, 200),
    commodity_nmf_act_private = c(0, 200),
    commodity_non_act_public = c(0, 200),
    commodity_non_act_private = c(0, 200),
    commodity_primaquine_public = c(0, 200),
    commodity_primaquine_private = c(0, 200),
    commodity_rdt_public = c(0, 200),
    commodity_nmf_rdt_public = c(0, 200),
    commodity_rdt_private = c(0, 200),
    commodity_nmf_rdt_private = c(0, 200),
    commodity_microscopy_public = c(0, 200),
    commodity_microscopy_private = c(0, 200)
    )

  y <- add_tx_costs(x)

  expect_equal(y$cost_act_public, treasure::cost_al(
    n_doses = (x$commodity_act_public + x$commodity_nmf_act_public) * 3 * 2 * 1))
  expect_equal(y$cost_act_private, treasure::cost_al(
    n_doses = (x$commodity_act_private + x$commodity_nmf_act_private) * 3 * 2 * 1))
  expect_equal(y$cost_microscopy_public, treasure::cost_rdt(n_tests = x$commodity_microscopy_public))
  expect_equal(y$cost_microscopy_private, treasure::cost_rdt(n_tests = x$commodity_microscopy_private))
  expect_equal(y$cost_non_act_public, treasure::cost_al(x$commodity_non_act_public * 3 * 2 * 1))
  expect_equal(y$cost_non_act_private, treasure::cost_al(x$commodity_non_act_private * 3 * 2 * 1))
  expect_equal(y$cost_primaquine_public, treasure::cost_primaquine(x$commodity_primaquine_public * 14 * 0.25 * 7.5 / 7.5))
  expect_equal(y$cost_primaquine_private, treasure::cost_primaquine(x$commodity_primaquine_private * 14 * 0.25 * 7.5 / 7.5))
})

test_that("facility visits", {
  x <- data.frame(
    tx_cov = c(0.5, 0.5),
    cases_pf = c(0, 100),
    cases_pv = c(0, 100),
    severe_cases_pf = c(0, 100),
    severe_cases_pv = c(0, 100),
    prop_public = c(0.2, 0.2)
  )
  y <- add_facility_visits(x)

  expect_equal(y$commodity_inpatient_visits, x$tx_cov * (x$severe_cases_pf + x$severe_cases_pv))
  expect_equal(y$commodity_outpatient_visits, x$tx_cov * (x$severe_cases_pf + x$severe_cases_pv) * x$prop_public)
})

test_that("facility costs", {
  x <- data.frame(
    commodity_inpatient_visits = c(0, 100),
    commodity_outpatient_visits = c(0, 100)
  )
  y <- add_facility_costs(x, iso3c = "BFA")
  facility_costs <- treasure::who_choice
  facility_costs <- facility_costs[facility_costs$ISO == "BFA",]

  expect_equal(y$cost_inpatient, round(x$commodity_inpatient_visits * facility_costs$inpatient))
  expect_equal(y$cost_outpatient, round(x$commodity_outpatient_visits * facility_costs$outpatient))

  y <- add_facility_costs(x, iso3c = "x")
  facility_costs <- treasure::who_choice

  expect_equal(y$cost_inpatient, round(x$commodity_inpatient_visits * median(facility_costs$inpatient)))
  expect_equal(y$cost_outpatient, round(x$commodity_outpatient_visits * median(facility_costs$outpatient)))
})

