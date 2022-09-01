test_that("format species works", {
  raw <- data.frame(timestep = 1)
  f1 <- add_missing_columns(x = raw, species = "pf")
  expect_equal(colnames(f1),
               c("timestep", "n_detect_730_3649", "n_730_3649",
                 "n_inc_clinical_0_1824", "n_inc_clinical_1825_5474",  "n_inc_clinical_5475_36499",
                 "n_inc_severe_0_1824",  "n_inc_severe_1825_5474", "n_inc_severe_5475_36499",
                 "n_age_0_1824", "n_age_1825_5474", "n_age_5475_36499",
                 "n_rtss_epi_dose_1", "n_rtss_epi_dose_2", "n_rtss_epi_dose_3", "n_rtss_epi_booster_1",
                 "n_pmc_treated", "n_smc_treated"))
  expect_equal(sum(f1), 1)

  f2 <- add_missing_columns(x = raw, species = "pv")
  expect_equal(colnames(f2),
               c("timestep", "n_detect_365_36499", "n_365_36499",
                 "n_inc_clinical_0_1824", "n_inc_clinical_1825_5474",  "n_inc_clinical_5475_36499",
                 "n_inc_severe_0_1824",  "n_inc_severe_1825_5474", "n_inc_severe_5475_36499"))
  expect_equal(sum(f2), 1)
})

test_that("add year",{
  x <- data.frame(timestep  = 1:(365*3))
  y <- add_year(x)
  expect_equal(y$year, rep(2000:2002, each = 365))
  y <- add_year(x, 2001)
  expect_equal(y$year, rep(2001:2003, each = 365))
})

test_that("add age mid", {
  x <- data.frame(age_l = 0, age_u = 1e6)
  y <- exp_mid(x$age_l, x$age_u)
  expect_equal(y / 365, 19)
})
