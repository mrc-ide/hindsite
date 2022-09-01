test_that("mortality rate", {
  x <- data.frame(severe_pf = c(0, 0.1, 0.1),
                  severe_pv = c(0, 0.1, 0.1),
                  tx_cov = c(0.2, 0.2, 0))
  y <- mortality_rate(x)
  expect_equal(y$mortality_pf, (1 - (0.5 * x$tx_cov)) * 0.215 * x$severe_pf)
  expect_equal(y$mortality_pv, (1 - (0.5 * x$tx_cov)) * 0.003 * x$severe_pv)
})

test_that("cases", {
  x <- data.frame(clinical_pf = c(0, 0.1, 0.1),
                  clinical_pv = c(0, 0.1, 0.1),
                  par_pf = c(100, 0, 100),
                  par_pv = c(100, 0, 100))

  y <- cases(x)
  expect_equal(y$cases_pf, x$clinical_pf * x$par_pf)
  expect_equal(y$cases_pv, x$clinical_pv * x$par_pv)
})

test_that("severe cases", {
  x <- data.frame(severe_pf = c(0, 0.1, 0.1),
                  severe_pv = c(0, 0.1, 0.1),
                  par_pf = c(100, 0, 100),
                  par_pv = c(100, 0, 100))

  y <- severe_cases(x)
  expect_equal(y$severe_cases_pf, x$severe_pf * x$par_pf)
  expect_equal(y$severe_cases_pv, x$severe_pv * x$par_pv)
})

test_that("deaths", {
  x <- data.frame(mortality_pf = c(0, 0.1, 0.1),
                  mortality_pv = c(0, 0.1, 0.1),
                  par_pf = c(100, 0, 100),
                  par_pv = c(100, 0, 100))

  y <- deaths(x)
  expect_equal(y$deaths_pf, x$mortality_pf * x$par_pf)
  expect_equal(y$deaths_pv, x$mortality_pv * x$par_pv)
})

test_that("yll", {
  x <- data.frame(deaths_pf = c(0, 100),
                  deaths_pv = c(0, 100),
                  age_mid = c(25, 25) * 365)

  life_expectancy <- 60
  y <- yll(x, life_expectancy = life_expectancy)
  expect_equal(y$yll, (life_expectancy - x$age_mid / 365) * (x$deaths_pf + x$deaths_pv))
})

test_that("yld", {
  x <- data.frame(cases_pf = c(0, 100, 100, 100),
                  cases_pv = c(0, 100, 100, 100),
                  severe_cases_pf = c(0, 100, 100, 100),
                  severe_cases_pv = c(0, 100, 100, 100),
                  age_l = c(0, 0, 1825, 5475))

  y <- yld(x)
  expect_equal(y$yld, (x$cases_pf + x$cases_pv)  * 0.01375 * c(0.211, 0.211, 0.195, 0.172) +
                 (x$severe_cases_pf + x$severe_cases_pv) * 0.04795 * 0.6)
})

test_that("dalys", {
  x <- data.frame(yll = c(0, 10),
                  yld = c(0, 10))

  y <- dalys(x)
  expect_equal(y$dalys, x$yll + x$yld)
})

test_that("nmfs", {
  x <- data.frame(par = c(1000, 1000),
                  age_l = c(0, 1000))

  y <- non_malarial_fevers(x)
  expect_equal(y$non_malarial_fevers, c(3.4 * 1000, 1 * 1000))
})

test_that("add rates",{
  x <- data.frame(age = c(1, 1, 2, 2),
                  prop = c(0.25,0.75, 0.25, 0.75),
                  clinical_pf = c(0, 1, 0, 1),
                  severe_pf = c(0, 1, 0, 1),
                  clinical_pv = c(0, 1, 0, 1),
                  severe_pv = c(0, 1, 0, 1))
  modelled_population_size <- 1000
  y <- add_rates(x, modelled_population_size)
  expect_equal(y,
               data.frame(age = x$age,
                          prop = x$prop,
                          clinical_pf = x$clinical_pf / (modelled_population_size * x$prop),
                          severe_pf = x$severe_pf / (modelled_population_size * x$prop),
                          clinical_pv = x$clinical_pv / (modelled_population_size * x$prop),
                          severe_pv = x$severe_pv / (modelled_population_size * x$prop)))
})
