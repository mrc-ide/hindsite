test_that("mortality rate", {
  x <- data.frame(sev = c(0, 0.1, 0.1),
                  tx_cov = c(0.2, 0.2, 0))
  y <- mortality_rate(x, scaler = 0.215)
  expect_equal(y$mort, (1 - (0.5 * x$tx_cov)) * 0.215 * x$sev)
})

test_that("cases", {
  x <- data.frame(inc = c(0, 0.1, 0.1),
                  par = c(100, 0, 100))

  y <- cases(x)
  expect_equal(y$cases, x$inc * x$par)

  test_bounds(y)
})

test_that("severe cases", {
  x <- data.frame(sev = c(0, 0.1, 0.1),
                  par = c(100, 0, 100))

  y <- severe_cases(x)
  expect_equal(y$severe_cases, x$sev * x$par)

  test_bounds(y)
})

test_that("deaths", {
  x <- data.frame(mort = c(0, 0.1, 0.1),
                  par = c(100, 0, 100))

  y <- deaths(x)

  expect_equal(y$deaths, x$mort * x$par)

  test_bounds(y)
})

test_that("yll", {
  x <- data.frame(deaths = c(0, 100),
                  deaths_lower = c(0, 10),
                  deaths_upper = c(10, 200),
                  age_mid = c(25, 25) * 365)

  life_expectancy <- 60
  y <- yll(x, life_expectancy = life_expectancy)
  expect_equal(y$yll, round((life_expectancy - x$age_mid / 365) * x$deaths))

  test_bounds(y)
})

test_that("yld", {
  x <- data.frame(cases = c(0, 100, 100, 100),
                  cases_lower = c(0, 10, 10, 10),
                  cases_upper = c(10, 200, 200, 200),
                  severe_cases = c(0, 100, 100, 100),
                  severe_cases_lower = c(0, 10, 10, 10),
                  severe_cases_upper = c(10, 200, 200, 200),
                  age_l = c(0, 0, 1825, 5475))

  y <- yld(x)
  expect_equal(y$yld, round(x$cases * 0.01375 * c(0.211, 0.211, 0.195, 0.172) +
                 x$severe_cases * 0.04795 * 0.6))

  test_bounds(y)
})

test_that("dalys", {
  x <- data.frame(yll = c(0, 10),
                  yll_lower = c(0, 5),
                  yll_upper = c(10, 20),
                  yld = c(0, 10),
                  yld_lower = c(0, 5),
                  yld_upper = c(10, 20))

  y <- dalys(x)
  expect_equal(y$dalys, x$yll + x$yld)
})

test_that("nmfs", {
  x <- data.frame(par = c(1000, 1000),
                  age_l = c(0, 1000))

  y <- non_malarial_fevers(x)
  expect_equal(y$non_malarial_fevers, c(3.4 * 1000, 1 * 1000))

  test_bounds(y)
})

test_that("add rates",{
  x <- data.frame(age = c(1, 1, 2, 2),
                  n = c(10, 20, 30, 40),
                  inc = c(0, 1, 0, 1),
                  sev = c(0, 1, 0, 1))
  modelled_population_size <- 1000
  y <- rates(x, modelled_population_size)
  expect_equal(y,
               data.frame(age = x$age,
                          inc = x$inc / x$n,
                          sev = x$sev / x$n,
                          prop = x$n / modelled_population_size))
})
