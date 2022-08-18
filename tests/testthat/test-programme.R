test_that("surveillance", {
  x <- data.frame(par = c(0, 100))
  y <- add_surveillance_cost(x)
  expect_equal(y$cost_surveillance, treasure::cost_surveillance(x$par))
})

test_that("case detection", {
  x <- data.frame(
    par_pf = c(10000, 10000, 10000),
    cases_pf = c(0, 1, 100),
    par_pv = c(10000, 10000, 10000),
    cases_pv = c(0, 1, 100)
  )
  y <- add_case_detection_cost(x)
  expect_equal(y$cost_proactive_case_detection,
               c(0,
                 treasure::cost_pacd(n_tested = x$cases_pf[2] + x$cases_pv[2]),
                 0))

})
