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
