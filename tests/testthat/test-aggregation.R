test_that("age aggregation works", {
  x <- data.frame(
    year = c(2000, 2000),
    pfpr_2_10 = c(0.1, 0.2),
    pvpr_1_99 = c(0.2, 0.4),
    par_pf = c(0, 1),
    par_pv = c(0, 2),
    pop = c(0, 3),
    cases1 = c(0, 4),
    cases2 = c(0, 5),
    severe1 = c(0, 6),
    severe2 = c(0, 7),
    yll = c(0, 8),
    yld = c(0, 9),
    dalys = c(0, 10),
    commodity1 = c(0, 11),
    commodity2 = c(0, 12),
    cost1 = c(0, 13),
    cost2 = c(0, 14)
  )
  y <- aggregate_age(x)
  expect_equal(as.numeric(y[1,]), c(2000, 0.2, 0.4, 1:14))
  expect_equal(colnames(y), colnames(x))
})
