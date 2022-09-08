#' Beta distributed uncertainty for coverage-related commodity outputs
#'
#' @param coverage Coverage
#' @param q Quantile
#' @param average_sample Average sample size for coverage estimate. These can
#' be informed via the denominators from DHS surveys (for example, median sample
#' size from most recent surveys for IRS = 9720, for children with fever taking
#' an act = 1473).
#'
#' @return Uncertainty estimates on coverage
beta_coverage_ci <- function(coverage, q, average_sample){
  stats::qbeta(q, coverage * average_sample, (1 - coverage) * average_sample)
}

#' Uncertainty approximation for epi outcomes
#'
#' @param n N number of cases/deaths
#' @param q Quantile
#' @param cv Uncertainty SD scaler, cases = 0.227, deaths = 0.265
epi_uncertainty_approximation <- function(n, q, cv){
  round(pmax(0, stats::qnorm(q, n, n * cv)))
}
