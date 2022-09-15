#' Add appropriate species suffix to column names
#'
#' @param x output
#' @param suffix species suffix (pf or pv)
#'
#' @return renamed output
rename_cols <- function(x, suffix){
  to_rename <- c(
    "prop",
    "cases", "cases_lower", "cases_upper",
    "severe_cases", "severe_cases_lower", "severe_cases_upper",
    "deaths", "deaths_lower", "deaths_upper",
    "yll", "yll_lower", "yll_upper",
    "yld", "yld_lower", "yld_upper",
    "dalys", "dalys_lower", "dalys_upper")

  names <- colnames(x)
  index <- names %in% to_rename
  names[index] <- paste0(names[index], "_", suffix)
  colnames(x) <- names
  return(x)
}

#' Add year column based on model timestep and baseline year
#'
#' @param x model output
#' @param baseline_year start year
#'
#' @return output with year column
#' @export
add_year <- function(x, baseline_year = 2000){
  x |>
    dplyr::mutate(year = floor((.data$timestep - 1) / 365) + baseline_year)
}

#' Approximate mean age within an age bracket assuming exponential
#' age distribution
#'
#' @param l age bracket min
#' @param u age bracket max
#' @param average_age Average age in days
#'
#' @return output with average age column
exp_mid <- function(l, u, average_age =  19 * 365){
  rate <- 1 / average_age
  out <- purrr::map2_dbl(l, u, function(l, u, rate){
    stats::weighted.mean(l:u, c(stats::pexp(l:u, rate, lower.tail = FALSE)))
  }, rate = rate)
  return(round(out))
}
