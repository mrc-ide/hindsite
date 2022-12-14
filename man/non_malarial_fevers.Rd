% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi.R
\name{non_malarial_fevers}
\alias{non_malarial_fevers}
\title{non_malarial_fevers}
\usage{
non_malarial_fevers(x, rate_under_5 = 3.4, rate_over_5 = 1)
}
\arguments{
\item{x}{output}

\item{rate_under_5}{Annual incidence of NMFs in children under 5}

\item{rate_over_5}{Annual incidence of NMFs in individuals over 5}
}
\value{
output with nmfs
}
\description{
Assume a constant rate of NMFs in under 5s and over 5s. This follows methodology
in \href{https://gh.bmj.com/content/2/2/e000176}{Patouillard _et al_, 2017}.
}
