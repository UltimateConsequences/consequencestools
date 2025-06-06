% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-cleaning.R
\name{assign_protest_domain_levels}
\alias{assign_protest_domain_levels}
\title{Assign Protest Domain Levels}
\usage{
assign_protest_domain_levels(dataframe, na.level = "Unknown")
}
\arguments{
\item{dataframe}{A dataframe containing a 'state_responsibility' column}

\item{na.level}{The name of the level to which NA is assigned}
}
\value{
A dataframe with the 'protest_domain' column turned into an
  ordered factor
}
\description{
This function factors the variable `protest_domain`.
}
\examples{
assign_protest_domain_levels(deaths_aug24)
}
