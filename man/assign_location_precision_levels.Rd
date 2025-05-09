% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-cleaning.R
\name{assign_location_precision_levels}
\alias{assign_location_precision_levels}
\title{Assign Location Precision Levels}
\usage{
assign_location_precision_levels(dataframe)
}
\arguments{
\item{dataframe}{A dataframe containing a 'state_responsibility' column}
}
\value{
A dataframe with the 'location_precision' column turned into an
  ordered factor
}
\description{
This function factors the variable `location_precision`.
}
\examples{
assign_location_precision_levels(deaths_aug24)
}
