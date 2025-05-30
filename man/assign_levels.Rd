% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-cleaning.R
\name{assign_levels}
\alias{assign_levels}
\title{Assign Levels to Multiple Variables}
\usage{
assign_levels(dataframe, ..., .simplify = TRUE)
}
\arguments{
\item{dataframe}{A dataframe to which levels will be assigned}

\item{...}{Variable names to which levels should be assigned}

\item{.simplify}{Logical, whether to simplify the levels, where this
applies (default: TRUE)}
}
\value{
A dataframe with assigned levels
}
\description{
This function assigns levels to multiple variables in a dataframe,
deploying the relevant functions. You can use "standard" (or)
to use the default list `standard_factoring_variables`.
}
\details{
This function applies level assignment to specified variables in the dataframe.
It only processes variables that exist in both the input list and the dataframe.
}
\examples{
de <- assign_levels(deaths_aug24, "pres_admin", "state_responsibility", "state_perpetrator")

}
