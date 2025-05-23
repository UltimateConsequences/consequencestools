% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/id-lookup.R
\name{add_id_for_president}
\alias{add_id_for_president}
\title{Add an unique identifier for presidency (id_pres) column to a dataset}
\usage{
add_id_for_president(
  dataset,
  pres_table = presidency_name_table,
  overwrite = FALSE
)
}
\arguments{
\item{dataset}{A dataframe with a column containing the name of the president,
either named `presidency` or `pres_admin`}

\item{pres_table}{A data frame containing the presidency name table}

\item{overwrite}{A boolean indicating whether to overwrite an existing
column.}
}
\value{
A dataframe with a new column `id_pres` containing the unique,
  placed after the `presidency` or `pres_admin` column.
}
\description{
Add an unique identifier for presidency (id_pres) column to a dataset
}
\examples{
add_id_for_president(deaths_aug24) \%>\% dplyr::select(1, pres_admin, id_pres)
}
