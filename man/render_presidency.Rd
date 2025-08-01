% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-presidency-column.R
\name{render_presidency}
\alias{render_presidency}
\title{Render Presidency Information}
\usage{
render_presidency(
  value,
  dest_var,
  lookup_table = presidency_name_table,
  source_var = "presidency"
)
}
\arguments{
\item{value}{The name of the presidency to look up.}

\item{dest_var}{The specific information to retrieve (e.g., "first_day",
"id_presidency", "presidency_surnames", "presidency_year_es").}

\item{lookup_table}{The table containing presidency information (defaults to
presidency_name_table).}

\item{source_var}{The column name in the lookup table to use as values
(defaults to "presidency").}
}
\value{
The requested information about the presidency.
}
\description{
This function retrieves specific information about a presidency from the
presidency lookup table based on a given presidency name.
}
\examples{
render_presidency("Gonzalo Sanchez de Lozada (2nd)", "first_day") # "2002-08-06"
render_presidency("Gonzalo Sanchez de Lozada (2nd)", "id_presidency") # "p107"
render_presidency("Gonzalo Sanchez de Lozada (2nd)", "presidency_surnames")
render_presidency("Gonzalo Sanchez de Lozada (2nd)", "presidency_year_es")
render_presidency("p111", "presidency_commonname_es", source_var="id_presidency")
render_presidency("p111", "presidency_initials_num", source_var="id_presidency")
render_presidency("p111", "presidency_commonname", source_var="id_presidency")
}
