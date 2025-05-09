% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event-outcomes-table.R
\name{wide_event_outcomes_table}
\alias{wide_event_outcomes_table}
\title{Generate a Table of Event Counts}
\usage{
wide_event_outcomes_table(deaths, deaths_unfiltered, event.status, ...)
}
\arguments{
\item{deaths}{A data frame containing filtered deaths data.}

\item{deaths_unfiltered}{A data frame containing unfiltered deaths data.}

\item{event.status}{An event status data table to be passed to
event_description_table()}

\item{...}{Additional arguments passed to the `count_range_by` function.}
}
\value{
A table of event counts.
}
\description{
This function generates a table of event counts based on the provided deaths data.
}
\examples{
deaths_aug24_filtered <- standard_filter(deaths_aug24)
deaths_aug24_unfiltered <- deaths_aug24
wide_event_outcomes_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
  event_status_aug24)
}
