% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event-outcomes-table.R
\name{event_description_table}
\alias{event_description_table}
\title{Generate a Table of Event Descriptions}
\usage{
event_description_table(deaths)
}
\arguments{
\item{deaths}{A data frame containing deaths data.}
}
\value{
A table of event descriptions.
}
\description{
This function generates a table of event descriptions based on the provided deaths data.
It assigns presidential administrations and years to each event title.
}
\examples{
event_description_table(deaths_aug24)
}
