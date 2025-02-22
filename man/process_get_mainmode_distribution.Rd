% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_trips.R
\name{process_get_mainmode_distribution}
\alias{process_get_mainmode_distribution}
\title{Calculates main mode distribution}
\usage{
process_get_mainmode_distribution(trips_table, percentage = FALSE)
}
\arguments{
\item{trips_table}{tibble of output_trips (from \link{read_output_trips})}

\item{percentage}{boolean, by default FALSE, sets if output should be given as a percentage}
}
\value{
tibble of the distribution of main modes in the input trips_table.
}
\description{
The function process_get_mainmode_distribution calculates the distribution of main modes in a given trips_table.\cr
It provides insights into the frequency or percentage of each main mode within the dataset.
}
\details{
By default the function counts the occurrences of each unique main mode in the trips_table tibble and returns the counts. \cr
Alternatively, when the percentage parameter is set to TRUE, the function normalizes the counts to represent the percentage distribution of each main mode within the data set.
}
