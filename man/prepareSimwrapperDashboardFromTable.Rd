% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simwrapper.R
\name{prepareSimwrapperDashboardFromTable}
\alias{prepareSimwrapperDashboardFromTable}
\title{Creates dashboard for the given table with data}
\usage{
prepareSimwrapperDashboardFromTable(
  table,
  dump.output.to = matsimDumpOutputDirectory,
  append = FALSE
)
}
\arguments{
\item{table}{trips_output tibble from readTripsTable()}

\item{dump.output.to}{output folder, yaml files and plots from the following functions are saved there:
plotModalSplitBarChart(),plotModalSplitPieChart(),plotModalShift().}

\item{append}{specifies if the ouput folder should be erased before creating, default = FALSE}
}
\value{
generates folder with content for simwrapper out of trips table
}
\description{
Creates dashboard for the given table with data
}
