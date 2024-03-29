% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counts.R
\name{readCounts}
\alias{readCounts}
\title{Load a MATSim Counts file into memory}
\usage{
readCounts(file)
}
\arguments{
\item{file}{File to load. Must be an .xml file}
}
\value{
tibble with MATSim link id ("loc_id") as key
}
\description{
Loads a MATSim Counts XML-file as tibble into memory
}
