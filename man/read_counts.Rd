% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counts.R
\name{read_counts}
\alias{read_counts}
\title{Load a MATSim Counts file into memory

Loads a MATSim Counts XML-file as tibble into memory}
\usage{
read_counts(input_path)
}
\arguments{
\item{input_path}{character string, file path to the .xml file}
}
\value{
tibble of the counts file with the link id ("loc_id") as key
}
\description{
Load a MATSim Counts file into memory

Loads a MATSim Counts XML-file as tibble into memory
}
