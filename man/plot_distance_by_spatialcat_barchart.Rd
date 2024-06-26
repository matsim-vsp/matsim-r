% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_trips.R
\name{plot_distance_by_spatialcat_barchart}
\alias{plot_distance_by_spatialcat_barchart}
\title{Plots a bar chart with spatial type on x-axis and traveled distance on y-axis}
\usage{
plot_distance_by_spatialcat_barchart(
  trips_table,
  shape_table,
  crs,
  euclidean = FALSE
)
}
\arguments{
\item{trips_table}{tibble of trips_output (from \link{read_output_trips})}

\item{shape_table}{sf object(data frame with geometries), can be created using st_read(), is used to categorize the trips.}

\item{crs}{numeric representation of the EPSG code or proj4string for the corresponding coordinate system of the trip coordinates,
can be found in network file from output directory of MATSim simulation}

\item{euclidean}{boolean indicating whether to calculate the average as euclidean distance or as travel distance. Default is FALSE, which calculates the average traveled distance.}
}
\value{
Bar chart plot of distance traveled by spatial type
}
\description{
Takes the tibble created by \link{read_output_trips} and an sf object (can be created using the function st_read())
to first use the function \link{process_append_spatialcat} to categorize all trips into the following spatial categories:
- inside: trips that start and end inside the given shape
- originating: trips that start inside the shape and end outside of the shape
- destinating: trips that end inside the shape and start outside of the shape
- outside: trips that start and end outside of the shape
}
\details{
In a next step a bar chart of the sum of traveled distance for each trip type is created.
}
