% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_trips.R
\name{plot_map_trips}
\alias{plot_map_trips}
\title{Plot start and end coordinates of the given trips table on an osm map}
\usage{
plot_map_trips(trips_table, crs, optimized = FALSE, shape_table = NULL)
}
\arguments{
\item{trips_table}{tibble of trips_output (from \link{read_output_trips})}

\item{crs}{numeric representation of the EPSG code or proj4string for the corresponding coordinate system of the trip coordinates,
can be found in network file from output directory of MATSim simulation}

\item{optimized}{optional, boolean, by default FALSE and creates interactive plot using leaflet, if TRUE uses ggplot}

\item{shape_table}{optional, sf object(data frame with geometries), can be created using st_read(), is used to categorize the trips, used as a background, default is NULL}
}
\value{
leaflet/ggplot plot with trips start-, end- points
}
\description{
Plot start and end coordinates of the given trips table on an osm map
}
