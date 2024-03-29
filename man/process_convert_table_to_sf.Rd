% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_trips.R
\name{process_convert_table_to_sf}
\alias{process_convert_table_to_sf}
\title{Transforms the data frame trips_output (from \link{read_output_trips}) from tibble to sf (table with geometry features)}
\usage{
process_convert_table_to_sf(table, crs, geometry.type = st_multipoint())
}
\arguments{
\item{table}{tibble trips_output (from \link{read_output_trips})}

\item{crs}{numeric, coordinate system in the form of the EPSG code or proj4string, can be found in the MATSim network file}

\item{geometry.type}{type of sf transformation, default is st_multipoint(), geometry.type can be:\cr
!!!st_point()- resulting table contains two geometry columns: start_wkt and end_wkt, representing start and end points as POINTS!!!  or\cr
!!!st_multipoint()- resulting table contains one geometry column, representing start and end points as MULTIPOINT!!! or\cr
!!!st_linestring() - resulting table contains one geometry column, representing  the line between start and end points as LINESTRING!!!\cr}
}
\value{
sf object (data frame with geometries depending on geometry.type)
}
\description{
Transforms the data frame trips_output (from \link{read_output_trips}) into an sf object using start_x, end_x, start_y, end_y as geometry features.\cr
If geometry.type = st_multipoint() or geometry.type = st_linestring() it adds one geometry column (wkt format),\cr
if geometry.type = st_point() it adds the geometry columns start_wkt and end_wkt.\cr
Added column/columns are projected to given CRS (coordinate reference system).\cr
The columns start_x, end_x, start_y, end_y are deleted from the resulting data frame.
}
