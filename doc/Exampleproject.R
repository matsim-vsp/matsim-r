## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install master,eval=FALSE------------------------------------------------
#  devtools::install_github("matsim-vsp/matsim-r")

## ----eval=FALSE---------------------------------------------------------------
#  library(matsim)
#  library(sf)

## ----eval=FALSE---------------------------------------------------------------
#   trips_file_path <- "paste_file_path_here"
#  
#   # if you don't want to immediately load the whole file, the function read_output_trips has the option to only load the first n lines
#   output_trips <- read_output_trips(trips_file_path, n_max = 5000)

## ----eval=FALSE---------------------------------------------------------------
#   plot_mainmode_barchart(output_trips)

## ----eval = FALSE-------------------------------------------------------------
#  trips_originating <- process_filter_by_shape(output_trips, leipzig_shp, crs = 25832, spatial_type = "originating" )

## ----eval = FALSE-------------------------------------------------------------
#  #loading the scenario data
#  output_trips_scenario <- read_output_trips(trips_file_scenario_path, n_max = 5000)
#  
#  #filtering the scenario data
#  trips_scenario_originating <- process_filter_by_shape(output_trips_scenario, leipzig_shp, crs = 25832, spatial_type = "originating")
#  
#  #creating the comparison plot
#  plot_compare_mainmode_barchart(trips_originating, trips_scenario_originating)

## ----eval=FALSE---------------------------------------------------------------
#  plot_map_trips(trips_originating, crs = 25832)

