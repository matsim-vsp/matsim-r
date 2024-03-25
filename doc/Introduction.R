## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE---------------------------------------------------
library(matsim)

## ----install master,eval=FALSE------------------------------------------------
#  devtools::install_github("matsim-vsp/matsim-r")

## ----install ref,eval=FALSE---------------------------------------------------
#  devtools::install_github("matsim-vsp/matsim-r", ref = "{name of the branch}")

## ----eval=FALSE---------------------------------------------------------------
#  library(matsim)

## ----reading, warning=FALSE,eval = FALSE--------------------------------------
#  # Specify the file path to your simulation output.
#  # It can be a directory path or direct path to a file
#  output_file <- "path/to/your/output_trips.csv"
#  
#  # Use the read_csv_trips() function to read the simulation data
#  trips_data <- read_output_trips(output_file)

## ----processing, warning=FALSE,eval=FALSE-------------------------------------
#  # Filter the trips data to include only weekday trips
#  weekday_trips <- process_filter_by_shape(trips_data)
#  
#  # Calculate the average trip duration
#  average_duration <- process_get_travelwaittime_by_mainmode(weekday_trips)

## ----plotting, eval = FALSE---------------------------------------------------
#  # Create a scatter plot of trip distance among different main modes
#  plot_distance_by_mainmode_barchart(trips_data, x = "distance", y = "duration")

