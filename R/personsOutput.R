matsimDumpOutputDirectory <- "./matsim_r_output"
dashboard_file <- "/dashboard-1-trips.yaml"


#' Load MATSim output_persons table into memory
#'
#' Loads a MATSim CSV output_persons from file or archive,
#' creating a tibble with columns as in csv file
#' copied + adopted code from readTripsTable in tripsOutput.R
#'
#' @param input_path is a character string, path to the local MATSim output directory, to the persons csv directly, or a http link to the file.
#' @param n_max integer, maximum number of lines to read within output_persons

#' @return tibble of output_persons
#'
#' @export
readPersonsTable <- function(input_path = ".",  n_max = Inf) {
  options(digits = 18)

  persons_file <- ""

  if(dir.exists(input_path)){
    files <- list.files(input_path, full.names = TRUE)
    person_file_indicies <- grep("output_persons.csv.gz$", files)

    if(length(person_file_indicies) == 1){
      persons_file <- files[person_file_indicies]
    } else {
      stop('There is supposed to be a single "output_persons.csv.gz" found in directory')
    }
  } else {
    persons_file <- input_path
  }

  persons_output_table <- read_delim(persons_file,
                                     delim = ";",
                                     locale = locale(decimal_mark = "."),
                                     n_max = n_max,
                                     col_types = cols(
                                       executed_score = col_character(),
                                       first_act_x = col_character(),
                                       first_act_y = col_character(),
                                       first_act_type = col_character(),
                                     )
  )

  persons_output_table <- persons_output_table %>%
    mutate(
      executed_score = as.double(executed_score),
      first_act_x = as.double(first_act_x),
      first_act_y = as.double(first_act_y),
    ) %>%
    separate(first_act_type, sep = "_", into = c("first_act_type", "typical_duration"))

  attr(persons_output_table,"table_name") <- input_path

  return(persons_output_table)
}

#' @param personTibble_base persons tibble of the base case, can be loaded with readPersonsTable.
#' @param personTibble_policy persons tibble of the policy case, can be loaded with readPersonsTable.
#'
#' @return ggplot boxplot of the distribution of the score differences
#'
#' @export
boxplotScoreDifferences <- function(personTibble_base, personTibble_policy){
  joined <- inner_join(personTibble_base, personTibble_policy, by = "person", suffix = c("_base", "_policy")) %>%
    select(person,
           executed_score_base,
           executed_score_policy) %>%
    mutate(score_diff = executed_score_policy - executed_score_base)

  result <- ggplot(joined, aes(y = score_diff)) +
    geom_boxplot(fill = "#0099f8") +
    labs(
      title = "Distribution of score differences",
      subtitle = "score_delta = score(policy) - score(base)",
      #caption = "Source: MTCars dataset",
      y = "score_delta"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
      plot.caption = element_text(face = "italic"),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    )
  result
}

# TODO: overlap between these function above and function below

#' Joins base and policy person tables
#'
#' Uses inner_join to combine base and policy persons tables.
#' Calculates difference in score between base and policy cases.
#'
#' @param base_persons persons tibble of the base case, can be loaded with readPersonsTable.
#' @param policy_persons persons tibble of the policy case, can be loaded with readPersonsTable.
#' @param crs coordinate reference system for first_act_x and first_act_y coordinates; enter in form of EPSG code. Default is EPSG:25832

#' @return tibble of joined persons
#'
#' @export
join_base_and_policy_persons <- function(base_persons, policy_persons, crs = "EPSG:25832"){

  joined <- base_persons %>%
    inner_join(policy_persons %>% dplyr::select(person,executed_score), by = "person", suffix = c("_base", "_policy")) %>%
    mutate(score_diff = executed_score_policy - executed_score_base) %>%
    relocate(contains("score"), .after = person)

  return(joined)
}

#' Transforms persons_table tibble (from readPersonsTable) from tibble to sf (table with attribute features and geometry feature)
#'
#' Takes first activity location of person turns it into simple feature POINT geometry
#'
#' @param persons table containing persons (from readPersonsTable)
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#' @param filter_shp (optional) if provided, will filter persons_table to only include first_activity_locations within given shape file
#' @param first_act_type_filter (optional) if provided, will filter persons_table to only include agents whose day starts with a certain activity type (e.g. home)
#'
#' @return sf object (data.frame with geometries depending to geometry.type)
#'
#' @export
transform_persons_sf <- function(persons, crs = "EPSG:25832", filter_shp = NULL, first_act_type_filter = NULL) {

  if(!is.null(first_act_type_filter)){
    persons <- persons %>% filter(first_act_type == first_act_type_filter)
  }

  persons_sf <- persons %>%
    st_as_sf(coords = c("first_act_x", "first_act_y"), crs = crs)

  if(!is.null(filter_shp)){
    persons_sf <- persons_sf %>% st_intersection(filter_shp %>% st_transform(crs))
  }
  return(persons_sf)
}


#' Overlays hexagonal grid over study area, and creates summary statistics for each pixel
#'
#' CRS of shape file must match CRS of persons sf object
#'
#' @param persons simple feature dataframe containing persons, including score_base, score_policy, and score_diff, income, carAvail, sim_ptAbo
#' @param shp shape file onto which grid is overlaid
#' @param n integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#'
#' @return sf object of hexagonal grid
#'
#' @export
persons_attributes_on_hex_grid <- function(persons, shp, n = 20){
  hex_grid <- st_make_grid(shp, square = F, n = n) %>%
    st_as_sf() %>%
    mutate(id = row_number())

  joined_hex <- hex_grid %>%
    st_join(persons) %>%
    group_by(id) %>%
    summarise(cnt = n(),
              income = mean(income, na.rm = T),
              carAvail = sum(carAvail=="always") / n(),
              sim_ptAbo = sum(sim_ptAbo=="full") / n(),
              executed_score_base = mean(executed_score_base, na.rm = TRUE),
              executed_score_policy = mean(executed_score_policy, na.rm = TRUE),
              score_diff = mean(score_diff, na.rm = TRUE)) %>%
    mutate(area = st_area(.) %>% units::set_units(km^2)) %>%
    mutate(pop_density = cnt / area) %>%
    filter(!is.na(executed_score_base))

  return(joined_hex)

}

