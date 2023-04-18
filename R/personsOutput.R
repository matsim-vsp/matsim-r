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
    )

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
