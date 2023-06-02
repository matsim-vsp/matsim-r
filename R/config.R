#' Load MATSIM config file into Memory
#'
#' Loads a MATSim xml config from file or archive,
#' creating a list with parameters as in xml file
#'
#' @param input_path character string, path to matsim output directory or http link to the file.
#' @param n_max integer, maximum number of lines to read within output_trips
#' @return tibble of trips_output
#'
#' @export
read_config <- function(input_path = ".", n_max = Inf) {

  result_tibble <- tibble(module = character(),
                          param = character(),
                          value = character())

  config_xml = read_xml(input_path)

  module_nodeset = xml_children(config_xml)

  for(i in 1:length(module_nodeset)){
    module_name = xml_attr(module_nodeset[[i]],"name")
    print(module_name)
    param_nodeset = xml_children(module_nodeset[[i]])
    for(j in 1:length(param_nodeset)){
      param_name = xml_attr(param_nodeset[[j]],"name")
      value_name = xml_attr(param_nodeset[[j]],"value")
      #print(param_name,value_name)
      temp_tibble = tibble(module = module_name,
                           param = param_name,
                           value = value_name
      )
      result_tibble = rbind(result_tibble,temp_tibble)
    }

  }

  return(result_tibble)

}
