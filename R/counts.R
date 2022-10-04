library(xml2)
library(tidyverse)

#' Load a MATSim Counts file into memory
#'
#' Loads a MATSim Counts XML-File as Dataframe into memory
#'
#'
#'@param file File to load. Must be an .xml file
#'
#'@return dataframe containing with MATSim Link id as "loc_id" as key
#'
#'@export
readCounts <- function(file){

  message = paste("Read counts file from", file)
  print(message)

  counts.xml <- read_xml(file)

  station = xml_find_all(counts.xml, "//count") %>%
    xml2::xml_attrs() %>%
    purrr::map_df(~as.list(.)) %>%
    readr::type_convert()

  volume = xml_find_all(counts.xml, "//volume") %>%
    xml2::xml_attrs() %>%
    purrr::map_df(~as.list(.)) %>%
    readr::type_convert()

  bind_cols(station, volume)
}

#' Load linkstats as tibble into memory
#'
#' Reads Linkstats as .tsv created from LinkStats.class
#' as dataframe into memory.
#' Counts can be provided in any time bins.
#' Counts can be provided for any qsim mode. The argument networkModes is used to
#' select and filter the columns.
#'
#'
#'@param file File to load. Must be an .csv or .tsv file with comma separator
#'
#'@param runId Id to tag columns with DTV
#'
#'@param sampleSize sample size of the MATSim scenario to scale DTV values
#'
#'@param networkModes a vector with network modes, which are needed for analysis
#'
#'@return Tibble with DTV values for each count station for each qsim mode
#'
#'@export
readLinkStats <- function(runId, file, sampleSize = 0.25){

  if(str_detect(string = runId, pattern = "_")){
    message <- "runId cannot contain '_'..."
    warning(message)
    return(NA)
  }

  message <- paste("Read in link stats from run", runId, ". Loading data from", file )
  print(message)

  linkstats <- readr::read_csv(file = file)

  volumeModes <- linkstats %>%
    select(starts_with("vol_")) %>%
    colnames()

  linkstats.1 <- linkstats %>%
    mutate_at(volumeModes, function(x){ x * (1 / sampleSize)})

  names = colnames(linkstats.1)
  newNames = character()

  for(name in names){

    if(str_detect(name, pattern = "vol_")){
      name = paste0(name, "_", runId)
    }

    newNames[length(newNames) + 1] = name
  }

  colnames(linkstats.1) = newNames

  linkstats.1
}


#' Load Counts, a limited number of Linkstats and Network links as joined tibble into memory
#'
#'Function to import and join MATSim Counts, Linkstats and network link types
#'Linkstats will be aggregated to DTV. An additional sampleSize parameter can be
#'used to scale DTV values.
#'The parameter networkModes contains a set of network modes to filter and select
#'columns of link stats.
#'
#'
#'@param counts Tibble with counts data
#'
#'@param network Tibble with network nodes and links
#'
#'@param linkStatsList List with filepaths to Linkstats, uses the runId as key, and filepath as value
#'
#'@param sampleSize sample size of the scenario between 0 and 1 to rescale DTV values
#'
#'@param networkModes a vector with network modes, which are needed for analysis
#'
#'@return Tibble with MATSim link id as key ("loc_id"), DTV from MATSim runs and link type
#'
#'@examples
#'
#'runList = list("run_1" = "path/to/linkstats_1",
#'               "run_2" = "path/to/linkstats_2"
#'               )
#'
#'mergeCountsAndLinks(
#'  counts = countsDataFrame,
#'  network = networkDataFrame,
#'  linkStatsList = runList,
#'  sampleSize = 0.25,
#'  networkModes = c("car", "bike", "freight")
#'  )
#'
#'
#'@export
mergeCountsAndLinks <- function(counts, network, linkStats, sampleSize = 0.25, networkModes = c("car"), aggr = c(TRUE, FALSE), earliest = 0, latest = 86400){

  # TODO: Output path is not necesarry, one can write this with one line, DONE
  # TODO: I would rather operate on the dataframes and not on the files paths, this way dataframes could be reused0
  # TODO: maybe this function could select and filter the wanted modes and unify it so it does not need to be specified or hardcoded in the other functions

  if(!is.vector(linkStatsList)){
    message <- "linkStatsList needs to be a vector1"
    warning(message)

    return(NA)
  }

  if(!is.data.frame(counts)){
    message <- "counts needs to be a data frame!"
    warning(message)

    return(NA)
  }

  if(!is.list(network)){
    message <- "network needs to be a list!"
    warning(message)

    return(NA)
  }

  links <- network$links %>%
    select(id, type)

  join <- left_join(x = counts, y = links, by = c("loc_id" = "id"))
  rm(counts, links)

  for(frame in linkStats){
    join <- left_join(x = join, y = frame, by = c("loc_id" = "linkId"))
  }

  join.long <- join %>%
    pivot_longer(cols = starts_with("vol_"), names_to = "name", names_prefix = "vol_", values_to = "volume") %>%
    mutate(split = unlist(str_split(name, pattern = "_"))[1],
           src = unlist(str_split(name, pattern = "_"))[2]) %>%
    select(-name) %>%
    filter()

  join.long
}

#'Prepares Linkstats and counts for VIA-style scatter plot
#'
#' Takes a tibble from mergeCountsAndLinks and prepare data for a VIA-style
#' scatterplot for one or more runs.
#' Only DTV values for mode 'car' is processed.
#'
#' Pattern "highway." is removed from link type, also link type is factored and
#' convert tibble to long-format
#'
#'@param joinedFrame a tibble, created from function mergeCountsAndLinks
#'
#'@return a long-format tibble, ready to for plotting
#'
#'@export
processLinkStatsForScatterPlot <- function(joinedFrame){

  if(!is.data.frame(joinedFrame)){

    message <- "joinedFrame needs to be a data frame, created from method mergeCountsAndLinks!"
    warning(message)
    return(NA)
  }

  join.1 = joinedFrame %>%
    filter(!is.na(type)) %>%
    filter(vol_car_count_station > 0) %>%
    select(loc_id, vol_car_count_station, starts_with("vol_"),type) %>%
    mutate(type = str_remove(type, pattern = "highway."),
           type = factor(type, levels = c("motorway", "primary", "secondary", "tertiary", "residential", "unclassified", "motorway_link", "primary_link", "trunk_link"))) %>%
    select(-c(starts_with("vol_bike"), starts_with("vol_freight"))) %>%
    pivot_longer(cols = c(vol_car_v1.0_run039, vol_car_v1.2_run007), names_to = "runId", values_to = "vol_sim", names_prefix = "vol_car_")

  join.1
}

#' Categorize DTV and calculate DTV distribution
#'
#' Takes a tibble from mergeCountsAndLinks. DTV is categorized into bins. Finally
#' data is aggregated to calculate DTV distribution in each link type category,
#' excluding 'residential' and 'unclassified'
#' Data can be used to create multiple geom_col plots to visualize and compare
#' DTV distributions between count data and several MATSim runs
#'
#' @param joinedFrame A tibble from mergeCountsAndLinks
#'
#' @param from Lower limit for count bin
#'
#' @param to Upper limit for count bins
#'
#' @param by Size of each count bin
#'
#' @return A long-format tibble which contains share of DTV Categories
#'
#' @export
processLinkStatsDtvDistribution <- function(joinedFrame, from = 0, to = 40000, by = 5000){

  # TODO: hard-coded car and bike volumes

  if(!is.data.frame(joinedFrame)){

    message <- "joinedFrame needs to be a data frame, created from method mergeCountsAndLinks!"
    warning(message)
    return(NA)
  }

  if((to - from) %% by != 0){

    message <- "Parameter 'from', 'to' and 'by' must be valid according to: (to - from) %% by == 0"
    warning(message)
    return(NA)
  }

  #### Counts in each bin
  options(scipen=999)
  breaks = seq(from = from, to = to, by = by)
  labels = character()

  for(i in 1:length(breaks) - 1){

    label = paste0(breaks[i] / 1000, "k < ", breaks[i + 1] / 1000, "k")
    labels[i] = label
    rm(label)
  }

  join.1 = joinedFrame %>%
    filter(!is.na(type)) %>%
    filter(vol_car_count_station > 0) %>%
    select(loc_id, vol_car_count_station, starts_with("vol_"),type) %>%
    mutate(type = str_remove(type, pattern = "highway."),
           type = factor(type, levels = c("motorway", "primary", "secondary", "tertiary", "residential", "unclassified", "motorway_link", "primary_link", "trunk_link"))) %>%
    pivot_longer(cols = starts_with("vol_"), names_to = "src", names_prefix = "vol_", values_to = "traffic_vol") %>%
    mutate(src = str_remove(src, pattern = "_vol"),
           traffic_bin = cut(traffic_vol, labels = labels, breaks = breaks, right = T)) %>%
    group_by(type, src, traffic_bin) %>%
    summarise(n = n()) %>%
    group_by(type, src) %>%
    mutate(share = n / sum(n)) %>%
    filter(!str_detect(type, pattern = "_link")) %>%
    filter(!type %in% c("residential", "unclassified"))

  join.1
}

#' Categorize DTV deviation and aggregate data
#'
#' Takes a tibble from mergeCountsAndLinks.
#' Deviation between count volumes and Linkstats is calculated
#' (e.g. deviation of 1.2 means 20 percent more DTV in MATSim than in counts) and
#' categorized.
#' If parameter 'aggr' is set to TRUE, data will be aggregated for each run and link type.
#' Can be used to visualize model quality by link type and to compare several runs.
#'
#' Estimation quality is determinated by the 'cut' function, limits for the label
#' 'exact' can be adjusted by tuning the parameter 'll' and 'ul'
#'
#' @param joinedFrame A tibble from mergeCountsAndLinks
#' @param aggr Boolean, if categorized data should returned aggregated
#' @param ll Double, lower limit for the quality label 'exact' default = 0.8*x - 200
#' @param ul Double, upper limit for the quality label 'exact', default = 1.2*x + 200
#'
#' @return A long-format tibble, which contains share of estimation quality for each scenario and link type, if aggr is FALSE disaggregated data is returned
#'
#' @export
processDtvEstimationQuality <- function(joinedFrame, aggr = c(T,F), ll = ~ .x *0.8 - 200, ul = ~ .x * 1.2 + 200){

  # TODO: changed ul and ll to be function, but other code needs to be adapted
  # TODO: hard-coded car and bike types

  join.1 = joinedFrame %>%
    filter(!is.na(type)) %>%
    filter(vol_car_count_station > 0) %>%
    select(loc_id, vol_car_count_station, starts_with("vol_car_"),type) %>%
    mutate(type = str_remove(type, pattern = "highway."),
           type = factor(type, levels = c("motorway", "primary", "secondary", "tertiary", "residential", "unclassified", "motorway_link", "primary_link", "trunk_link"))) %>%
    select(-starts_with("vol_bike"))

  names <- colnames(join.1)
  cs_col <- names[str_detect(names, "count_station")][1]
  names = names[str_detect(names, pattern = "vol_car")]
  names = names[!str_detect(names, pattern = "count_station")]

  for(n in names){

    n_fixed = str_remove(n, pattern = "vol_car_")
    join.1[paste0("rel_vol_", n_fixed)] = join.1[n] / join.1[cs_col]
  }

  pv_longer_cols <- colnames(join.1)
  pv_longer_cols = pv_longer_cols[str_detect(pv_longer_cols, pattern = "rel_vol")]

  est.breaks = c(-Inf, ul, ll, Inf)
  est.labels = c("less", "exact", "more")

  join.2 <- join.1 %>%
    select(- starts_with("vol_car_")) %>%
    pivot_longer(cols = pv_longer_cols, names_prefix = "rel_vol_", names_to = "src", values_to = "rel_vol") %>%
    mutate(quality = cut(rel_vol, breaks = est.breaks, labels = est.labels)) %>%
    filter(! type %in% c("residential", "unclassified")) %>%
    mutate(rel_vol_round = round(rel_vol, 2),
           estimation = cut(rel_vol_round, breaks = est.breaks, labels = est.labels))

  if(aggr){
    join.2.1 <- join.2 %>%
      group_by(src, type, estimation) %>%
      summarise(n = n()) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()

    return(join.2.1)
  }

  join.2
}
