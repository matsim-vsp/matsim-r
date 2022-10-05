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
#'@return Tibble with link stats for each qsim mode
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
#'Function to join counts, network links and several matsim link stats. Data can be aggregated
#'and filtered by time or network mode.
#'
#'
#'@param counts Tibble with counts data
#'
#'@param network Tibble with network nodes and links
#'
#'@param linkStats List with link stats tibbles
#'
#'@param networkModes a vector with network modes, which are needed for analysis
#'
#'@param aggr Boolean, if data should be aggregated
#'
#'@param earliest Lower limit to filter link stats by time, default is 0
#'
#'@param latest Upper limit to filter link stats by time, default is 86400 (midnight)
#'
#'@return Long-format tibble with MATSim link id as key ("loc_id"), traffic volume from MATSim runs and link type
#'
#'
#'@export
mergeCountsAndLinks <- function(counts, network, linkStats, networkModes = c("car"), aggr = TRUE, earliest = 0, latest = 86400){

  if(!is.list(linkStats)){
    message <- "linkStatsList needs to be a list!"
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
  rm(links)

  for(i in 1:length(linkStats)){
    frame <- linkStats[[i]]

    if(i == 1){

      join = left_join(x = join, y = frame, by = c("loc_id" = "linkId"))
      join = join %>%
        mutate(key = paste0(loc_id, "-", time))

    } else {

      frame = frame %>%
        mutate(key = paste0(linkId, "-", time))
      join = left_join(x = join, y = frame, by = "key")
    }
    rm(frame)
  }

  join.long <- join %>%
    mutate(time = time.x,
           count = val) %>%
    select(-c(ends_with(".x"), ends_with(".y"), key, val)) %>%
    pivot_longer(cols = starts_with("vol_"), names_to = "name", names_prefix = "vol_", values_to = "volume") %>%
    separate(col = name, into = c("mode", "src"), sep = "_") %>%
    mutate(type = str_remove(type, pattern = "highway."),
           type = factor(type, levels = c("motorway", "primary", "secondary", "tertiary", "residential", "unclassified", "motorway_link", "primary_link", "trunk_link"))) %>%
    filter(time < latest & time > earliest) %>%
    filter(mode %in% networkModes)

  if(aggr){

    join.aggr <- join.long %>%
      group_by(loc_id, mode, src) %>%
      summarise(
        volume = sum(volume, na.rm = T),
        count = first(count),
        type = first(type)
      )

    return(join.aggr)
  }

  join.long
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

  wider_cols <- unique(joinedFrame$src)

  join.1 = joinedFrame %>%
    filter(!is.na(type)) %>%
    filter(count > 0) %>%
    select(loc_id, src, volume, type, count) %>%
    pivot_wider(names_from = src, values_from = volume) %>%
    pivot_longer(cols = c(wider_cols, "count"), names_to = "src", values_to = "volume") %>%
    mutate(traffic_bin = cut(volume, labels = labels, breaks = breaks, right = T)) %>%
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
#' @param ll Formula to calculate lower limit of the quality label 'exact', default = 0.8*x - 200
#' @param ul Formula to calculate lower limit of the quality label 'exact', default = 1.2*x + 200
#'
#' @return A long-format tibble, which contains share of estimation quality for each scenario and link type, if aggr is FALSE disaggregated data is returned
#'
#' @export
processDtvEstimationQuality <- function(joinedFrame, aggr = TRUE, ll =  ~ x *0.8 - 200, ul = ~ x * 1.2 + 200){

  ll.call <- ll[[length(ll)]]
  ul.call <- ul[[length(ul)]]

  x <- joinedFrame$count

  joinedFrame$ul <- eval(expr = ul.call)
  joinedFrame$ll <- eval(expr = ll.call)

  join.1 <- joinedFrame %>%
    mutate(ll = ifelse(ll < 0, 0, ll),
           estimation = ifelse(volume < ll, "less",
                            ifelse(volume > ul, "more",
                                   "exact"))) %>%
    select(-c(ul, ll))

  if(aggr){
    join.2 <- join.1 %>%
      group_by(src, type, estimation) %>%
      summarise(n = n()) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()

    return(join.2)
  }

  join.1
}
