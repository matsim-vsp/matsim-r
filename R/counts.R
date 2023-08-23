#'@import tidyverse
#' @rawNamespace import(scales, except = c(discard, col_factor))
#'@import xml2
#'@import geomtextpath
#'@import readr

##### Deprecated Functions ####

#'@title Deprecated Function
#'
#'@description \strong{readCounts} - Loads a MATSim Counts XML-file as tibble into memory
#'
#' @rdname matsimr-deprecated
#'
#'@param file File to load. Must be an .xml file
#'
#'@return \strong{readCounts} - tibble with MATSim link id ("loc_id") as key
#'
#'@export
readCounts <- function(file){

  .Deprecated("read_counts")

  message = paste("Read counts file from", file)
  print(message)

  counts.xml <- read_xml(file)

  children <- xml_children(counts.xml)

  result <- tibble("loc_id" = character(),
                   "cs_id" = character(),
                   "h" = numeric(),
                   "val" = numeric())

  for(c in children){

    station <- xml_attrs(c)

    volume <- xml_find_all(c, "volume") %>%
      xml_attrs() %>%
      purrr::map_df(~as.list(.)) %>%
      type_convert()

    length <- nrow(volume)

    loc_col <- rep(station["loc_id"], length) %>% unname()
    station_col <- rep(station["cs_id"], length) %>% unname()

    counts.frame <- data.frame("cs_id" = station_col,"loc_id" = loc_col) %>% bind_cols(volume)
    result = bind_rows(result, counts.frame)
  }

  result
}


#' Load a MATSim linkstats file into memory
#'
#' Loads a linkstats tsv file created from the LinkStats class
#' as a dataframe into memory.
#' Counts can be provided in any time bin.
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


#### Actual Function ####

#'@title Load a MATSim Counts file into memory
#'
#'@description Loads a MATSim Counts XML-file as tibble into memory
#'
#'
#'@param file File to load. Must be an .xml file
#'
#'@return tibble with MATSim link id ("loc_id") as key
#'
#'@export
read_counts <- function(file){

  message = paste("Read counts file from", file)
  print(message)

  counts.xml <- read_xml(file)

  children <- xml_children(counts.xml)

  result <- tibble("loc_id" = character(),
                   "cs_id" = character(),
                   "h" = numeric(),
                   "val" = numeric())

  for(c in children){

    station <- xml_attrs(c)

    volume <- xml_find_all(c, "volume") %>%
      xml_attrs() %>%
      purrr::map_df(~as.list(.)) %>%
      type_convert()

    length <- nrow(volume)

    loc_col <- rep(station["loc_id"], length) %>% unname()
    station_col <- rep(station["cs_id"], length) %>% unname()

    counts.frame <- data.frame("cs_id" = station_col,"loc_id" = loc_col) %>% bind_cols(volume)
    result = bind_rows(result, counts.frame)
  }

  result
}

#' Load a MATSim linkstats file into memory
#'
#' Loads a linkstats tsv file created from the LinkStats class
#' as a dataframe into memory.
#' Counts can be provided in any time bin.
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


#' Join counts and linkstats to the network, creating a tibble into memory
#'
#'Function to join counts, linkstats and network links. Data can be aggregated
#'and filtered by time or mode.
#'
#'
#'@param counts Tibble with counts data
#'
#'@param network Tibble with network nodes and links
#'
#'@param linkStats List with linkstats tibbles
#'
#'@param networkModes Vector with network modes that will be analyzed, default is "car".
#'
#'@param aggr_to Determines if data should be aggregated into hourly bins or as daily traffic volume, can either be "day" or "hour"
#'
#'@param earliest Integer. Lower limit to filter link stats by time, default = 0.
#'
#'@param latest Integer. Upper limit to filter link stats by time, default = 86400 (midnight).
#'
#'@return Long-format tibble with MATSim link id as key ("loc_id"), traffic volumes from MATSim runs and link type
#'
#'@export

mergeCountsAndLinks <- function(counts, network, linkStats, networkModes = c("car"), aggr_to = c("day", "hour"), earliest = 0, latest = 86400){

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

  if(aggr_to == "day"){

    counts = counts %>%
      group_by(loc_id) %>%
      summarise(cs_id = first(cs_id),
                h = first(h),
                val = sum(val)) %>%
      mutate(time = h)
  }

  print("Join counts and network links.")
  join <- left_join(x = counts, y = links, by = c("loc_id" = "id"))  %>%
    mutate(key = paste0(loc_id, "-", h))

  rm(links)

  print("Aggregate link stats.")
  for(i in 1:length(linkStats)){
    frame <- linkStats[[i]]

    time_value <- unique(frame$time)
    hours <- seq(0, 24 * 3600, 3600)

    ids <- counts %>% distinct(loc_id)

    if(aggr_to == "day"){

      sum_cols <- frame %>%
        select(starts_with("vol_")) %>%
        colnames()

      frame = left_join(ids, frame, by = c("loc_id" = "linkId")) %>%
        filter(time <= latest & time >= earliest) %>%
        group_by(loc_id) %>%
        summarise_at(sum_cols, sum)

      join = left_join(join, frame, by = "loc_id")
    }

    if(aggr_to == "hour"){

      hours <- seq(0, 24 * 3600, 3600)
      labels <- seq(1, 24, 1) %>% as.character()

      sum_cols <- frame %>%
        select(starts_with("vol_")) %>%
        colnames()

      frame.1 = left_join(ids, frame, by = c("loc_id" = "linkId")) %>%
        filter(time <= latest & time >= earliest) %>%
        mutate(hour = cut(time, labels = labels, breaks = hours, right = F)) %>%
        group_by(loc_id, hour) %>%
        summarise_at(sum_cols, sum, na.rm = T) %>%
        ungroup() %>%
        mutate(time = as.numeric(hour) * 3600,
               key = paste0(loc_id, "-", hour)) %>%
        select(-c(loc_id, hour))

      join = left_join(join, frame.1, by = "key")
    }
    rm(frame)
  }

  if("time.x" %in% colnames(join)){
    join = mutate(join, time = time.x)
  }

  if("key" %in% colnames(join)){
    join = join %>% select(-key)
  }

  print("Cleaning up ...")
  join.long <- join %>%
    mutate(count = val) %>%
    select(-c(ends_with(".x"), ends_with(".y"), val)) %>%
    pivot_longer(cols = starts_with("vol_"), names_to = "name", names_prefix = "vol_", values_to = "volume") %>%
    separate(col = name, into = c("mode", "src"), sep = "_") %>%
    mutate(type = str_remove(type, pattern = "highway."),
           type = factor(type, levels = c("motorway", "trunk", "primary", "secondary", "tertiary", "residential", "unclassified", "motorway_link", "primary_link", "trunk_link"))) %>%
    filter(mode %in% networkModes)

  print("Done!")
  join.long
}

#' Categorize daily traffic volume (DTV) and calculate DTV for different link types.
#'
#' Takes a tibble from mergeCountsAndLinks. DTV is categorized into bins. Finally
#' data is aggregated to calculate DTV distribution in each link type category,
#' excluding 'residential' and 'unclassified'
#' Data can be used to create multiple geom_col plots to visualize and compare
#' DTV distributions between count data and several MATSim runs
#'
#' @param joinedFrame A tibble from mergeCountsAndLinks
#'
#' @param from Integer. Lower limit for count bin, default = 0.
#'
#' @param to Integer. Upper limit for count bins, default = 40000.
#'
#' @param by Integer. Size of each count bin, default = 5000.
#'
#' @return A long-format tibble which contains share of DTV for link types
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
#' Deviation between count volumes and Linkstats is calculated and
#' categorized (i.e. deviation of 1.2 means 20 percent more DTV in MATSim than in counts).
#' If parameter 'aggr' is set to TRUE, data will be aggregated for each run and link type.
#' Can be used to visualize model quality by link type and to compare several runs.
#'
#' Estimation quality is determined by the 'cut' function, limits for the label
#' 'exact' can be adjusted by tuning the parameters 'll' (lower limit) and 'ul' (upper limit)
#'
#' @param joinedFrame A tibble from mergeCountsAndLinks
#' @param aggr Boolean, if categorized data should returned aggregated, default is TRUE.
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
                                      "exact")),
           estimation = factor(estimation, levels = c("less", "exact", "more")))

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

#' Creates a Via-Style scatterplot for each run
#'
#' Takes a tibble from mergeCountsAndLinks.
#' A scatterplot with counts on the x axis and MATSim dtv on the y axis is created and colored
#' by the road type.
#' Lower and upper limits define the section which is considered an 'exact' estimation. Limits
#' are defined by custom formulas.
#'
#' The function calls the matsim-r function processDtvEstimationQuality which is handling the limits.
#'
#' @param joinedFrame A tibble from mergeCountsAndLinks
#' @param ll Formula to calculate lower limit of the quality label 'exact', default = 0.8*x - 200
#' @param ul Formula to calculate lower limit of the quality label 'exact', default = 1.2*x + 200
#' @param threshold Threshold from which data is scaled to log10.
#'
#' @return A ggplot Scatterplotplot, which can be adjusted, if needed.
#'
#' @export
createCountScatterPlot <- function(joinedFrame, ll = ~ x * 0.8 - 200, ul = ~ x * 1.2 + 200, threshold = 100){

  line.size <- 0.7

  x <- seq(threshold, round(max(joinedFrame$count), -2), 10)

  middle.line <- data.frame(x = x,
                            y = x)

  ll.call <- ll[[length(ll)]]
  ul.call <- ul[[length(ul)]]

  x <- middle.line$x

  middle.line$ul <- eval(expr = ul.call)
  middle.line$ll <- eval(expr = ll.call)

  middle.line = middle.line %>%
    mutate(ll = ifelse(ll < 0, 0, ll))

  ggplot(joinedFrame, aes(x = count, y = volume, color = type)) +

    geom_point() +

    geom_line(data = middle.line, mapping = aes(x = x, y = ul), color = "black", size = line.size + 0.1) +

    geom_line(data = middle.line, mapping = aes(x = x, y = ll), color = "black", size = line.size + 0.1) +

    geom_line(data = middle.line, mapping = aes(x, y), size = line.size, linetype = "dashed", color = "grey60") +

    geom_vline(xintercept = threshold, linetype = "dashed") +

    geom_textvline(xintercept = threshold, label = "x = 100", linetype = "dashed", vjust = -0.3) +

    scale_x_continuous(trans = symlog_trans(thr = threshold, scale = 1000), breaks = c(0, 300, 1000, 3000, 10000, 30000, 100000)) +

    scale_y_continuous(trans = "log10", breaks = c(3, 10, 30, 100, 300, 1000, 3000, 10000, 30000)) +

    facet_wrap(.~ src) +

    labs(x = "Daily traffic volume from Count Stations", y = "Daily traffic volume from MATSim Data", color = "Road type:") +

    theme_bw() +

    theme(legend.position = "bottom", panel.background = element_rect(fill = "grey90"),
          panel.grid = element_line(colour = "white"))
}


#' A function to create symlog scaling for a plot
#'
#' Can be used to symlog scale the axis of a ggplot object. Is called in createCountScatterPlot.
#' Note that this function is taken from Stackoverflow!
#' For more informations, see the thread here:
#' https://stackoverflow.com/questions/14613355/how-to-get-something-like-matplotlibs-symlog-scale-in-ggplot-or-lattice
#'
#' @param base base for log
#' @param thr threshold from which data is scaled to log
#'
#' @export
symlog_trans <- function(base = 10, thr = 1, scale = 1){
  trans <- function(x)
    ifelse(abs(x) < thr, x, sign(x) *
             (thr + scale * suppressWarnings(log(sign(x) * x / thr, base))))

  inv <- function(x)
    ifelse(abs(x) < thr, x, sign(x) *
             base^((sign(x) * x - thr) / scale) * thr)

  breaks <- function(x){
    sgn <- sign(x[which.max(abs(x))])
    if(all(abs(x) < thr))
      pretty_breaks()(x)
    else if(prod(x) >= 0){
      if(min(abs(x)) < thr)
        sgn * unique(c(pretty_breaks()(c(min(abs(x)), thr)),
                       log_breaks(base)(c(max(abs(x)), thr))))
      else
        sgn * log_breaks(base)(sgn * x)
    } else {
      if(min(abs(x)) < thr)
        unique(c(sgn * log_breaks()(c(max(abs(x)), thr)),
                 pretty_breaks()(c(sgn * thr, x[which.min(abs(x))]))))
      else
        unique(c(-log_breaks(base)(c(thr, -x[1])),
                 pretty_breaks()(c(-thr, thr)),
                 log_breaks(base)(c(thr, x[2]))))
    }
  }
  trans_new(paste("symlog", thr, base, scale, sep = "-"), trans, inv, breaks)
}
