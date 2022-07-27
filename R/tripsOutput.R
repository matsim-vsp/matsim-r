matsimDumpOutputDirectory <- "./matsim_r_output"
#' Load MATSIM output_trips table into Memory
#'
#' Loads a MATSim CSV output_trips from file or archive,
#' creating a tibble with columns as in csv file
#'
#'
#'
#'
#'
#' @param pathToMATSimOutputDirectory character string, path to matsim output directory or http link to the file.
#'
#' @return tibble of trips_output
#'
#' @export
readTripsTable <- function(pathToMATSimOutputDirectory = ".") {
  # Get the file names, output_trips should be there
  options(digits = 18)
  # Read from URL
  if (grepl("http", pathToMATSimOutputDirectory) == TRUE) {
    trips_output_table <- read_delim(pathToMATSimOutputDirectory,
      delim = ";",
      col_types = cols(
        start_x = col_character(),
        start_y = col_character(), end_x = col_character(),
        end_y = col_character(),
        end_link = col_character(),
        start_link = col_character()
      )
    )

    trips_output_table <- trips_output_table %>% mutate(
      start_x = as.double(start_x),
      start_y = as.double(start_y),
      end_x = as.double(end_x),
      end_y = as.double(end_y)
    )
    attr(trips_output_table,"table_name") <- pathToMATSimOutputDirectory
    return(trips_output_table)
  }
  if (grepl("output_trips.csv.gz$", pathToMATSimOutputDirectory) == TRUE) {
    trips_output_table <- read_csv2(pathToMATSimOutputDirectory,
      col_types = cols(
        start_x = col_character(),
        start_y = col_character(),
        end_x = col_character(),
        end_y = col_character(),
        end_link = col_character(),
        start_link = col_character()
      )
    )
    # person is mostly integer, but contains also chars(see Hamburg 110813 observation)
    # doesn't reads coordinates correctly
    trips_output_table <- trips_output_table %>% mutate(
      start_x = as.double(start_x),
      start_y = as.double(start_y),
      end_x = as.double(end_x),
      end_y = as.double(end_y)
    )
    attr(trips_output_table,"table_name") <- pathToMATSimOutputDirectory
    return(trips_output_table)
  }

  files <- list.files(pathToMATSimOutputDirectory, full.names = TRUE)
  # Read from global/local directory
  # output_trips is contained as output_trips.csv.gz
  if (length(grep("output_trips.csv.gz$", files)) != 0) {
    trips_output_table <- read_csv2(files[grep("output_trips.csv.gz$", files)],
      col_types = cols(
        start_x = col_character(),
        start_y = col_character(),
        end_x = col_character(),
        end_y = col_character(),
        end_link = col_character(),
        start_link = col_character()
      )
    )
    # person is mostly integer, but contains also chars(see Hamburg 110813 observation)
    # doesn't reads coordinates correctly
    trips_output_table <- trips_output_table %>% mutate(
      start_x = as.double(start_x),
      start_y = as.double(start_y),
      end_x = as.double(end_x),
      end_y = as.double(end_y)
    )
    attr(trips_output_table,"table_name") <- files[grep("output_trips.csv.gz$", files)]
    return(trips_output_table)
  } else { # if Directory doesn't contain trips_output, then nothing to read
    return(NULL)
  }
}

#' Plot main_mode as a Pie Chart
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot pie chart with with values that represent
#' percentage of using transport modes from trips
#'
#' Function automatically detects transport_modes from table
#' and plots pie chart.
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Pie Chart plot of transport mode distribution, values given in percents
#'
#' @export
plotModalSplitPieChart <- function(tripsTable, unite.columns = character(0), united.name = "united", dump.output.to = matsimDumpOutputDirectory) {

  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }

  # tripsTableCount gives percentage representation out
  tripsTableCount <- tripsTable %>%
    count(main_mode) %>%
    mutate(n = n / sum(n) * 100)

  # getthe positions
  positions <- tripsTableCount %>%
    mutate(
      csum = rev(cumsum(rev(n))),
      pos = n / 2 + lead(csum, 1),
      pos = if_else(is.na(pos), n / 2, pos)
    )

  # plotting

  plt <- ggplot(tripsTableCount, aes(x = "", y = n, fill = main_mode)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_label_repel(
      data = positions,
      aes(y = pos, label = paste0(round(n, digits = 1), "%")),
      size = 4.5, nudge_x = 1, show.legend = FALSE
    ) +
    ggtitle("Distribution of transport type") +
    theme_void()
  plt
  if (file.exists(dump.output.to)) {
    ggsave(paste0(dump.output.to, "/modalSplitPieChart.png"),width = 6,height = 10, plt)
  } else {
    dir.create(dump.output.to)
    ggsave(paste0(dump.output.to, "/modalSplitPieChart.png"),width = 6,height = 10, plt)
  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write_file(paste(tripsTableCount$main_mode, collapse = "\t"), paste0(dump.output.to, "/modalSplitPieChart.txt"), append = FALSE)
    write_file(paste("\r\n", paste(tripsTableCount$n, collapse = "\t")), paste0(dump.output.to, "/modalSplitPieChart.txt"), append = TRUE)
    # write.csv2(tripsTableCount,paste0(matsimDumpOutputDirectory,"/modalSplitPieChart.csv"))
  } else {
    dir.create(dump.output.to)
    write_file(paste(tripsTableCount$main_mode, collapse = "\t"), paste0(dump.output.to, "/modalSplitPieChart.txt"), append = FALSE)
    write_file(paste("\r\n", paste(tripsTableCount$n, collapse = "\t")), paste0(dump.output.to, "/modalSplitPieChart.txt"), append = TRUE)
    # write.csv2(tripsTableCount,paste0(matsimDumpOutputDirectory,"/modalSplitPieChart.csv"))
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title = paste0("Modal Split Pie Chart from",attr(tripsTable,"table_name")),
      description = "generated by plotModalSplitPieChart()",
      type = "pie",
      width = 1,
      props = list(dataset = "modalSplitPieChart.txt", useLastRow = "true")
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title = paste0("Modal Split Pie Chart from",attr(tripsTable,"table_name")),
      description = "generated by plotModalSplitPieChart()",
      type = "pie",
      width = 1,
      props = list(dataset = "modalSplitPieChart.txt", useLastRow = "true")
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  (plt)
  return(plt)
}

#' Plot main_mode as a bar Chart
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot bar chart with with values that represent
#' percentage of using transport modes from trips
#'
#' Function automatically detects transport_modes from table
#' and plots pie chart with percentage of distribution.
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of transport mode distribution, values given in percents
#'
#' @export
plotModalSplitBarChart <- function(tripsTable, unite.columns = character(0), united.name = "united",dump.output.to = matsimDumpOutputDirectory) {

  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }
  # Get percentage
  tripsTableCount <- tripsTable %>%
    count(main_mode) %>%
    arrange(desc(n))
  # plotting
  text_for_y = tripsTableCount$n
  fig = plot_ly(data = tripsTableCount,x = ~main_mode,y = ~n,type = "bar",
               text = text_for_y,
               textposition = "auto",
               name = "Distribution of transport type")
  fig = fig %>% layout(yaxis = list(title = "Count"),barmode = "group")
  fig

  if (file.exists(dump.output.to)) {
    #ggsave(paste0(dump.output.to, "/modalSplitBarChart.png"),width = 6,height = 10, fig)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/modalSplitBarChart.html"))
  } else {
    dir.create(dump.output.to)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/modalSplitBarChart.html"))
    #ggsave(paste0(dump.output.to, "/modalSplitBarChart.png"),width = 6,height = 10, fig)
  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.table(tripsTableCount,paste0(dump.output.to,"/modalSplitBarChart.csv"),row.names = FALSE,sep = ",")
  } else {
    dir.create(dump.output.to)
    #write_file(paste(tripsTableCount$main_mode, collapse = "\t"), paste0(dump.output.to, "/modalSplitBarChart.txt"), append = FALSE)
    #write_file(paste("\r\n", paste(tripsTableCount$n, collapse = "\t")), paste0(dump.output.to, "/modalSplitBarChart.txt"), append = TRUE)
    write.table(tripsTableCount,paste0(dump.output.to,"/modalSplitBarChart.csv"),row.names = FALSE,sep = ",")
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  paste0("Modal Split Bar Chart from ",attr(tripsTable,"table_name")),
      description = "generated by plotModalSplitBarChart()",
      type = "bar",
      width = 1,
      props = list(dataset = "modalSplitBarChart.csv",
                   x = "main_mode",
                   y = "n",
                   yAxisTitle = "Count of trips",
                   xAxisTitle = "Main mode of trip")
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title = paste0("Modal Split Bar Chart from ",attr(tripsTable,"table_name")),
      description = "generated by plotModalSplitBarChart()",
      type = "bar",
      width = 1,
      props = list(dataset = "modalSplitBarChart.csv",
                   x = "main_mode",
                   y = "n",
                   yAxisTitle = "Count of trips",
                   xAxisTitle = "Main mode of trip")
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  #plotly::ggplotly(plt)
  return(fig)
}

#' Bar Chart with main_mode on x-axis and average travel/wait time on y-axis
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot bar chart with with values that represent
#' time spent on traveling/waiting
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of average time spent on travel/wait
#'
#' @export
plotAverageTravelWait <- function(tripsTable, unite.columns = character(0), united.name = "united",dump.output.to = matsimDumpOutputDirectory) {

  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }

  avg_time = tripsTable %>% group_by(main_mode)%>%
    summarize(trav_time_avg = hms::hms(seconds_to_period(mean(trav_time))),
                                       wait_time_avg = hms::hms(seconds_to_period(mean(wait_time)))) %>%
    mutate(trav_time_avg = minute(trav_time_avg),wait_time_avg = minute(wait_time_avg))


  fig = plot_ly(data = avg_time,x = ~main_mode,y = ~trav_time_avg,type = 'bar',name = "AVG Time Travelling")
  fig = fig %>% add_trace(y = ~wait_time_avg,name = "AVG Time Waiting")
  fig = fig %>% layout(yaxis = list(title = "Time spent (in minutes)"),barmode = "group")
  fig

  #files
  if (file.exists(dump.output.to)) {
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/averageTravelWait.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  } else {
    dir.create(dump.output.to)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/averageTravelWait.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.table(avg_time,paste0(dump.output.to,"/averageTravelWait.csv"),row.names = FALSE,sep = ",")
  } else {
    dir.create(dump.output.to)
    write.table(avg_time,paste0(dump.output.to,"/averageTravelWait.csv"),row.names = FALSE,sep = ",")
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  paste0("Average Travel/Wait Time Chart from ",attr(tripsTable,"table_name")),
      description = "generated by plotAverageTravelWait()",
      type = "bar",
      width = 1,
      props = list(dataset = "averageTravelWait.csv",
                   x = "main_mode",
                   y = "[trav_time_avg,wait_time_avg]",
                   yAxisTitle = "Time in minutes",
                   xAxisTitle = "main_mode")
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title = paste0("Average Travel/Wait Time Chart from ",attr(tripsTable,"table_name")),
      description = "generated by plotAverageTravelWait()",
      type = "bar",
      width = 1,
      props = list(dataset = "averageTravelWait.csv",
                   x = "main_mode",
                   y = "[trav_time_avg,wait_time_avg]",
                   yAxisTitle = "Time in minutes",
                   xAxisTitle = "main_mode")
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  return(fig)
}

#' Bar Chart with distance travelled on x-axis and number of trips on y-axis
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot bar chart with with values that represent
#' number of trips ~ distance travelled
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of count of trips among distance travelled
#'
#' @export
plotTripsByDistance <- function(tripsTable, unite.columns = character(0), united.name = "united",dump.output.to = matsimDumpOutputDirectory) {


  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }
  modes = levels(factor(tripsTable$main_mode))

  #This is a very bad way to do that, but I see no other way to get it done
  #Also filtering table into a new doesn't creates new objects in memory, so it works fast
  tripsTable_05km = tripsTable %>% filter(traveled_distance<=1000) %>% mutate(dist_cat = "0-1km")
  #tripsTable_1km = tripsTable %>% filter(traveled_distance>500 & traveled_distance<=1000  ) %>% mutate(dist_cat = "0.5-1km")
  tripsTable_2km = tripsTable %>% filter(traveled_distance>1000 & traveled_distance<=2000) %>% mutate(dist_cat = "1-2km")
  tripsTable_5km = tripsTable %>% filter(traveled_distance>2000 & traveled_distance<=5000) %>% mutate(dist_cat = "2-5km")
  tripsTable_10km = tripsTable %>% filter(traveled_distance>5000 & traveled_distance<=10*1000) %>% mutate(dist_cat = "5-10km")
  tripsTable_20km = tripsTable %>% filter(traveled_distance>10*1000 & traveled_distance<=20*1000) %>% mutate(dist_cat = "10-20km")
  tripsTable_50km = tripsTable %>% filter(traveled_distance>20*1000 & traveled_distance<=50*1000) %>% mutate(dist_cat = "20-50km")
  tripsTable_100km = tripsTable %>% filter(traveled_distance>50*1000 & traveled_distance<=100*1000) %>% mutate(dist_cat = "50-100km")
  tripsTable_100pluskm = tripsTable %>% filter(traveled_distance>100*1000) %>% mutate(dist_cat = "> 100km")

  tripsTable_result = rbind(tripsTable_05km,
                            #tripsTable_1km,
                            tripsTable_2km,
                            tripsTable_5km,
                            tripsTable_10km,
                            tripsTable_20km,
                            tripsTable_50km,
                            tripsTable_100km,
                            tripsTable_100pluskm)


  tableWithCounts = tripsTable_result %>% count(main_mode,dist_cat)
  tableToWrite = tripsTable_result %>% select(dist_cat) %>% unique() %>% arrange(dist_cat)
  for( mode in modes){
    newColumn = tableWithCounts %>%
      filter(main_mode == mode) %>%
      mutate(mode = n) %>%
      select(dist_cat,mode)
    diff = setdiff(tableToWrite$dist_cat,newColumn$dist_cat)
    for(dist in diff){
      newColumn = rbind(newColumn,c(dist,0))
    }
    newColumn = newColumn %>%arrange(dist_cat) %>% select(-dist_cat) %>% mutate(mode = as.numeric(mode))
    colnames(newColumn)[1] = mode
    #print(newColumn)
    tableToWrite = cbind(tableToWrite,newColumn)
  }
  tableToWrite$dist_cat = factor(tableToWrite$dist_cat,ordered = TRUE,
                                 levels = c("0-1km","1-2km","2-5km","5-10km","10-20km","20-50km","50-100km"))
  tableToWrite = tableToWrite %>% arrange(dist_cat)
  plt = ggplot(tripsTable_result) +
    geom_bar(aes(x = dist_cat,fill = main_mode),position = position_dodge())+
    ggtitle("Number of trips per travelling distance")



  fig = plotly::ggplotly(plt)
  fig

  #files
  if (file.exists(dump.output.to)) {
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/tripsPerDistance.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  } else {
    dir.create(dump.output.to)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/tripsPerDistance.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.table(tableToWrite,paste0(dump.output.to,"/tripsPerDistance.csv"),row.names = FALSE,sep = ",")
  } else {
    dir.create(dump.output.to)
    write.table(tableToWrite,paste0(dump.output.to,"/tripsPerDistance.csv"),row.names = FALSE,sep = ",")
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  paste0("Count of trips travelled per distance from ",attr(tripsTable,"table_name")),
      description = "generated by plotTripsByDistance()",
      type = "bar",
      width = 1,
      props = list(dataset = "tripsPerDistance.csv",
                   x = "dist_cat",
                   y = "[main_mode,n]",
                   yAxisTitle = "Count of trips",
                   xAxisTitle = "Distance")
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title =  paste0("Count of trips travelled per distance from ",attr(tripsTable,"table_name")),
      description = "generated by plotTripsByDistance()",
      type = "bar",
      width = 1,
      props = list(dataset = "tripsPerDistance.csv",
                   x = "dist_cat",
                   y = "[main_mode,n]",
                   yAxisTitle = "Count of trips",
                   xAxisTitle = "Distance")
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  return(fig)
}

#' Bar Chart with distance travelled on x-axis and number of trips on y-axis
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot bar chart with with values that represent
#' average distance traveled ~ main mode used
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of distance traveled per mode
#'
#' @export
plotTripDistanceByMode <- function(tripsTable, unite.columns = character(0), united.name = "united",dump.output.to = matsimDumpOutputDirectory) {


  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }
  modes = levels(factor(tripsTable$main_mode))

  table_name = (attr(tripsTable,"table_name"))
  tripsTable = tripsTable %>%
    group_by(main_mode) %>%
    summarize(avg_dist = mean(traveled_distance)/1000)
  attr(tripsTable,"table_name") = table_name
  text_for_y = round(tripsTable$avg_dist,digits = 2)

  fig = plot_ly(data = tripsTable,x = ~main_mode,y = ~avg_dist,
                type = 'bar',
                text = text_for_y,
                textposition = "auto",
                name = "AVG Distance traveled by a person over a day")
  fig = fig %>% layout(yaxis = list(title = "Distance (kms)"),barmode = "group")
  fig

  #files
  if (file.exists(dump.output.to)) {
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/tripsPerDistanceByMode.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  } else {
    dir.create(dump.output.to)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/tripsPerDistanceByMode.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.table(tripsTable,paste0(dump.output.to,"/tripDistancePerMode.csv"),row.names = FALSE,sep = ",")
  } else {
    dir.create(dump.output.to)
    write.table(tripsTable,paste0(dump.output.to,"/tripDistancePerMode.csv"),row.names = FALSE,sep = ",")
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  paste0("Average distance traveled by person per mode from ",attr(tripsTable,"table_name")),
      description = "generated by plotTripDistanceByMode()",
      type = "bar",
      width = 1,
      props = list(dataset = "tripDistancePerMode.csv",
                   x = "main_mode",
                   y = "[avg_dist]",
                   yAxisTitle = "Distance (kms)",
                   xAxisTitle = "Main_mode")
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title =  paste0("Average distance traveled by person per mode from ",attr(tripsTable,"table_name")),
      description = "generated by plotTripDistanceByMode()",
      type = "bar",
      width = 1,
      props = list(dataset = "tripDistancePerMode.csv",
                   x = "main_mode",
                   y = "[avg_dist]",
                   yAxisTitle = "Distance (kms)",
                   xAxisTitle = "Main_mode")
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  return(fig)
}



#' Scatter plot with departure time x-axis and number of trips on y-axis
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot scatter plot with with values that represent
#' count of trips for a specific departure time by main_mode
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of distance traveled per mode
#'
#' @export
plotTripCountByDepTime <- function(tripsTable, unite.columns = character(0), united.name = "united",dump.output.to = matsimDumpOutputDirectory) {


  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }
  modes = levels(factor(tripsTable$main_mode))



  table_name = (attr(tripsTable,"table_name"))

  tripsTable = tripsTable %>%
    mutate(dep_time = hour(dep_time)) %>%
    count(dep_time,main_mode)

  attr(tripsTable,"table_name") = table_name          #reset table_name
  #text_for_y = round(tripsTable$avg_dist,digits = 2)


  tableToWrite = tripsTable %>% select(dep_time) %>% unique() %>% arrange(dep_time)
  for( mode in modes){
    newColumn = tripsTable %>%
      filter(main_mode == mode) %>%
      mutate(mode = n) %>%
      select(dep_time,mode)
    #diff = setdiff(tableToWrite$dist_cat,newColumn$dist_cat)
    #for(dist in diff){
      #newColumn = rbind(newColumn,c(dist,0))
    #}
    newColumn = newColumn %>%arrange(dep_time) %>% select(-dep_time) %>% mutate(mode = as.numeric(mode))
    colnames(newColumn)[1] = mode
    #print(newColumn)
    tableToWrite = cbind(tableToWrite,newColumn)
  }
  #print(tableToWrite)
  fig = plot_ly(tripsTable,x = ~dep_time,y = ~n,type = "scatter",mode = "line",linetype = ~main_mode)
  fig = fig %>% layout(yaxis = list(title = "Count of trips per departure Time"),barmode = "group")
  fig

  #files
  if (file.exists(dump.output.to)) {
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/countTripsByDep.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  } else {
    dir.create(dump.output.to)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/countTripsByDep.html"))
    #ggsave(paste0(dump.output.to, "/averageTravelWait.png"),width = 6,height = 10, fig)
  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.table(tableToWrite,paste0(dump.output.to,"/countTripsByDep.csv"),row.names = FALSE,sep = ",")
  } else {
    dir.create(dump.output.to)
    write.table(tableToWrite,paste0(dump.output.to,"/countTripsByDep.csv"),row.names = FALSE,sep = ",")
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  paste0("Trips counted by departure time from ",attr(tripsTable,"table_name")),
      description = "generated by plotTripCountByDepTime()",
      type = "line",
      width = 1,
      props = list(dataset = "countTripsByDep.csv",
                   x = "dep_time",
                   y = "[n]",
                   yAxisTitle = "Count of trips",
                   xAxisTitle = "departure time")
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title =  paste0("Trips counted by departure time from ",attr(tripsTable,"table_name")),
      description = "generated by plotTripCountByDepTime()",
      type = "line",
      width = 1,
      props = list(dataset = "countTripsByDep.csv",
                   x = "dep_time",
                   y = "[n]",
                   yAxisTitle = "Count of trips",
                   xAxisTitle = "departure time")
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  return(fig)
}

#' Scatter plot with departure time x-axis and number start activities on y-axis
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot scatter plot with with values that represent
#' count of start activities for a specific departure time by main_mode
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of distance traveled per mode
#'
#' @export
plotStartActCountByDepTime <- function(tripsTable, unite.columns = character(0), united.name = "united",dump.output.to = matsimDumpOutputDirectory) {


  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }


  table_name = (attr(tripsTable,"table_name"))

  tripsTable = tripsTable %>%
    mutate(dep_time = hour(dep_time),start_activity_type = sapply(strsplit(start_activity_type,"_"),"[[",1)) %>%
    count(dep_time,start_activity_type)

  attr(tripsTable,"table_name") = table_name          #reset table_name
  #text_for_y = round(tripsTable$avg_dist,digits = 2)

  activities = levels(factor(tripsTable$start_activity_type))

  tableToWrite = tripsTable %>% select(dep_time) %>% unique() %>% arrange(dep_time)
  for( act in activities){
    newColumn = tripsTable %>%
      filter(start_activity_type == act) %>%
      mutate(act = n) %>%
      select(dep_time,act)
    diff = setdiff(tableToWrite$dep_time,newColumn$dep_time)
    for(dtime in diff){
      newColumn = rbind(newColumn,c(dtime,0))
    }
    newColumn = newColumn %>%arrange(dep_time) %>% select(-dep_time) %>% mutate(act = as.numeric(act))
    colnames(newColumn)[1] = act
    #print(newColumn)
    tableToWrite = cbind(tableToWrite,newColumn)
  }
  #print(tableToWrite)
  fig = plot_ly(tripsTable,x = ~dep_time,y = ~n,type = "scatter",mode = "line",linetype = ~start_activity_type)
  fig = fig %>% layout(yaxis = list(title = "Count of start activities per departure Time"),barmode = "group")
  fig

  #files
  if (file.exists(dump.output.to)) {
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/countTripsByDep.html"))
  } else {
    dir.create(dump.output.to)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/countTripsByDep.html"))

  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.table(tableToWrite,paste0(dump.output.to,"/countStartActByDep.csv"),row.names = FALSE,sep = ",")
  } else {
    dir.create(dump.output.to)
    write.table(tableToWrite,paste0(dump.output.to,"/countStartActByDep.csv"),row.names = FALSE,sep = ",")
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  paste0("Start activities counted by departure time from ",attr(tripsTable,"table_name")),
      description = "generated by plotStartActCountByDepTime()",
      type = "bar",
      width = 1,
      props = list(dataset = "countStartActByDep.csv",
                   x = "dep_time",
                   y = "[n]",
                   yAxisTitle = "Count of start activities",
                   xAxisTitle = "departure time",
                   stacked= TRUE)
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title =  paste0("Start activities counted by departure time from ",attr(tripsTable,"table_name")),
      description = "generated by plotStartActCountByDepTime()",
      type = "bar",
      width = 1,
      props = list(dataset = "countStartActByDep.csv",
                   x = "dep_time",
                   y = "[n]",
                   yAxisTitle = "Count of start activities",
                   xAxisTitle = "departure time",
                   stacked = TRUE)
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  return(fig)
}

#' Scatter plot with arrival time x-axis and number end activities on y-axis
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot scatter plot with with values that represent
#' count of end activities for a specific arrival time
#' Using parameters unite.columns, specific columns could be given, to unite them in 1 mode with the name united.name(by default 'united')
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character strings, that represent patterns of columns to be united, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name character string, if columns were united, you can specify name for the resulting column in chart
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of distance traveled per mode
#'
#' @export
plotEndActCountByArrTime <- function(tripsTable, unite.columns = character(0), united.name = "united",dump.output.to = matsimDumpOutputDirectory) {


  # If some columns should be united
  if (length(unite.columns) != 0) {
    tripsTable$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable$main_mode)] <- united.name
  }


  table_name = (attr(tripsTable,"table_name"))

  tripsTable = tripsTable %>%
    mutate(arr_time = hour(hms(tripsTable$dep_time)+hms(tripsTable$trav_time))) %>%
    mutate(end_activity_type = sapply(strsplit(end_activity_type,"_"),"[[",1)) %>%
    count(arr_time,end_activity_type)

  attr(tripsTable,"table_name") = table_name          #reset table_name
  #text_for_y = round(tripsTable$avg_dist,digits = 2)

  activities = levels(factor(tripsTable$end_activity_type))

  tableToWrite = tripsTable %>% select(arr_time) %>% unique() %>% arrange(arr_time)
  for( act in activities){
    newColumn = tripsTable %>%
      filter(end_activity_type == act) %>%
      mutate(act = n) %>%
      select(arr_time,act)
    diff = setdiff(tableToWrite$arr_time,newColumn$arr_time)
    for(dtime in diff){
      newColumn = rbind(newColumn,c(dtime,0))
    }
    newColumn = newColumn %>%arrange(arr_time) %>% select(-arr_time) %>% mutate(act = as.numeric(act))
    colnames(newColumn)[1] = act
    #print(newColumn)
    tableToWrite = cbind(tableToWrite,newColumn)
  }
  #print(tableToWrite)
  fig = plot_ly(tripsTable,x = ~arr_time,y = ~n,type = "scatter",mode = "line",linetype = ~end_activity_type)
  fig = fig %>% layout(yaxis = list(title = "Count of end activities per arrival Time"),barmode = "group")
  fig

  #files
  if (file.exists(dump.output.to)) {
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/countEndActByArr.html"))
  } else {
    dir.create(dump.output.to)
    htmlwidgets::saveWidget(fig,paste0(dump.output.to, "/countEndActByArr.html"))

  }

  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.table(tableToWrite,paste0(dump.output.to,"/countEndActByArr.csv"),row.names = FALSE,sep = ",")
  } else {
    dir.create(dump.output.to)
    write.table(tableToWrite,paste0(dump.output.to,"/countEndActByArr.csv"),row.names = FALSE,sep = ",")
  }

  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  paste0("End activities counted by arrival time from ",attr(tripsTable,"table_name")),
      description = "generated by plotEndActCountByArrTime()",
      type = "bar",
      width = 1,
      props = list(dataset = "countEndActByArr.csv",
                   x = "arr_time",
                   y = "[n]",
                   yAxisTitle = "Count of end activities",
                   xAxisTitle = "arrival time",
                   stacked = TRUE)
    ))
  )

  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title =  paste0("End activities counted by arrival time from ",attr(tripsTable,"table_name")),
      description = "generated by plotEndActCountByArrTime()",
      type = "bar",
      width = 1,
      props = list(dataset = "countEndActByArr.csv",
                   x = "arr_time",
                   y = "[n]",
                   yAxisTitle = "Count of end activities",
                   xAxisTitle = "arrival time",
                   stacked = TRUE)
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }
  return(fig)
}


#' Bar Chart with tripType on x-axis and travelled distance on y-axis
#'
#' Takes Table trips_output (from readTripsTable()),
#' to plot bar chart with with values that represent
#' travelled distance of each tripType related to the shapeTable
#'
#'
#' @param tripsTable tible of trips_output (from readTripsTable())
#' @param shapeTable sf object(data.frame with geometries), can be received by using st_read(path_to_geographical_file)
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard. folder where png of plot is stored
#'
#' @return Bar Chart plot of average time spent on travel/wait
#'
#' @export
plotDistanceTraveledByType <- function(tripsTable,shapeTable,crs,dump.output.to = matsimDumpOutputDirectory) {

  #Getting all the trip types
  filtered_inside = filterByRegion(berlin,shape,31468)
  filtered_outside = filterByRegion(berlin,shape,31468,start.inshape = FALSE,end.inshape = FALSE)
  filtered_origin = filterByRegion(berlin,shape,31468,start.inshape = TRUE,end.inshape = FALSE)
  filtered_destinating = filterByRegion(berlin,shape,31468,start.inshape = FALSE,end.inshape = TRUE)
  #Adding collumn representing type
  filtered_inside = filtered_inside %>% mutate(type = "inside")
  filtered_outside = filtered_outside %>% mutate(type = "outside")
  filtered_origin = filtered_origin %>% mutate(type = "origin")
  filtered_destinating = filtered_origin %>% mutate(type = "destinating")
  #Adding all the subtables
  result = rbind(filtered_inside,filtered_outside,filtered_destinating,filtered_origin)
  #For plotting
  travelled = result %>% group_by(type) %>% summarize(travelled = sum(traveled_distance)/1000)
  fig = plot_ly(data = travelled,x = ~type,y = ~travelled,type = 'bar',name = "Distance travelled by trip Type")
  fig = fig %>% layout(yaxis = list(title = "Distance travelled (in kms)"),barmode = "group")
  fig
  return(fig)
}



#' Plot alluvial/sankey diagram of transport mode changes
#'
#' Takes two trips_table (from readTripsTable), and collects
#' changes between transport mode distribution of these tables
#' to make alluvial diagram from this data
#'
#' Function calculates number of each transport mode used in
#' first and second table, and draws plot that represent how
#' distribution of transport mode has changed (f. e. what part of concrete trasport mode changed to another)
#' Using parameter unite.columns transport modes that match PATTERN in unite.columns can be united in 1 transport mode type (by default united.name is "united")
#' Using parameter show.onlyChanges
#'
#' @param tripsTable1 tible of trips_output (from readTripsTable())
#' @param tripsTable2 tible of trips_output (from readTripsTable())
#' @param show.onlyChanges boolean, if it is set to TRUE => sankey diagram only contains changes on axes
#' @param unite.columns vector of character string, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name if columns were united, you can specify name for the resulting column in plot
#' @param dump.output.to folder that saves and configures yaml for simwrapper. folder where png of plot is stored
#'
#' @return Alluvial diagram that represents changes in transport mode distribution of trip tables
#'
#' @export
plotModalShiftSankey <- function(tripsTable1, tripsTable2, show.onlyChanges = FALSE, unite.columns = character(0), united.name = "united", dump.output.to = matsimDumpOutputDirectory) {
  if (show.onlyChanges == TRUE) {
    joined <- as_tibble(inner_join(tripsTable1, tripsTable2 %>%
      select(trip_id, main_mode), by = "trip_id") %>%
      rename(base_mode = main_mode.x, policy_mode = main_mode.y))
    joined <- joined %>%
      filter(base_mode != policy_mode) %>%
      group_by(base_mode, policy_mode) %>%
      count()
  } else {
    joined <- as_tibble(inner_join(tripsTable1, tripsTable2 %>%
      select(trip_id, main_mode), by = "trip_id") %>% rename(base_mode = main_mode.x, policy_mode = main_mode.y))
    joined <- joined %>%
      group_by(base_mode, policy_mode) %>%
      count()
  }
  # If the unite.commercials flag is set to TRUE, then join all commercials under 1 name commercial
  if (length(unite.columns) != 0) {
    joined$base_mode[grep(paste0(unite.columns, collapse = "|"), joined$base_mode)] <- united.name
    joined$policy_mode[grep(paste0(unite.columns, collapse = "|"), joined$policy_mode)] <- united.name
  }
  #########################################################
  # Generating yaml and output_files
  if (file.exists(dump.output.to)) {
    write.csv2(joined, paste0(dump.output.to, "/modalShift.csv"), row.names = FALSE)
  } else {
    dir.create(dump.output.to)
    write.csv2(joined, paste0(dump.output.to, "/modalShift.csv"), row.names = FALSE)
  }

  yaml_list <- list(csv = "/modalShift.csv", title = "Modal Shift Sankey Diagram", description = "generated by plotModalShift")

  write_yaml(yaml_list, paste0(dump.output.to, "/sankey-modalshift.yaml"))



  ##########################################################

  plt = ggplot(joined, aes(y = n, axis1 = base_mode, axis2 = policy_mode)) +
    geom_alluvium(aes(fill = base_mode), width = 1 / 8, knot.pos = 0) +
    geom_stratum(width = 1 / 8, alpha = 0.25) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_x_discrete(limits = c("Base Mode", "Policy Mode"), expand = c(.05, .05))
  plt
  return(plt)
}

#' Plot bar chart diagram of transport mode changes
#'
#' Takes two trips_table (from readTripsTable), and collects
#' changes between transport mode distribution of these tables
#' to make bar chart diagram with dodging positioning from this data
#'
#' Function calculates number of each transport mode used in
#' first and second table, and draws plot that represent how
#' distribution of transport mode has changed (f. e. what part of concrete trasport mode changed to another)
#' Using parameter unite.columns transport modes that match PATTERN in unite.columns can be united in 1 transport mode type (by default united.name is "united")
#' Using parameter show.onlyChanges
#'
#' @param tripsTable1 tible of trips_output (from readTripsTable())
#' @param tripsTable2 tible of trips_output (from readTripsTable())
#' @param unite.columns vector of character string, changes name of all transport modes in the tibble copy to united.name = "united" that matches PATTERNS given in unite.columns
#' @param united.name if columns were united, you can specify name for the resulting column in plot
#' @param dump.output.to folder that saves and configures yaml for simwrapper. folder where png of plot is stored
#'
#' @return Alluvial diagram that represents changes in transport mode distribution of trip tables
#'
#' @export
plotModalShiftBar <- function(tripsTable1, tripsTable2, unite.columns = character(0), united.name = "united", dump.output.to = matsimDumpOutputDirectory) {
  # If the unite.columns is specified, then
  if (length(unite.columns) != 0) {
    tripsTable1$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable1$main_mode)] <- united.name
    tripsTable2$main_mode[grep(paste0(unite.columns, collapse = "|"), tripsTable2$main_mode)] <- united.name
  }
  tripsTable1  = tripsTable1 %>% mutate(type = "base")
  tripsTable2  = tripsTable2 %>% mutate(type = "policy")

  total_trips = rbind(tripsTable1,tripsTable2)
  ##########################################################

  plt = ggplot(total_trips, aes(x =main_mode,fill = factor(type)))+
    geom_bar(position = position_dodge())+
    coord_flip()
  plotly::ggplotly(plt)

  if (file.exists(dump.output.to)) {
    ggsave(paste0(dump.output.to, "/modalShiftBarChart.png"), plt)
  } else {
    dir.create(dump.output.to)
    ggsave(paste0(dump.output.to, "/modalShiftBarChart.png"), plt)
  }

  return(plotly::ggplotly(plt))
}

#' Transforms trips_table tibble (from readTripsTable) from tibble to sf (table with attribute features and geometry feature)
#'
#' Takes trips_table (from readTripsTable) and transforms trips_table to sf object using start_x, end_x, start_y, end_y as a geometry features
#' deletes from resulting data.frame start_x, end_x, start_y, end_y.
#' And adds wkt column, if geometry.type = st_mulitpoint(), or geometry.type = st_linestring()
#' Or adds start_wkt and end_wkt, if geometry.type = st_point()
#' Added column/columns projected to given CRS (coordinate reference system),
#' that can be taken from network file of MATSimOutputDirectory
#'
#' Function also sets attribute geometry.type to resulting table to character value of "POINT","MULTIPOINT","LINESTRING"
#' to get which type of table was generated, if it is needed
#'
#' @param table tibble of trips_output (from readTripsTable())
#'
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#'
#' @param geometry.type function of sf transformation, geometry.type can be (by default is st_multipoint())
#' !!!st_point()-resulting table contains 2 geometries start_wkt and end_wkt, representing start and end POINTs, and have type POINT!!!  or
#' !!!st_multipoint()-resulting table contains 1 geometry wkt, representing start and end POINTS as MULTIPOINT!!! or
#' !!!st_linestring() - resulting table contains 1 geometry wkt, representing line between start and end points as LINESTRING!!!
#'
#' @return sf object (data.frame with geometries depending to geometry.type)
#'
#' @export
transformToSf <- function(table, crs, geometry.type = st_multipoint()) {
  if (class(geometry.type)[2] == "POINT") {
    table1 <- table %>%
      # mutate(wkt = paste("MULTIPOINT(", start_x, " ", start_y, ",", end_x, " ", end_y, ")", sep =""))
      mutate(start_wkt = paste("POINT(", start_x, " ", start_y, ")", sep = ""))
    table2 <- table %>%
      mutate(end_wkt = paste("POINT(", end_x, " ", end_y, ")", sep = ""))
    attr(table, "geometry.type") <- "POINT"


    table1_wkt <- st_as_sf(table1, wkt = "start_wkt") %>% select(-start_x, -start_y, -end_x, -end_y)
    table2_wkt <- st_as_sf(table2, wkt = "end_wkt") %>% select(-start_x, -start_y, -end_x, -end_y)


    result_table <- table1_wkt %>% mutate(end_wkt = table2_wkt$end_wkt)
    st_geometry(result_table) <- "start_wkt"
    st_crs(result_table) <- crs
    st_geometry(result_table) <- "end_wkt"
    st_crs(result_table) <- crs
    st_geometry(result_table) <- "start_wkt"
    return(result_table)
  } else if (class(geometry.type)[2] == "MULTIPOINT") {
    table <- table %>%
      mutate(wkt = paste("MULTIPOINT(", start_x, " ", start_y, ",", end_x, " ", end_y, ")", sep = ""))
    attr(table, "geometry.type") <- "MULTIPOINT"


    result_table <- st_as_sf(table, wkt = "wkt") %>% select(-start_x, -start_y, -end_x, -end_y)

    st_crs(result_table) <- crs
    return(result_table)
  } else if (class(geometry.type)[2] == "LINESTRING") {
    table <- table %>%
      mutate(wkt = paste("LINESTRING(", start_x, " ", start_y, ",", end_x, " ", end_y, ")", sep = ""))
    attr(table, "geometry.type") <- "LINESTRING"


    result_table <- st_as_sf(table, wkt = "wkt") %>% select(-start_x, -start_y, -end_x, -end_y)

    st_crs(result_table) <- crs
    return(result_table)
  } else {
    return(NA)
  }
}

#' Filtering of trips_table(from readTripsTable) depending on how they located in given shape
#'
#' Takes trips_table and shapeTable(sf object from file representing geographical data, can be received by using function st_read(path_to_file).
#' Please be aware that this filterByRegion currently only works, when one geometry is loaded.)
#' transforms both objects to match mutual CRS(network.xml from MATSimOutputDirectory)
#' and filters the trips from table depending on *.inshape flags:
#' if start.inshape = TRUE & end.inshape = TRUE return table that contains trips inside given shape
#' if start.inshape = TRUE & end.inshape = FALSE return table that contains trips which starts in shape and ends out of the shape
#' if start.inshape = FALSE & end.inshape = TRUE return table that contains trips which ends in shape and starts out of the shape
#' if start.inshape = FALSE & end.inshape = FALSE return table that contains trips which starts and ends our of the given shape
#'
#' @param tripsTable tibble of trips_output (from readTripsTable())
#'
#' @param shapeTable sf object(data.frame with geometries), can be received by using st_read(path_to_geographical_file)
#'
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#'
#' @param start.inshape bool, defines trips to conclude (see Description)
#'
#' @param end.inshape bool, defines trips to conclude (see Description)
#'
#' @return tibble, with filtered trips depending on shapeTable and special flags (see Description)
#'
#' @export
filterByRegion <- function(tripsTable, shapeTable, crs, start.inshape = TRUE, end.inshape = TRUE) {

  # shapeTable <- st_read(shapeFile)
  if (st_crs(shapeTable) == NA) {
    st_crs(shapeTable) <- crs
  }

  sf_table <- transformToSf(tripsTable, crs = crs, geometry.type = st_point())
  shapeTable <- st_transform(shapeTable, crs = crs)
  # shapeTable isn't table - shape

  union_shape <- st_union(shapeTable) # transforms the crs back to the previous in the file
  union_shape <- st_transform(union_shape, crs = st_crs(shapeTable))


  st_geometry(sf_table) <- "start_wkt" # Set start_wkt as an active geometry
  cont1 <- st_contains(union_shape, sf_table)[[1]] # Indexes of rows where start point is in shapefile

  st_geometry(sf_table) <- "end_wkt" # Set end_wkt as and active geometry
  cont2 <- st_contains(union_shape, sf_table)[[1]] # Indexes of rows where end point is in shapefile

  # get trips that ended outside of shape
  cont_end_outside <- setdiff(1:nrow(sf_table), cont2)

  # get trips that started outside of shape
  cont_start_outside <- setdiff(1:nrow(sf_table), cont1)

  if (start.inshape == TRUE && end.inshape == TRUE) {
    cont_union <- intersect(cont1, cont2)
  } else if (start.inshape == TRUE && end.inshape == FALSE) {
    cont_union <- intersect(cont1, cont_end_outside)
  } else if (start.inshape == FALSE && end.inshape == TRUE) {
    cont_union <- intersect(cont2, cont_start_outside)
  } else {
    cont_union <- intersect(cont_start_outside, cont_end_outside) # Give back trips that are neither starting and ending outside the area
  }


  return(tripsTable[cont_union, ])
}

#' Plots result of filtered trips on the map (from shape)
#'
#' Takes trips_table and shapeTable(sf object from file representing geographical data, can be received by using function st_read(path_to_file))
#' transforms both objects to match mutual CRS(network.xml from MATSimOutputDirectory)
#' and filters the trips from table depending on *.inshape flags:
#' if start.inshape = TRUE & end.inshape = TRUE return table that contains trips inside given shape
#' if start.inshape = TRUE & end.inshape = FALSE return table that contains trips which starts in shape and ends out of the shape
#' if start.inshape = FALSE & end.inshape = TRUE return table that contains trips which ends in shape and starts out of the shape
#' if start.inshape = FALSE & end.inshape = FALSE return table that contains trips which starts and ends our of the given shape
#' result of filtering is plotted on map of shapeTable where green points are startpoints of trip and red points are endpoints of trip
#'
#' @param table tibble of trips_output (from readTripsTable())
#'
#' @param shapeTable sf object(data.frame with geometries), can be received by using st_read(path_to_geographical_file)
#'
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#'
#' @param start.inshape bool, defines trips to conclude (see Description)
#'
#' @param end.inshape bool, defines trips to conclude (see Description)
#'
#' @param optimized bool, by default FALSE and gives interactive plot using leaflet, if TRUE using image with ggplot
#'
#'
#' @return plot with trips filtered depending on flags *.inshape on map from shapeTable
#'
#' @export
plotMapWithTrips <- function(table, shapeTable, crs, start.inshape = TRUE, end.inshape = TRUE, optimized = FALSE) {
  # table = table[1:5000,] #To make plotting faster
  # table_sf = transformToSf(table,crs = crs)
  filtered <- filterByRegion(table, shapeTable, crs = crs, start.inshape, end.inshape)
  if (nrow(filtered) == 0) {
    ggplot() +
      geom_sf(data = shapeTable)
    warning("there is no trip filtered for this map")
  }
  filtered_sf <- transformToSf(filtered, crs = crs, geometry.type = st_point())
  st_geometry(filtered_sf) <- "start_wkt"
  filtered_sf_start <- filtered_sf %>% select(-end_wkt)
  st_geometry(filtered_sf) <- "end_wkt"
  filtered_sf_end <- filtered_sf %>% select(-start_wkt)
  # shape = st_read(shapePath)
  if (st_crs(shapeTable) == NA) {
    ct_crs(shapeTable) <- crs
  }
  shapeTable <- st_transform(shapeTable, crs = "+proj=longlat +datum=WGS84 +no_defs")
  filtered_sf_start <- st_transform(filtered_sf_start, "+proj=longlat +datum=WGS84 +no_defs")
  filtered_sf_end <- st_transform(filtered_sf_end, "+proj=longlat +datum=WGS84 +no_defs")

  if (optimized) {
    colors <- c("Start" = "blue", "End" = "red")
    shapes <- c("Start" = 5, "End" = 3)
    # ggplot2 isn't interactive!
    plt <- ggplot() +
      geom_sf(data = shapeTable) +
      # geom_sf(data = )
      geom_sf(data = filtered_sf_start, aes(color = "Start"), size = 1, shape = 5) +
      geom_sf(data = filtered_sf_end, aes(color = "End"), size = 1, shape = 3) +
      labs(color = "Type") +
      scale_colour_manual(values = colors)
    plt
    return(plt)
  }





  # If we need to change design
  # css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
  # Convert CSS to HTML
  # html_fix <- htmltools::tags$style(type = "text/css", css_fix)


  plt <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
    addProviderTiles(
      "Stamen.Toner",
      group = "Stamen.Toner"
    ) %>%
    addProviderTiles(
      "Stamen.Terrain",
      group = "Stamen.Terrain"
    ) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      group = "Esri.WorldStreetMap"
    ) %>%
    addProviderTiles(
      "Wikimedia",
      group = "Wikimedia"
    ) %>%
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Esri.WorldImagery"
    ) %>%
    addPolygons(data = shapeTable, opacity = 0.1, color = "green") %>%
    addCircleMarkers(filtered_sf_start,
      lng = st_coordinates(filtered_sf_start$start_wkt)[, 1],
      lat = st_coordinates(filtered_sf_start$start_wkt)[, 2], radius = 3, color = "blue",
      label = paste(
        "Person_id:",
        filtered_sf_start$person, "<br>",
        "Trip_id:",
        filtered_sf_start$trip_id, "<br>",
        "main_mode:", filtered_sf_start$main_mode, "<br>",
        "type:", "start", "<br>",
        "Start activity:",
        filtered_sf_start$start_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_end,
      lng = st_coordinates(filtered_sf_end$end_wkt)[, 1],
      lat = st_coordinates(filtered_sf_end$end_wkt)[, 2], radius = 0.15, color = "red",
      label = paste(
        "Person_id:",
        filtered_sf_end$person, "<br>",
        "Trip_id:",
        filtered_sf_end$trip_id, "<br>",
        "main_mode:", filtered_sf_end$main_mode, "<br>",
        "type:", "end", "<br>",
        "End activity:",
        filtered_sf_end$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addLegend(
      colors = c("blue", "red"),
      labels = c("Start of trip", "End of trip"),
      position = "bottomleft",
      title = "Type of the point",
      opacity = 0.9
    ) %>%
    addMiniMap() %>%
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap", "Stamen.Toner",
        "Stamen.Terrain", "Esri.WorldStreetMap",
        "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
      ),
      # position it on the topleft
      position = "topleft"
    )

  return(plt)
}

#' Plots distribution of every type of trips(inside, outside, origin and destinating) in Pie Chart
#'
#'
#'
#' @param table tibble of trips_output (from readTripsTable())
#'
#' @param shapeTable sf object(data.frame with geometries), can be received by using st_read(path_to_geographical_file)
#'
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#'
#'
#'
#' @return plot with percentage of each type of trips
#'
#' @export
plotTripTypesPieChart <- function(table, shapeTable, crs) {

  # table_sf = transformToSf(table,crs = crs)
  # Maybe union all this tables as 1 extended with additional column
  filtered_inside <- filterByRegion(table, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = TRUE)
  filtered_origin <- filterByRegion(table, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = FALSE)
  filtered_destination <- filterByRegion(table, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = TRUE)
  filtered_transit <- filterByRegion(table, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = FALSE)

  result <- tibble(
    n = c(nrow(filtered_inside), nrow(filtered_transit), nrow(filtered_origin), nrow(filtered_destination)),
    type = c("inside the shape", "outside the shape", "starting in shape", "ending in shape")
  )

  # result gives percentage representation out
  result <- result %>% mutate(n = n / sum(n) * 100)

  # getthe positions
  positions <- result %>%
    mutate(
      csum = rev(cumsum(rev(n))),
      pos = n / 2 + lead(csum, 1),
      pos = if_else(is.na(pos), n / 2, pos)
    )

  return(ggplot(result, aes(x = "", y = n, fill = fct_inorder(type))) +
    geom_col(width = 1, col = 1) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    # geom_text(aes(label = round(n,digits = 1)),
    # position=position_stack(vjust = 0.5),
    # show.legend = FALSE,size = 4)+
    geom_label_repel(
      data = positions,
      aes(y = pos, label = paste0(round(n, digits = 1), "%")),
      size = 4.5, nudge_x = 1, show.legend = FALSE
    ) +
    ggtitle("Distribution"))
}

#' Creates BarChart of changing trip types(originating,transit etc) between 2 tables
#' and saves output to dump.output.to
#'
#'
#'
#' @param tripsTable1 tibble of trips_output (from readTripsTable(),f.e. base case)
#'
#' @param tripsTable2 tibble of trips_output (from readTripsTable(),f.e. policy case)
#'
#' @param shapeTable sf object(data.frame with geometries), can be received by using st_read(path_to_geographical_file)
#'
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#'
#' @param dump.output.to folder that saves resulting image of BarChart
#'
#'
#' @return plot with percentage of each type of trips between 2 tables
#'
#' @export
compareTripTypesBarChart <- function(tripsTable1,tripsTable2,shapeTable,crs,dump.output.to = matsimDumpOutputDirectory){
  # table_sf = transformToSf(table,crs = crs)
  # Maybe union all this tables as 1 extended with additional column
  filtered_inside1 <- filterByRegion(tripsTable1, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = TRUE) %>%
    mutate(type = "inside",table_num = "1 table")
  filtered_origin1 <- filterByRegion(tripsTable1, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = FALSE) %>%
    mutate(type = "start_in_shape",table_num = "1 table")
  filtered_destination1 <- filterByRegion(tripsTable1, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = TRUE) %>%
    mutate(type = "endind_in_shape",table_num = "1 table")
  filtered_transit1 <- filterByRegion(tripsTable1, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = FALSE)%>%
    mutate(type = "outside",table_num = "1 table")

  filtered_inside2 <- filterByRegion(tripsTable2, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = TRUE) %>%
    mutate(type = "inside",table_num = "2 table")
  filtered_origin2 <- filterByRegion(tripsTable2, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = FALSE) %>%
    mutate(type = "start_in_shape",table_num = "2 table")
  filtered_destination2 <- filterByRegion(tripsTable2, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = TRUE) %>%
    mutate(type = "endind_in_shape",table_num = "2 table")
  filtered_transit2 <- filterByRegion(tripsTable2, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = FALSE)%>%
    mutate(type = "outside",table_num = "2 table")


  result_table = rbind(filtered_inside1,filtered_origin1,filtered_destination1,filtered_transit1,
                       filtered_inside2,filtered_origin2,filtered_destination2,filtered_transit2)

  #########################################################
  plt = ggplot(result_table, aes(x =type,fill = factor(table_num)))+
    geom_bar(position = position_dodge())+
    coord_flip()
  plotly::ggplotly(plt)

  if (file.exists(dump.output.to)) {
    ggsave(paste0(dump.output.to, "/tripTypeComparison.png"), plot = plt,width = 6,height = 10)
  } else {
    dir.create(dump.output.to)
    ggsave(paste0(dump.output.to, "/tripTypeComparison.png"), plot = plt,width = 6,height = 10)
  }

  return(plotly::ggplotly(plt))

}

#' Plots every type of trips(inside, outside, origin and destinating) on map
#'
#'
#'
#' @param table tibble of trips_output (from readTripsTable())
#'
#' @param shapeTable sf object(data.frame with geometries), can be received by using st_read(path_to_geographical_file)
#'
#' @param crs numeric of EPSG code or proj4string, can be found in network file from output directory of MATSim simulation
#'
#' @param optimized bool, by default FALSE and gives interactive plot using leaflet, if TRUE using image with ggplot
#'
#' @return plot that contains every trip with defined trip type
#'
#' @export
plotMapWithTripsType <- function(table, shapeTable, crs, optimized = FALSE) {
  # table = table[1:5000,] #To make plotting faster
  # table_sf = transformToSf(table,crs = crs)
  # Maybe union all this tables as 1 extended with additional column
  filtered_inside <- filterByRegion(table, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = TRUE)
  filtered_origin <- filterByRegion(table, shapeTable, crs = crs, start.inshape = TRUE, end.inshape = FALSE)
  filtered_destination <- filterByRegion(table, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = TRUE)
  filtered_transit <- filterByRegion(table, shapeTable, crs = crs, start.inshape = FALSE, end.inshape = FALSE)

  filtered_sf_inside <- transformToSf(filtered_inside, crs = crs, geometry.type = st_multipoint())
  filtered_sf_origin <- transformToSf(filtered_origin, crs = crs, geometry.type = st_multipoint())
  filtered_sf_destination <- transformToSf(filtered_destination, crs = crs, geometry.type = st_multipoint())
  filtered_sf_transit <- transformToSf(filtered_transit, crs = crs, geometry.type = st_multipoint())

  if (st_crs(shapeTable) == NA) {
    ct_crs(shapeTable) <- crs
  }

  shapeTable <- st_transform(shapeTable, crs = "+proj=longlat +datum=WGS84 +no_defs")
  filtered_sf_inside <- st_transform(filtered_sf_inside, "+proj=longlat +datum=WGS84 +no_defs")
  filtered_sf_origin <- st_transform(filtered_sf_origin, "+proj=longlat +datum=WGS84 +no_defs")
  filtered_sf_destination <- st_transform(filtered_sf_destination, "+proj=longlat +datum=WGS84 +no_defs")
  filtered_sf_transit <- st_transform(filtered_sf_transit, "+proj=longlat +datum=WGS84 +no_defs")

  if (optimized) {
    colors <- c("inside" = "green", "origin" = "red", "destination" = "orange", "transit" = "blue")
    shapes <- c("Start" = 5, "End" = 3)
    plt <- ggplot() +
      geom_sf(data = shapeTable) +
      # geom_sf(data = )
      geom_sf(data = filtered_sf_inside, aes(color = "inside"), size = 3, alpha = 0.5) +
      geom_sf(data = filtered_sf_origin, aes(color = "origin"), size = 3, alpha = 0.4) +
      geom_sf(data = filtered_sf_destination, aes(color = "destination"), size = 3, alpha = 0.3) +
      geom_sf(data = filtered_sf_transit, aes(color = "transit"), size = 2, alpha = 0.1) +
      labs(color = "Type") +
      scale_colour_manual(values = colors)
    plt
    return((plt))
  }

  # If we need to adjust design
  # css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
  # Convert CSS to HTML
  # html_fix <- htmltools::tags$style(type = "text/css", css_fix)


  plt <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
    addProviderTiles(
      "Stamen.Toner",
      group = "Stamen.Toner"
    ) %>%
    addProviderTiles(
      "Stamen.Terrain",
      group = "Stamen.Terrain"
    ) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      group = "Esri.WorldStreetMap"
    ) %>%
    addProviderTiles(
      "Wikimedia",
      group = "Wikimedia"
    ) %>%
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Esri.WorldImagery"
    ) %>%
    addPolygons(data = shapeTable, opacity = 0.1, color = "green") %>%
    addCircleMarkers(filtered_sf_inside,
      lng = st_coordinates(filtered_sf_inside$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_inside$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 2], radius = 3, color = "blue",
      label = paste(
        "Person_id:",
        filtered_sf_inside$person, "<br>",
        "Trip_id:",
        filtered_sf_inside$trip_id, "<br>",
        "main_mode:", filtered_sf_inside$main_mode, "<br>",
        "type:", "start", "<br>",
        "Start activity:",
        filtered_sf_inside$start_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_inside,
      lng = st_coordinates(filtered_sf_inside$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_inside$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 2], radius = 3, color = "blue",
      label = paste(
        "Person_id:",
        filtered_sf_inside$person, "<br>",
        "Trip_id:",
        filtered_sf_inside$trip_id, "<br>",
        "main_mode:", filtered_sf_inside$main_mode, "<br>",
        "type:", "end", "<br>",
        "End activity:",
        filtered_sf_inside$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_origin,
      lng = st_coordinates(filtered_sf_origin$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_origin$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 2], radius = 2, color = "red",
      label = paste(
        "Person_id:",
        filtered_sf_origin$person, "<br>",
        "Trip_id:",
        filtered_sf_origin$trip_id, "<br>",
        "main_mode:", filtered_sf_origin$main_mode, "<br>",
        "type:", "start", "<br>",
        "Start activity:",
        filtered_sf_origin$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_origin,
      lng = st_coordinates(filtered_sf_origin$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_origin$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 2], radius = 2, color = "red",
      label = paste(
        "Person_id:",
        filtered_sf_origin$person, "<br>",
        "Trip_id:",
        filtered_sf_origin$trip_id, "<br>",
        "main_mode:", filtered_sf_origin$main_mode, "<br>",
        "type:", "end", "<br>",
        "End activity:",
        filtered_sf_origin$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_destination,
      lng = st_coordinates(filtered_sf_destination$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_destination$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 2], radius = 1, color = "orange",
      label = paste(
        "Person_id:",
        filtered_sf_destination$person, "<br>",
        "Trip_id:",
        filtered_sf_destination$trip_id, "<br>",
        "main_mode:", filtered_sf_destination$main_mode, "<br>",
        "type:", "start", "<br>",
        "Start activity:",
        filtered_sf_destination$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_destination,
      lng = st_coordinates(filtered_sf_destination$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_destination$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 2], radius = 1, color = "orange",
      label = paste(
        "Person_id:",
        filtered_sf_destination$person, "<br>",
        "Trip_id:",
        filtered_sf_destination$trip_id, "<br>",
        "main_mode:", filtered_sf_destination$main_mode, "<br>",
        "type:", "end", "<br>",
        "End activity:",
        filtered_sf_destination$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_transit,
      lng = st_coordinates(filtered_sf_transit$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_transit$wkt)[seq(1, length(filtered_sf_inside$wkt), 2), 2], radius = 0.15, color = "black",
      label = paste(
        "Person_id:",
        filtered_sf_transit$person, "<br>",
        "Trip_id:",
        filtered_sf_transit$trip_id, "<br>",
        "main_mode:", filtered_sf_transit$main_mode, "<br>",
        "type:", "start", "<br>",
        "Start activity:",
        filtered_sf_transit$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addCircleMarkers(filtered_sf_transit,
      lng = st_coordinates(filtered_sf_transit$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 1],
      lat = st_coordinates(filtered_sf_transit$wkt)[seq(2, length(filtered_sf_inside$wkt), 2), 2], radius = 0.15, color = "black",
      label = paste(
        "Person_id:",
        filtered_sf_transit$person, "<br>",
        "Trip_id:",
        filtered_sf_transit$trip_id, "<br>",
        "main_mode:", filtered_sf_transit$main_mode, "<br>",
        "type:", "end", "<br>",
        "End activity:",
        filtered_sf_transit$end_activity_type, "<br>"
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addLegend(
      colors = c("blue", "red", "orange", "black"),
      labels = c(
        "Trips inside of region",
        "Trips with the end outside region",
        "Trips with the start outside region",
        "Trips that start and end outside region"
      ),
      position = "bottomleft",
      title = "Type of the point",
      opacity = 0.9
    ) %>%
    addMiniMap() %>%
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap", "Stamen.Toner",
        "Stamen.Terrain", "Esri.WorldStreetMap",
        "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
      ),
      # position it on the topleft
      position = "topleft"
    )

  return(plt)
}


#' Chooses a function to compare output_trips from the folders.
#' baseFolder contains all base outputs, policyFolder contains all policy outputs.
#'
#'
#'
#' @param baseFolder specifies data source folder with multiple base output_trips
#'
#' @param policyFolder specifies data source folder with multiple policy output_trips
#'
#' @param dump.output.to that saves result of all comparisons between each base and each policy.
#' For now it creates plotModalShiftBar() for the output_trips
#'
#' @return list of tibbles, list of all base and policy output_trips as tibble
#'
#' @export
compareBasePolicyOutput <- function(baseFolder,policyFolder,dump.output.to = matsimDumpOutputDirectory) {

  base_trips = list(NULL)
  policy_trips = list(NULL)
  for(file in list.files(baseFolder,full.names = TRUE))
  {
    temp = readTripsTable(file)
    if(!is.null(temp)){
      attr(temp,"table_name") = strsplit(file,'/')[[1]][-1]
      if(is.null(base_trips[[1]])){
        base_trips[[1]] = temp
      }else{
        base_trips = append(base_trips,list(temp))
      }
    }
  }

  for(file in list.files(policyFolder,full.names = TRUE))
  {
    temp = readTripsTable(file)
    if(!is.null(temp)){
      attr(temp,"table_name") = strsplit(file,'/')[[1]][-1]
      if(is.null(policy_trips[[1]])){
        policy_trips[[1]] = temp
      }else{
        policy_trips = append(policy_trips,list(temp))
      }
    }
  }
  i = 0;

  if (!file.exists(dump.output.to)) {
    dir.create(dump.output.to)
  }
  for(base in base_trips){

    for(policy in policy_trips){
      print(paste0(i," comparison"))
      plotModalShiftBar(base,
                        policy,
                        dump.output.to = paste0(dump.output.to,"/",i,"_",attr(base,"table_name"),"--",attr(policy,"table_name")))

      i=i+1
    }
  }
  invisible(list(base = base_trips,policy = policy_trips))
}
#' Chooses a function to compare output_trips from the folders.
#' baseFolder contains all base outputs, policyFolder contains all policy outputs.
#'
#'
#'
#' @param baseFolder specifies data source folder with multiple base output_trips
#'
#' @param policyFolder specifies data source folder with multiple policy output_trips
#'
#' @param shapeFilePath specifies shapeFile used for comparison
#' @param crs numeric of EPSG code or proj4string, can be found in network/config file from output directory of MATSim simulation
#' @param dump.output.to that saves result of all comparisons between each base and each policy.
#' For now it creates plotModalShiftBar() for the output_trips
#'
#' @return list of tibbles, list of all base and policy output_trips as tibble
#'
#' @export
compareBasePolicyShapeOutput <- function(baseFolder,policyFolder,shapeFilePath,crs,dump.output.to = matsimDumpOutputDirectory) {

  base_trips = list(NULL)
  policy_trips = list(NULL)
  shape = st_read(shapeFilePath)
  for(file in list.files(baseFolder,full.names = TRUE))
  {
    temp = readTripsTable(file)
    if(!is.null(temp)){
      attr(temp,"table_name") = strsplit(file,'/')[[1]][-1]
      if(is.null(base_trips[[1]])){
        base_trips[[1]] = temp
      }else{
        base_trips = append(base_trips,list(temp))
      }
    }
  }

  for(file in list.files(policyFolder,full.names = TRUE))
  {
    temp = readTripsTable(file)
    if(!is.null(temp)){
      attr(temp,"table_name") = strsplit(file,'/')[[1]][-1]
      if(is.null(policy_trips[[1]])){
        policy_trips[[1]] = temp
      }else{
        policy_trips = append(policy_trips,list(temp))
      }
    }
  }
  i = 0;

  if (!file.exists(dump.output.to)) {
    dir.create(dump.output.to)
  }
  for(base in base_trips){

    for(policy in policy_trips){
      print(paste0(i," comparison"))
      plotModalShiftBar(base,
                        policy,
                        crs,
                        dump.output.to = paste0(dump.output.to,"/",i,"_",attr(base,"table_name"),"--",attr(policy,"table_name")) )
      compareTripTypesBarChart(base,policy,shape,crs,dump.output.to = paste0(dump.output.to,"/",i,"_",attr(base,"table_name"),"--",attr(policy,"table_name")))
      i=i+1

    }
  }
  invisible(list(base = base_trips,policy = policy_trips))
}

#' Creates dashboard for the given table or folder with data
#'
#'
#'
#' @param table trips_output tibble from readTripsTable()
#'
#' @param append specifies if the ouput folder should be erased before creating
#'
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard and all plots using functions:
#' plotModalSplitBarChart(),plotModalSplitPieChart(),plotModalShift().
#'
#' @return generates folder with content for simwrapper out of trips table
#'
#' @export
prepareSimwrapperDashboardFromTable <- function(table, dump.output.to = matsimDumpOutputDirectory, append = FALSE) {
  if (append == FALSE) {
    if (file.exists(dump.output.to)) {
      unlink(dump.output.to, recursive = TRUE)
    }
  }
  plotModalSplitBarChart(table,dump.output.to = dump.output.to)
  plotModalSplitPieChart(table,dump.output.to = dump.output.to)
  plotAverageTravelWait(table,dump.output.to = dump.output.to)
  plotTripsByDistance(table,dump.output.to = dump.output.to)
  plotTripDistanceByMode(table,dump.output.to = dump.output.to)
  plotTripCountByDepTime(table,dump.output.to = dump.output.to)
  plotStartActCountByDepTime(table,dump.output.to = dump.output.to)
  plotEndActCountByArrTime(table,dump.output.to = dump.output.to)
  #Not sure if it is needed
  #plotModalShift(table, table,dump.output.to = dump.output.to)
}

#' Creates dashboard for the given table or folder with data
#'
#'
#'
#' @param folder specifies data source folder with tripsOutput
#'
#' @param append specifies if the ouput folder should be erased before creating
#'
#' @param dump.output.to folder that saves and configures yaml for simwrapper dashboard and all plots
#'
#' @return tibble of output_trips from folder. Generates content needed for Simwrapper
#'
#' @export
prepareSimwrapperDashboardFromFolder <- function(folder,dump.output.to = matsimDumpOutputDirectory, append = FALSE) {
  options(digits = 18)
  table = readTripsTable(folder)
  path = attr(table,"table_name")
  prepareSimwrapperDashboardFromTable(table,dump.output.to,append)
  crs = getCrsFromConfig(folder)
  if(!is.na(crs)){
    generateXYHexagonYaml(attr(table,"table_name"),crs,dump.output.to = dump.output.to)
  }
  return(table)
}

#' Reads an coordinate referenec system of MATSim output directory
#' from output_config.xml
#'
#' @param folder specifies path to find config
#'
#'
#' @return code of coordinate reference system
#'
#' @export
getCrsFromConfig <- function(folder) {

  if (grepl("output_config.xml$", folder) == TRUE)
  {
    config <- read_xml(folder)

    param_nodes = xml_find_all(config,"//param")

    coord_node = param_nodes[xml_attr(param_nodes,"name") == "coordinateSystem"]

    coord_system = xml_attr(coord_node,"value")
    return(coord_system)
  }

  files <- list.files(folder, full.names = TRUE)
  # Read from global/local directory
  # output_config.xml is contained as output_trips.csv.gz
  if (length(grep("output_config.xml$", files)) != 0) {
    config <- read_xml(files[grep("output_config.xml$", files)])

    param_nodes = xml_find_all(config,"//param")

    coord_node = param_nodes[xml_attr(param_nodes,"name") == "coordinateSystem"]

    coord_system = xml_attr(coord_node,"value")
    return(coord_system)
  }
  return(NA)
}


#' Creates/adds XY hexagon definition of output trips to a summary dashboard
#'
#' @param pathToOutputTrips specifies path to a data source of output_trips
#'
#' @param crs specifies an coordinate reference system of output_trips
#'
#' @param dump.output.to path to an folder with dashboard
#'
#' @return changed file
#'
#' @export
generateXYHexagonYaml<- function(pathToOutputTrips,crs,dump.output.to = matsimDumpOutputDirectory)
{
  yaml_list <- list(
    header = list(tab = "Summary", title = "Dashboard", description = "Plots from output directory"),
    layout = list("1" = list(
      title =  "XY Hexagon of trip origings/destinations ",
      description = "generated by generateXYHexagonYaml()",
      type = "hexagons",
      width = 1,
      props = list(height = 10,
                   width=2,
                   file =paste0("../",pathToOutputTrips) ,
                   projection = crs,
                   zoom  = 10,
                   radius = 100,
                   maxHeight=20,
                   aggregations = list("OD Summary" = list(list(title = "origins",x = "start_x",y = "start_y"),
                                                           list(title = "destinations",x = "end_x",y = "end_y")))
                   )
      )
      )
    )
  if (file.exists(paste0(dump.output.to, "/dashboard-sum.yaml"))) {
    yaml_from_directory <- read_yaml(paste0(dump.output.to, "/dashboard-sum.yaml"))
    yaml_from_directory$layout <- append(yaml_from_directory$layout, list(new_row = list(
      title =  "XY Hexagon of trip origins/destinations ",
      description = "generated by generateXYHexagonYaml()",
      type = "hexagons",
      width = 1,
      props = list(height = 10,
                   width=2,
                   file =paste0("../",pathToOutputTrips) ,
                   projection = crs,
                   zoom  = 10,
                   radius = 100,
                   maxHeight=20,
                   aggregations = list("OD Summary" = list(list(title = "origins",x = "start_x",y = "start_y"),
                                                           list(title = "destinations",x = "end_x",y = "end_y")))
      )
    )))
    names(yaml_from_directory$layout) <- 1:length(names(yaml_from_directory$layout))

    write_yaml(yaml_from_directory, paste0(dump.output.to, "/dashboard-sum.yaml"))
  } else {
    write_yaml(yaml_list, paste0(dump.output.to, "/dashboard-sum.yaml"))
  }

}
clearDashboard<- function(matsimOutputFolder){
  if (file.exists(dump.output.to)) {
    unlink(dump.output.to, recursive = TRUE)
  }
}
