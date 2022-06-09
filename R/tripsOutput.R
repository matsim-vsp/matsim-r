#' Works and analyses with trips_output file from MatSim
#' Starting point of the workflow is readTripsTable
#' Plotting is based on main_mode attr of trips_output
#'
#' transformToSf changes representation of trips_output to geographical with geometry
#' filterByRegion takes location of shapeFile and filters the trips_output excluding trips out of shapeFile





#' @import tidyverse
#' @import sf
#' @import ggalluvial
#' @import ggrepel
#' @import tidyr


#Reading of Output_Trips from directory
#' @export
readTripsTable <- function (pathToMATSimOutputDirectory = "."){
  #Get the file names, output_trips should be there
  options(digits = 12)
  #Read from URL
  if(grepl("http",pathToMATSimOutputDirectory) == TRUE){
    trips_output_table = read_delim(pathToMATSimOutputDirectory,delim = ";",
                                    col_types = cols(start_x = col_character(),
                                                    start_y = col_character(),                                                       end_x = col_character(),
                                                    end_y = col_character(),
                                                    end_link = col_character(),
                                                    start_link = col_character()))

    trips_output_table <- trips_output_table %>% mutate(start_x = as.double(start_x),
                                                        start_y = as.double(start_y),
                                                        end_x = as.double(end_x),
                                                        end_y = as.double(end_y))

    return(trips_output_table)

  }

  files = list.files(pathToMATSimOutputDirectory,full.names = TRUE)
  #Read from global/local directory
  #output_trips is contained as output_trips.csv.gz
  if(length(grep("output_trips.csv.gz$",files)) !=0){
    trips_output_table = read_csv2(files[grep("output_trips.csv.gz$",files)],
                                   col_types = cols(start_x = col_character(),
                                                    start_y = col_character(),
                                                    end_x = col_character(),
                                                    end_y = col_character(),
                                                    end_link = col_character(),
                                                    start_link = col_character()))
                                            # person is mostly integer, but contains also chars(see Hamburg 110813 observation)
                                            # doesn't reads coordinates correctly
    trips_output_table <- trips_output_table %>% mutate(start_x = as.double(start_x),
                                                        start_y = as.double(start_y),
                                                        end_x = as.double(end_x),
                                                        end_y = as.double(end_y))
    return(trips_output_table)

  }else{ # if Directory doesn't contain trips_output, then nothing to read
    return(NULL)
  }
}

#Plots the main_mode percentage in PieChart
#' @export
plotModalSplitPieChart<-function(tripsTable, unite.columns = character(0),united.name = "united"){

  #If some columns should be united
  if(length(unite.columns)!=0){
    tripsTable$main_mode[grep(paste0(unite.columns,collapse = "|"),tripsTable$main_mode)] = united.name
  }

  #tripsTableCount gives percentage representation out
  tripsTableCount <- tripsTable %>% count(main_mode)%>% mutate(n = n/sum(n)*100)

  #getthe positions
  positions <- tripsTableCount %>%
    mutate(csum = rev(cumsum(rev(n))),
           pos = n/2 + lead(csum, 1),
           pos = if_else(is.na(pos), n/2, pos))

  #plotting
  return(ggplot(tripsTableCount,aes(x="",y = n,fill = main_mode))+
         geom_bar(stat="identity",width = 1)+
         coord_polar("y",start = 0)+
         #geom_text(aes(label = round(n,digits = 1)),
                    #position=position_stack(vjust = 0.5),
                    #show.legend = FALSE,size = 4)+
         geom_label_repel(data = positions,
                            aes(y = pos, label = paste0(round(n,digits = 1), "%")),
                            size = 4.5, nudge_x = 1, show.legend = FALSE) +
         ggtitle("Distribution of transport type")+
         theme_void())
}

#Plots the Bar Chart for the percentage of used main_mode
#' @export
plotModalSplitBarChart<-function(tripsTable,unite.columns = character(0),united.name = "united"){

  #If some columns should be united
  if(length(unite.columns)!=0){
    tripsTable$main_mode[grep(paste0(unite.columns,collapse = "|"),tripsTable$main_mode)] = united.name
  }
  #Get percentage
  tripsTableCount <- tripsTable %>% count(main_mode)%>% mutate(n = n/sum(n)*100) %>% arrange(desc(n))
  #plotting
  ggplot(tripsTableCount,aes(x=main_mode,y = n,fill= main_mode))+
    geom_bar(stat="identity")+
    geom_text(aes(label = round(n,digits = 1)),
              position=position_stack(vjust = 0.5),
              size = 2)+
    theme_minimal()+
    labs(x = "main_mode",y = "Percentage")+
    ggtitle("Distribution of transport type (in %)")+
    theme(legend.position = "none")+
    coord_flip()
}

#using ggaluvial CRAN Package
#supress warning message options(warn = -1)
#' @export
plotModalShift<-function(tripsTable1,tripsTable2,show.onlyChanges = FALSE, unite.columns = character(0)){


  if(show.onlyChanges == TRUE){
    joined <- as_tibble(inner_join(tripsTable1,tripsTable2 %>%
                                select(trip_id,main_mode),by = "trip_id") %>%
                          rename(base_mode = main_mode.x,policy_mode = main_mode.y))
    joined <- joined %>% filter(base_mode!=policy_mode)%>% group_by(base_mode,policy_mode)%>%count()
  }else{
    joined <- as_tibble(inner_join(tripsTable1,tripsTable2 %>%
                                select(trip_id,main_mode),by = "trip_id") %>% rename(base_mode = main_mode.x,policy_mode = main_mode.y))
    joined <- joined %>% group_by(base_mode,policy_mode)%>%count()
  }


  # If the unite.commercials flag is set to TRUE, then join all commercials under 1 name commercial
  if(length(unite.columns) != 0){
    joined$base_mode[grep(paste0(unite.columns,collapse = "|"),joined$base_mode)] = "united"
    joined$policy_mode[grep(paste0(unite.columns,collapse = "|"),joined$policy_mode)] = "united"
  }


  ggplot(joined,aes(y = n,axis1 = base_mode,axis2 = policy_mode))+
    geom_alluvium(aes(fill = base_mode),width = 1/15)+
    geom_stratum(width = 1/10, fill = "black", color = "grey")+
    geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
    scale_x_discrete(limits = c("Base Mode", "Policy Mode"), expand = c(.05, .05))
}

#Use parameter for defining the point_representation
#column start_wkt - POINT and column end_wkt - POINT
#or
#column wkt - MULTIPOINT(start,end)
#or
#column wkt - LINESTRING
#geometry.type is also a attribute for the point representation. What variant is better

#' @export
transformToSf <- function(table, crs, geometry.type = st_multipoint()){

  if(class(geometry.type)[2] == "POINT"){
    table1 <- table %>%
      #mutate(wkt = paste("MULTIPOINT(", start_x, " ", start_y, ",", end_x, " ", end_y, ")", sep =""))
      mutate(start_wkt = paste("POINT(", start_x, " ", start_y,")", sep =""))
    table2 <- table %>%
      mutate(end_wkt = paste("POINT(", end_x, " ", end_y,")", sep =""))
    attr(table,"geometry.type")<-"POINT"


    table1_wkt<- st_as_sf(table1,wkt = "start_wkt")%>% select(-start_x,-start_y,-end_x,-end_y)
    table2_wkt<- st_as_sf(table2,wkt = "end_wkt")%>% select(-start_x,-start_y,-end_x,-end_y)


    result_table<- table1_wkt %>% mutate(end_wkt = table2_wkt$end_wkt)
    st_geometry(result_table)<-"start_wkt"
    st_crs(result_table) <- crs
    st_geometry(result_table)<-"end_wkt"
    st_crs(result_table) <- crs
    st_geometry(result_table)<-"start_wkt"
    return(result_table)

  }else if(class(geometry.type)[2] == "MULTIPOINT"){

    table <- table %>%
      mutate(wkt = paste("MULTIPOINT(", start_x, " ", start_y, ",", end_x, " ", end_y, ")", sep =""))
    attr(table,"geometry.type")<-"MULTIPOINT"


    result_table<- st_as_sf(table,wkt = "wkt")%>% select(-start_x,-start_y,-end_x,-end_y)

    st_crs(result_table) <- crs
    return(result_table)
  }else if(class(geometry.type)[2] == "LINESTRING"){

    table <- table %>%
      mutate(wkt = paste("LINESTRING(", start_x, " ", start_y, ",", end_x, " ", end_y, ")", sep =""))
    attr(table,"geometry.type")<-"LINESTRING"


    result_table<- st_as_sf(table,wkt = "wkt")%>% select(-start_x,-start_y,-end_x,-end_y)

    st_crs(result_table) <- crs
    return(result_table)
  }
  else{
    return(NA)
  }
}
#' @export
filterByRegion <- function(tripsTable,shapeTable,crs,start.inshape = TRUE,end.inshape = TRUE){

  #shapeTable <- st_read(shapeFile)
  if(st_crs(shapeTable) == NA){
    st_crs(shapeTable)<-crs
  }

  sf_table <-  transformToSf(tripsTable,crs = crs,geometry.type = st_point())
  shapeTable <- st_transform(shapeTable,crs = crs)
  #shapeTable isn't table - shape

  union_shape<-st_union(shapeTable) # transforms the crs back to the previous in the file
  union_shape<- st_transform(union_shape,crs = st_crs(shapeTable))


  st_geometry(sf_table)<-"start_wkt"             # Set start_wkt as an active geometry
  cont1 = st_contains(union_shape,sf_table)[[1]] # Indexes of rows where start point is in shapefile

  st_geometry(sf_table)<-"end_wkt"               # Set end_wkt as and active geometry
  cont2 = st_contains(union_shape,sf_table)[[1]] # Indexes of rows where end point is in shapefile

  #get trips that ended outside of shape
  cont_end_outside = setdiff(1:nrow(sf_table),cont2)

  #get trips that started outside of shape
  cont_start_outside = setdiff(1:nrow(sf_table),cont1)

  if(start.inshape== TRUE && end.inshape == TRUE){
    cont_union = intersect(cont1,cont2)
  }else if(start.inshape == TRUE && end.inshape == FALSE){

    cont_union = intersect(cont1,cont_end_outside)
  }
  else if(start.inshape == FALSE && end.inshape == TRUE){
    cont_union = intersect(cont2,cont_start_outside)
  }else{
    cont_union = intersect(cont_start_outside,cont_end_outside)  # Give back trips that are neither starting and ending outside the area
  }


  return(tripsTable[cont_union,])

}
#bug if filtered_set is empty, then handle the mistake
#' @export
plotMapWithTrips <- function(table,shapeTable,crs,start.inshape = TRUE,end.inshape = TRUE){
  table = table[1:5000,]
  #table_sf = transformToSf(table,crs = crs)
  filtered = filterByRegion(table,shapeTable,crs = crs, start.inshape, end.inshape)

  filtered_sf = transformToSf(filtered,crs = crs,geometry.type = st_point())
  filtered_sf_start = filtered_sf
  st_geometry(filtered_sf_start) = "start_wkt"
  filtered_sf_end = filtered_sf
  st_geometry(filtered_sf_end) = "end_wkt"
  #shape = st_read(shapePath)
  if(st_crs(shapeTable) == NA){
    ct_crs(shapeTable) = crs
  }
  shapeTable = st_transform(shapeTable,crs = crs)

  colors  = c("Start" = "green","End" = "red")
  shapes  = c("Start" = 5,"End" = 3)

  ggplot()+
    geom_sf(data = shapeTable)+
    #geom_sf(data = )
    geom_sf(data = filtered_sf_start,aes(color = "Start"),size = 1,shape = 5)+
    geom_sf(data = filtered_sf_end,aes(color ="End"),size = 1,shape = 3)+
    labs(color = "Type")+
    scale_colour_manual(values=colors)
}
#bug if filtered_set is empty, then handle the mistake
#' @export
plotMapWithTripsType <- function(table,shapeTable,crs){
  table = table[1:200,]
  #table_sf = transformToSf(table,crs = crs)
  #Maybe union all this tables as 1 extended with additional column
  filtered_inside = filterByRegion(table,shapeTable,crs = crs, start.inshape = TRUE, end.inshape = TRUE)
  filtered_origin = filterByRegion(table,shapeTable,crs = crs, start.inshape = TRUE, end.inshape = FALSE)
  filtered_destination = filterByRegion(table,shapeTable,crs = crs, start.inshape = FALSE, end.inshape = TRUE)
  filtered_transit = filterByRegion(table,shapeTable,crs = crs, start.inshape = FALSE, end.inshape = FALSE)

  filtered_sf_inside = transformToSf(filtered_inside,crs = crs,geometry.type = st_multipoint())
  filtered_sf_origin = transformToSf(filtered_origin,crs = crs,geometry.type = st_multipoint())
  filtered_sf_destination = transformToSf(filtered_destination,crs = crs,geometry.type = st_multipoint())
  filtered_sf_transit = transformToSf(filtered_transit,crs = crs,geometry.type = st_multipoint())

  if(st_crs(shapeTable) == NA){
    ct_crs(shapeTable) = crs
  }
  shapeTable = st_transform(shapeTable,crs = crs)

  colors  = c("inside" = "green","origin" = "red","destination" = "orange","transit" = "blue")
  shapes  = c("Start" = 5,"End" = 3)

  ggplot()+
    geom_sf(data = shapeTable)+
    #geom_sf(data = )
    geom_sf(data = filtered_sf_inside,aes(color = "inside"),size = 3,alpha = 0.5)+
    geom_sf(data = filtered_sf_origin,aes(color = "origin"),size = 3,alpha = 0.4)+
    geom_sf(data = filtered_sf_destination,aes(color = "destination"),size = 3,alpha = 0.3)+
    geom_sf(data = filtered_sf_transit,aes(color ="transit"),size = 2,alpha = 0.1)+
    labs(color = "Type")+
    scale_colour_manual(values=colors)
}
#Mb Create analytical functions/plots of trips_type(Transit,indide,destination,origin) distribution(like for modal)

