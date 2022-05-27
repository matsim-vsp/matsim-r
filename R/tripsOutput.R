#' Works and analyses with trips_output file from MatSim
#' Starting point of the workflow is readTripsTable
#' Plotting is based on main_mode attr of trips_output
#' 
#' transformToSf changes representation of trips_output to geographical with geometry
#' filterByRegion takes location of shapeFile and filters the trips_output excluding trips out of shapeFile





#Adding libraries
library("tidyverse")
install.packages("ggalluvial")
library("ggalluvial")
library("sf") #Geography library

#Reading of Output_Trips from directory 
readTripsTable <- function (pathToMATSimOutputDirectory = "."){
  #Get the file names, output_trips should be there
  options(digits = 12)
  
  files = list.files(pathToMATSimOutputDirectory,full.names = TRUE)
  
  #output_trips is contained as output_trips.csv.gz
  if(length(grep("output_trips.csv.gz$",files)) !=0){
    trips_output_table = read_csv2(files[grep("output_trips.csv.gz$",files)],
                                   col_types = cols(start_x = col_character(),
                                                    start_y = col_character(),
                                                    end_x = col_character(),
                                                    end_y = col_character()))
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
plotModalSplitPieChart<-function(tripsTable,unite.columns = character(0)){
  
  #If some columns should be united
  if(length(unite.columns)!=0){
    tripsTable$main_mode[grep(paste(unite.columns,collapse = "|"),tripsTable$main_mode)] = "united"
  }
  
  #tripsTableCount gives percentage representation out
  tripsTableCount <- tripsTable %>% count(main_mode)%>% mutate(n = n/sum(n)*100)
  
  
  #plotting
  ggplot(tripsTableCount,aes(x="",y = n,fill = main_mode))+
         geom_bar(stat="identity",width = 1)+
         coord_polar("y",start = 0)+
         geom_text(aes(label = round(n,digits = 1)),
                    position=position_stack(vjust = 0.5),
                    show.legend = FALSE,size = 2)+
         ggtitle("Distribution of transport type")+
         theme_void()
}

#Plots the Bar Chart for the percentage of used main_mode
plotModalSplitBarChart<-function(tripsTable,unite.columns = character(0)){
  
  #If some columns should be united
  if(length(unite.columns)!=0){
    tripsTable$main_mode[grep(paste(unite.columns,collapse = "|"),tripsTable$main_mode)] = "united"
  }
  
  tripsTableCount <- tripsTable %>% count(main_mode)%>% mutate(n = n/sum(n)*100) %>% arrange(desc(n))
  
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

#using ggaluval CRAN Package
plotModalShift<-function(tripsTable1,tripsTable2,show.changes = FALSE, unite.columns = character(0)){
  
  
  if(show.changes == TRUE){
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
    joined$base_mode[grep(paste(unite.columns,collapse = "|"),joined$base_mode)] = "united"
    joined$policy_mode[grep(paste(unite.columns,collapse = "|"),joined$policy_mode)] = "united"
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
transformToSf <- function(table, crs = 25832, geometry.type = st_point()){
  
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

#I think that it will be better to give shape_table(not the file name) as a parameter
filterByRegion <- function(tripsTable, shapeFile,start.inshape = TRUE,end.inshape = TRUE){
  shapeTable <- st_read(shapeFile)
  union_shape<-st_union(shapeTable)
  sf_table <-  transformToSf(tripsTable,crs = st_crs(union_shape),geometry.type = st_point())
  st_geometry(sf_table)<-"start_wkt"             # Set start_wkt as an active geometry
  cont1 = st_contains(union_shape,sf_table)[[1]] # Indexes of rows where start point is in shapefile
  st_geometry(sf_table)<-"end_wkt"               # Set end_wkt as and active geometry
  cont2 = st_contains(union_shape,sf_table)[[1]] # Indexes of rows where end point is in shapefile
  
  if(start.inshape && end.inshape){
    cont_union = intersect(cont1,cont2) 
  }else if(start.inshape == TRUE && end.inshape == FALSE){
    cont_union = cont1
  }
  else if(start.inshape == FALSE && end.inshape == TRUE){
    cont_union = cont2
  }else{
    cont_union = 1:nrow(table)
  }
  
  
  return(tripsTable[cont_union,])

}



