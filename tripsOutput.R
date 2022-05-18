library("tidyverse")

#Reading of Output_Trips from directory 
readTripsTable <- function (pathToMATSimOutputDirectory){
  #Get the file names, output_trips should be there
  files = list.files(pathToMATSimOutputDirectory)
  
  #output_trips is contained as output_trips.csv.gz
  if(length(grep("output_trips.csv.gz$",files)) !=0){
    trips_output_table = read_csv2(files[grep("output_trips.csv.gz$",files)],
                                   col_types = c('c','d','c','t','t','t','d','d','c','c','c','c','c','c','c','d','d','c','c','d','d','c','c'))
                                            #person is mostly integer, but contains also chars(see Hamburg 110813 observation)
                
  }else{ # if Directory doesn't contain trips_output, then nothing to read
    return(NULL)
  }
}
#Plots the main_mode percentage in PieChart
#question: it is reasonable maybe to unite all the comercial vehicle to one
plotModalSplitPieChart<-function(tripsTable){
  
  tripsTableCount <- tripsTable %>% count(main_mode)%>% mutate(n = n/sum(n)*100)

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
#unite commercial transport?
#use of external libraries, e.g.:ggrepel ?
plotModalSplitBarChart<-function(tripsTable){
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
#Check the alluvial plots or sankey diagram
#question: downloading packages is allowed?
plotModalShift<-function(tripsTable1,tripsTable2){
  joined <- as_tibble(merge(tripsTable1,tripsTable2 %>% 
                    select(trip_id,main_mode),by = "trip_id") %>% rename(base_mode = main_mode.x,policy_mode = main_mode.y))
  joined <- joined %>% group_by(base_mode,policy_mode)%>%count()
  
  # Not sure if joining commercial under 1 type is needed
  joined$base_mode[grep("commercial",joined$base_mode)] = "commercial"
  joined$policy_mode[grep("commercial",joined$policy_mode)] = "commercial"
  
  ggplot(joined,aes(y = n,axis1 = base_mode,axis2 = policy_mode))+
    geom_alluvium(aes(fill = base_mode),width = 1/15)+
    geom_stratum(width = 1/10, fill = "black", color = "grey")+
    geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
    scale_x_discrete(limits = c("Base Mode", "Policy Mode"), expand = c(.05, .05))+
    scale_fill_brewer(type = "qual", palette = "Set1")
}


test_alluvia_plot <- function(tripsTable){
  f <- factor(tripsTable$main_mode)
  tripsTable$new_main_mode <- sample(levels(f),size = nrow(tripsTable),replace= TRUE)
  tripsModalShift <- tripsTable %>% select(trip_id,main_mode,new_main_mode) %>% group_by(main_mode,new_main_mode) %>% count()
  tripsModalShift$main_mode[grep("commercial",tripsModalShift$main_mode)] = "commercial"
  tripsModalShift$new_main_mode[grep("commercial",tripsModalShift$new_main_mode)] = "commercial"
  
  ggplot(tripsModalShift,aes(y = n,axis1 = main_mode,axis2 = new_main_mode))+
    geom_alluvium(aes(fill = main_mode),width = 1/15)+
    geom_stratum(width = 1/12, fill = "black", color = "grey")+
    geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
    scale_x_discrete(limits = c("Old Mode", "New Mode"), expand = c(.05, .05))+
    scale_fill_brewer(type = "qual", palette = "Set1")
    
    
}
