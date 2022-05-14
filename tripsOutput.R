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
#question: it is reasonable maybe to unite all the comerial vehicle to one
plotModalSplitPieChart<-function(tripsTable){
  
  tripsTableCount <- tripsTable %>% count(main_mode)%>% mutate(n = n/sum(n)*100)

  ggplot(tripsTableCount,aes(x="",y = n,fill = main_mode))+
         geom_bar(stat="identity",width = 1)+
         coord_polar("y",start = 0)+
         geom_text(aes(label = round(n,digits = 1)),
                    position=position_stack(vjust = 0.5),
                    show.legend = FALSE,size = 2)+
         theme_minimal()
}
#Plots the Bar Chart for the percentage of used main_mode
#also: unite commercial transport?
plotModalSplitBarChart<-function(tripsTable){
  tripsTableCount <- tripsTable %>% count(main_mode)%>% mutate(n = n/sum(n)*100) %>% arrange(desc(n))
  
  ggplot(tripsTableCount,aes(x=main_mode,y = n))+
    geom_bar(stat="identity")+
    geom_text(aes(label = round(n,digits = 1)),
              position=position_stack(vjust = 0.5),
              show.legend = FALSE,size = 2)+
    theme_minimal()+
    coord_flip()
}
git
#Check the alluvial plots or sankey diagram
#question: downloading packages is allowed?
plotModalShift<-function(tripsTable1,tripsTable2){
  
}

