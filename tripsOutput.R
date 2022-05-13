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

plotModalSplitPieChart<-function(tripsTable){
  ggplot(tripsTable %>% group_by(main_mode), aes(x = main_mode))+
    geom_bar()+
    coord_polar("x", start=0)
}
