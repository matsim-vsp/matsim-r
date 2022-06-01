library("tidyverse")
library("xml2")

readEventsXml<-function(xmlpath){#(pathToMATSimOutputDirectory = "."){
  #Get the file names, output_trips should be there
  options(digits = 12)
  
  #files = list.files(pathToMATSimOutputDirectory,full.names = TRUE)
  #output_trips is contained as output_trips.csv.gz
  #if(length(grep("output_events.xml$",files)) !=0){
    #events_xml = read_xml(files[grep("output_events.xml$",files)][1])
  #}
  events_xml = read_xml(xmlpath)
  events = xml_find_all(events_xml,"//event")
  df = data.frame(event_id = c(1:length(events)))
  df = df %>% 
    mutate(time = xml_attr(events,"time")) %>%
    mutate(type = xml_attr(events,"type"))
  
  return(df)
  
}