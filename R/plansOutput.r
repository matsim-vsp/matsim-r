library("tidyverse")
library("xml2")

xml <- read_xml("./output/test.xml")

#there is no plan_id, should it contain selected plan or all plans
#
readPlans <- function(pathToMaTSimOutputDirectory){
   
  
  return(-1)
}

test <- function(xml){
  persons <- xml_find_all(xml,"//person")
  for (i in 1:length(persons)){
    id <- xml_attr(persons[i],"id")
    print(id)
  }
}
