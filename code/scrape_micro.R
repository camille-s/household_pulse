library(tidyverse)
library(rvest)
#the microdata in .csv form are actually quite large, and I'm not sure how to work with them in Git effectively (Git LFS returned some errors about exceeding quota). 
#This script just get the microdata straight from the Census website into RDS files without saving a .csv file anywhere in the repository (only into a temp file stored elsewhere on the computer). 
#it actually takes a while to run though (downloading takes  a while) and isn't really necessary to run twice. 

links <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html") %>% 
  html_nodes(".uscb-text-link") %>% 
  html_attr("href") %>% 
  str_subset("CSV") %>% 
  str_sub(3,-1)

walk(links, function(puf_url){
  temp <- tempfile()
  download.file(paste0("https://", puf_url),temp)
  filename <- unzip(temp, list = T)$Name %>% 
    str_subset("pulse2020_puf*")
  week <- str_extract(filename, "(?<=puf_0)([0-9])")
  
  read_csv(unz(temp, filename)) %>% 
    saveRDS(paste0("data/microdata/week_", week, ".RDS"))
  unlink(temp)
}) 

