library(tidyverse)
#Reformatting/updating pitching data to only include 4 starters and a reliever/closer.
ATL_PITCH <- ATL_PITCH[]
Relief <- ATL_PITCH[,2:27] %>% summarise_each(funs(mean))
Relief <- Relief %>%
  add_column(., "Relief") %>%
  select(27, everything()) %>%
  add_column(., "ATL")
colnames(Relief)[1] <- "Name" 
colnames(Relief)[28] <- "ATL"
ATL_PITCH <- rbind(ATL_PITCH, Relief)
ATL_PITCH <- ATL_PITCH[c(1,2,3,4,10),]

