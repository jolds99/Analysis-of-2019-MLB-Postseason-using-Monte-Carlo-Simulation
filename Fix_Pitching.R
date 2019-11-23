library(tidyverse)
library(stringr)
MLB_PITCH <- read_csv("MLB_PITCH.csv")
#Clean MLB_PITCH 
#Remove teams stats, player first initial and player WAR
for(i in 2:31){
  MLB_PITCH[i,2] <- str_sub(MLB_PITCH[i,2], start = 1, end = 3)
  for(l in 3:11){
    MLB_PITCH[i,l] <- str_sub(MLB_PITCH[i,l], start = 3, end = -8)
    MLB_PITCH[i,l] <- str_trim(MLB_PITCH[i,l])
  }
}
#Making starting rotation consist of 4 players, not 5
MLB_PITCH <- MLB_PITCH[,-7]

#Reformatting/updating pitching data to only include 4 starters and a reliever/closer.
clean_pitch <- function(team){
  df <- as.data.frame(team)
  #Gathering which 8 players are included in the MLB_PITCH data
  rotation <- MLB_PITCH[grep(team[1,29], MLB_PITCH$X2), 3:10]
  #Creating data frame that will eventually consist of 4 starters and a reliever
  pitchers <- df[0,]
  #Adding players from rotation to pitchers, including their stats
  for(j in 1:length(rotation)){
    for(i in 1:nrow(df)){
      if(isTRUE(str_detect(as.character(df[i,1]), as.character(rotation[1,j])))){
        pitchers <- rbind(pitchers, df[i,])
      }
      else{}
    }
  }
  #Sorting by games played, as one more reliever is needed to average the 5 most
  #prolific relievers into one reliever to use in the simulation.
  df <- arrange(df, -G)
  #Adding all pitchers to pitchers data frame
  r <- 0
  while(r < 1){
    for(i in 1:nrow(df)){
      if(colSums(stri_extract_last_words(df[i,1]) == pitchers) == 0){
        pitchers <- rbind(pitchers, df[i,])
        r <- 1
      }
      else{}
    }
  }
  #Removing all duplicate players
  pitchers <- unique(pitchers)
  #Removing all players beside 4 starters and 5 most prolific reliever
  pitchers <- pitchers[1:9,]
  #Summarizing the 5 relievers into one, average player
  reliever <- pitchers[5:9,2:28] %>% summarise_each(funs(mean))
  reliever <- reliever %>%
    cbind("Reliever", .) %>%
    cbind(., pitchers[1,29])
  colnames(reliever)[1] <- "Name" 
  colnames(reliever)[29] <- pitchers[1,29]
  #Removing the relievers
  pitchers <- pitchers[1:4,]
  #Adding the compiled reliever
  pitchers <- rbind(pitchers, reliever)
  pitchers
}
#Getting data for all 10 teams
ATL_PITCH <- read_csv("ATL_PITCH.csv")
ATL_Rotation <- clean_pitch(ATL_PITCH)
HOU_PITCH <- read_csv("HOU_PITCH.csv")
HOU_Rotation <- clean_pitch(HOU_PITCH)
LAD_PITCH <- read_csv("LAD_PITCH.csv")
LAD_Rotation <- clean_pitch(LAD_PITCH)
MIL_PITCH <- read_csv("MIL_PITCH.csv")
MIL_Rotation <- clean_pitch(MIL_PITCH)
MIN_PITCH <- read_csv("MIN_PITCH.csv")
MIN_Rotation <- clean_pitch(MIN_PITCH)
NYY_PITCH <- read_csv("NYY_PITCH.csv")
NYY_Rotation <- clean_pitch(NYY_PITCH)
OAK_PITCH <- read_csv("OAK_PITCH.csv")
OAK_Rotation <- clean_pitch(OAK_PITCH)
STL_PITCH <- read_csv("STL_PITCH.csv")
STL_Rotation <- clean_pitch(STL_PITCH)
TBR_PITCH <- read_csv("TBR_PITCH.csv")
TBR_Rotation <- clean_pitch(TBR_PITCH)
WSN_PITCH <- read_csv("WSN_PITCH.csv")
WSN_Rotation <- clean_pitch(WSN_PITCH)

