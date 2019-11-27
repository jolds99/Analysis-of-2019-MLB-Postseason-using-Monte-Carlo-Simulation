install.packages("tidyverse")
library(tidyverse)

#Loading Batting Data
ATL_BAT = read_csv("ATL_BAT.csv")
HOU_BAT = read_csv("HOU_BAT.csv")
LAD_BAT = read_csv("LAD_BAT.csv")
MIL_BAT = read_csv("MIL_BAT.csv")
MIN_BAT = read_csv("MIN_BAT.csv")
NYY_BAT = read_csv("NYY_BAT.csv")
OAK_BAT = read_csv("OAK_BAT.csv")
STL_BAT = read_csv("STL_BAT.csv")
TBR_BAT = read_csv("TBR_BAT.csv")
WSN_BAT = read_csv("WSN_BAT.csv")

#Batting Probability Function
batting_probs = function(team){
  df = as.data.frame(team) 
  df = df %>%
    #Singles
    mutate(., p1B = (df[,9]-(df[,10] + df[,11] + df[,12]))/df[,6]) %>%
    #Doubles (not taken as an average of doubles and triples based on league average
    #because we believe that the odds of a double and triple happening is already
    #well represented in the season data))
    mutate(., p2B = (df[,10]/df[,6])) %>%
    #Triples
    mutate(., p3B = (df[,11]/df[,6])) %>%
    #Home Runs
    mutate(., pHR = (df[,12]/df[,6])) %>%
    #Walks and HBP
    mutate(., pBB = (df[,16] + df[,25])/df[,6]) %>%
    #Selecting only names and probabilities
    select(.,c("Name", "p1B", "p2B", "p3B", "pHR", "pBB"))
  df
}


ATL_Batting = batting_probs(ATL_BAT)
HOU_Batting = batting_probs(HOU_BAT)
LAD_Batting = batting_probs(LAD_BAT)
MIL_Batting = batting_probs(MIL_BAT)
MIN_Batting = batting_probs(MIN_BAT)
NYY_Batting = batting_probs(NYY_BAT)
OAK_Batting = batting_probs(OAK_BAT)
STL_Batting = batting_probs(STL_BAT)
TBR_Batting = batting_probs(TBR_BAT)
WSN_Batting = batting_probs(WSN_BAT)


