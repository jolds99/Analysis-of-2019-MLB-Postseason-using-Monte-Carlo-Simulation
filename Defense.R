#Finding probability of an error occurring
#Calculated by dividing number of errors by the number of putouts
LEAGUE_DEFENSE <- read_csv("LEAGUE_DEFENSE.csv")
team_list <- c("ATL", "HOU" , "LAD", "MIL", "MIN", "NYY", "OAK", "STL", "TBR", "WSN")
#Creating data frame that only includes teams in the postseason
PSteam_def <- LEAGUE_DEFENSE[0,]
for(j in 1:length(team_list)){
  for(i in 1:nrow(LEAGUE_DEFENSE)){
    if(isTRUE(team_list[j] == LEAGUE_DEFENSE$Tm[i])){
      PSteam_def <- rbind(PSteam_def, LEAGUE_DEFENSE[i,])
    }
    else{}
  }
}
#Adding probability of the defense committing an error
PSteam_def <- PSteam_def %>% 
  mutate(pERR = PSteam_def$E/PSteam_def$PO)

