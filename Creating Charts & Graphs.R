library(tidyverse)
library(knitr)
install.packages("kableExtra")
library(kableExtra)
library(stringr)
library(stringi)
install.packages("ggrepel")
library(ggrepel)

#CHART/GRAPH 1

# Using pitcher Stephen Strasburg and batter Cody Bellinger
# to illustrate Log5 method 

#Gathering Strasburg & Bellenger Data
strasburg <- WSN_Pitching[2,]
bellinger <- LAD_Batting[6,]

strasburg %>%
  kable(.)%>%
  kable_styling(.)

bellinger %>%
  kable(.)%>%
  kable_styling(.)

AL_AVG[1,c(1,30:34)] %>%
  kable(.)%>%
  kable_styling(.)

matchup <- data.frame() %>% rbind(matchup, NA)
matchup <- matchup %>%
  mutate(., Matchup = "Matchup") %>%
  mutate(., p1B = (((bellinger$p1B)*(strasburg$p1B))/(AL_AVG$p1B))/(((((bellinger$p1B)*(strasburg$p1B))/(AL_AVG$p1B)))+(((1-bellinger$p1B)*(1-strasburg$p1B))/(1-AL_AVG$p1B)))) %>%
  mutate(., p2B = (((bellinger$p2B)*(strasburg$p2B))/(AL_AVG$p2B))/(((((bellinger$p2B)*(strasburg$p2B))/(AL_AVG$p2B)))+(((1-bellinger$p2B)*(1-strasburg$p2B))/(1-AL_AVG$p2B)))) %>%
  mutate(., p3B = (((bellinger$p3B)*(strasburg$p3B))/(AL_AVG$p3B))/(((((bellinger$p3B)*(strasburg$p3B))/(AL_AVG$p3B)))+(((1-bellinger$p3B)*(1-strasburg$p3B))/(1-AL_AVG$p3B)))) %>%
  mutate(., pHR = (((bellinger$pHR)*(strasburg$pHR))/(AL_AVG$pHR))/(((((bellinger$pHR)*(strasburg$pHR))/(AL_AVG$pHR)))+(((1-bellinger$pHR)*(1-strasburg$pHR))/(1-AL_AVG$pHR)))) %>%
  mutate(., pBB = (((bellinger$pBB)*(strasburg$pBB))/(AL_AVG$pBB))/(((((bellinger$pBB)*(strasburg$pBB))/(AL_AVG$pBB)))+(((1-bellinger$pBB)*(1-strasburg$pBB))/(1-AL_AVG$pBB)))) %>%
  select(., -1)

matchup %>%
  kable(.)%>%
  kable_styling(.)


### CHART/GRAPH 2
# Using Houston Astros batting + pitching data to create example
# of lineup and pitching rotation

HOU_BAT = read_csv("HOU_BAT.csv")

HOU_Lineup = c(HOU_BAT[9,3], HOU_BAT[3,3], HOU_BAT[6,3],
               HOU_BAT[5,3], HOU_BAT[2,3], HOU_BAT[8,3],
               HOU_BAT[4,3], HOU_BAT[1,3], HOU_BAT[7,3])

# Using (clean_pitch) function to find 
# pitching rotation + relievers 

clean_pitch_adj <- function(team){
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
  pitchers
}

HOU_PITCH = read_csv("HOU_PITCH.csv")
HOU_ROTATION = clean_pitch_adj(HOU_PITCH)

HOU_Pitching_Rotation = c(HOU_ROTATION[1:9,1])

HOU_Pitching_Rotation[5] = "Brad Peacock (R)"
HOU_Pitching_Rotation[6] = "Chris Devenski (R)"
HOU_Pitching_Rotation[7] = "Will Harris (R)"
HOU_Pitching_Rotation[8] = "Josh James (R)"
HOU_Pitching_Rotation[9] = "Hector Rondon (R)"

Houston_Example = cbind(HOU_Lineup, HOU_Pitching_Rotation)
colnames(Houston_Example) = c("Batting Lineup", "Pitching Rotation")
Houston_Example %>% 
  kable(.) %>%
  kable_styling(.)

#CHART/GRAPH 3
#Creating Graph of World Series Probabilites by Regular Season Wins
#Loading in Regular Season win data for playoff teams
RegSeas <- read.csv("RegSeasRec.csv")
RegSeas$W <- as.integer(RegSeas$W)
class(RegSeas$W)
RegSeas <- filter(RegSeas, grepl("-", RegSeas$Tm))
RegSeas$Tm <- str_sub(RegSeas$Tm, start = 3, end = 5)
#Only keeping team names and wins for simplicity
RegSeas <- RegSeas[,c(2,5)]
class(RegSeas$W)
#World Series probs
Win_WS_Probabilities <- round(c(HOU_Win_WS, LAD_Win_WS, NYY_Win_WS,MIN_Win_WS,ATL_Win_WS,
                               OAK_Win_WS, TBR_Win_WS, WSN_Win_WS, STL_Win_WS,MIL_Win_WS),3)
RegSeas <- cbind(RegSeas, Win_WS_Probabilities)

ggplot(RegSeas, aes(x = W, y = Win_WS_Probabilities, label = RegSeas$Tm)) + 
  geom_label_repel(box.padding = .35) + xlab("# of Regular Season Wins") + ylab("Calculated Probability of Winning World Series")

#CHART/GRAPH 4
#Chart Displaying All Final Probabilities
Teams = c("Houston Astros", "Minnesota Twins", "New York Yankees", "Tampa Bay Rays", "Oakland Athletics",
          "Los Angeles Dodgers", "Atlanta Braves", "St. Louis Cardinals", "Washington Nationals", "Milwuakee Brewers")

LDS_Probabilities = round(c(HOU_Adv_LDS, MIN_Adv_LDS, NYY_Adv_LDS,TBR_Adv_LDS,OAK_Adv_LDS,
                            LAD_Adv_LDS, ATL_Adv_LDS, STL_Adv_LDS, WSN_Adv_LDS,MIL_Adv_LDS),3)
LCS_Probabilities = round(c(HOU_Adv_LCS, MIN_Adv_LCS, NYY_Adv_LCS,TBR_Adv_LCS,OAK_Adv_LCS,
                            LAD_Adv_LCS, ATL_Adv_LCS, STL_Adv_LCS, WSN_Adv_LCS,MIL_Adv_LCS),3)
WS_Probabilities = round(c(HOU_Adv_WS, MIN_Adv_WS, NYY_Adv_WS,TBR_Adv_WS,OAK_Adv_WS,
                           LAD_Adv_WS, ATL_Adv_WS, STL_Adv_WS, WSN_Adv_WS,MIL_Adv_WS),3)
Win_WS_Probabilities = round(c(HOU_Win_WS, MIN_Win_WS, NYY_Win_WS,TBR_Win_WS,OAK_Win_WS,
                               LAD_Win_WS, ATL_Win_WS, STL_Win_WS, WSN_Win_WS,MIL_Win_WS),3)

Probabilities_Chart = cbind(Teams, LDS_Probabilities, LCS_Probabilities,
                            WS_Probabilities, Win_WS_Probabilities)

Probabilities_Chart = as.data.frame(Probabilities_Chart)
colnames(Probabilities_Chart) = c("Team", "P(Advance to LDS)", "P(Advance to LCS)", "P(Advance to WS)", "P(Win WS)")
grid.table(Probabilities_Chart)

#CHART/GRAPH 5
#Bar Chart of Win WS Probabilities

Teams_New = c("Astros", "Twins", "Yankees", "Rays", "Athletics", "Dodgers", "Braves", "Cardinals", "Nationals", "Brewers")
team_colors = c("#005A9C", "#EB6E1F", "#CE1141", "#002B5C", "#0C2340",
                "#C41E3A", "#FFFFFF", "#8FBCE6", "#003831", "#B6922E")
bp = ggplot(Probabilities_Chart, aes(x = reorder(Teams_New, -Win_WS_Probabilities), y = Win_WS_Probabilities)) + 
  geom_bar(stat="identity", fill = team_colors) + xlab("Team") + ylab("Probability of Winning World Series")
bp
