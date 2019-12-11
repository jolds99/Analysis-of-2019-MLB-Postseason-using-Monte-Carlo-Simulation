library(tidyverse)
library(knitr)
install.packages("kableExtra")
library(kableExtra)

#Gathering Strasburg Data
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

matchup <- data.frame() %>%
  rbind(matchup, NA)
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
