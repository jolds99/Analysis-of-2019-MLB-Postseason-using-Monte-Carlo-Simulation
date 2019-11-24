source("Fix_Pitching.R")
#Inning data in format of home_team, home_score, vis_team, vis_score, inning, 
#topbot, outs, runners_pos, home_pitch, vis_pitch
#n = number of simulations
LEAGUE_AVG$p1B
inning_test <- c(1,000,0)

inning <- function(n, bases, outs){
  runs <- rep(NA, n)
  for(i in 1:n){
    runs[i] <- 0
    while(outs < 3){
      if(1 == rbinom(1,1, prob = LEAGUE_AVG$p1B)){
        if(bases == 000){
          bases <- 001
        }
        else if(bases == 001){
          bases <- 101
        }
        else if(bases == 010){
          bases <- 001
          runs[i] <- runs[i] + 1
        }
        else if(bases == 100){
          bases <- 001
          runs[i] <- runs[i] + 1
        }
        else if(bases == 011){
          bases <- 101
          runs[i] <- runs[i] + 1
        }
        else if(bases == 101){
          bases <- 101
          runs[i] <- runs[i] + 1
        }
        else if(bases == 110){
          bases <- 001
          runs[i] <- runs[i] + 2
        }
        else if(bases == 111){
          bases <- 001
          runs[i] <- runs[i] + 2
        }
      }
      else if(1 == rbinom(1,1, prob = LEAGUE_AVG$p2B)){
        if(bases == 000){
          bases <- 010
        }
        else if(bases == 001){
          bases <- 010
          runs[i] <- runs[i] + 1
        }
        else if(bases == 010){
          bases <- 010
          runs[i] <- runs[i] + 1
        }
        else if(bases == 100){
          bases <- 010
          runs[i] <- runs[i] + 1
        }
        else if(bases == 011){
          bases <- 010
          runs[i] <- runs[i] + 2
        }
        else if(bases == 101){
          bases <- 010
          runs[i] <- runs[i] + 2
        }
        else if(bases == 110){
          bases <- 010
          runs[i] <- runs[i] + 2
        }
        else if(bases == 111){
          bases <- 010
          runs[i] <- runs[i] + 3
        }
      }
      else if(1 == rbinom(1,1, prob = LEAGUE_AVG$p3B)){
        if(bases == 000){
          bases <- 100
        }
        else if(bases == 001){
          bases <- 100
          runs[i] <- runs[i] + 1
        }
        else if(bases == 010){
          bases <- 100
          runs[i] <- runs[i] + 1
        }
        else if(bases == 100){
          bases <- 100
          runs[i] <- runs[i] + 1
        }
        else if(bases == 011){
          bases <- 100
          runs[i] <- runs[i] + 2
        }
        else if(bases == 101){
          bases <- 100
          runs[i] <- runs[i] + 2
        }
        else if(bases == 110){
          bases <- 100
          runs[i] <- runs[i] + 2
        }
        else if(bases == 111){
          bases <- 100
          runs[i] <- runs[i] + 3
        }
      }
      else if(1 == rbinom(1,1, prob = LEAGUE_AVG$pHR)){
        if(bases == 000){
          runs[i] <- runs[i] + 1
        }
        else if(bases == 001){
          runs[i] <- runs[i] + 2
        }
        else if(bases == 010){
          runs[i] <- runs[i] + 2
        }
        else if(bases == 100){
          runs[i] <- runs[i] + 2
        }
        else if(bases == 011){
          runs[i] <- runs[i] + 3
        }
        else if(bases == 101){
          runs[i] <- runs[i] + 3
        }
        else if(bases == 110){
          runs[i] <- runs[i] + 3
        }
        else if(bases == 111){
          runs[i] <- runs[i] + 4
        }
      }
      else if(1 == rbinom(1,1, prob = LEAGUE_AVG$pBB)){
        if(bases == 000){
          bases <- 001
        }
        else if(bases == 001){
          bases <- 011
        }
        else if(bases == 010){
          bases <- 011
        }
        else if(bases == 100){
          bases <- 101
        }
        else if(bases == 011){
          bases <- 111
        }
        else if(bases == 101){
          bases <- 111
        }
        else if(bases == 110){
          bases <- 111
        }
        else if(bases == 111){
          bases <- 111
          runs[i] <- runs[i] + 1
        }
      }
      else{
        outs <- outs + 1
      }
    }
  }
  runs
}
inning(100,111,0)




