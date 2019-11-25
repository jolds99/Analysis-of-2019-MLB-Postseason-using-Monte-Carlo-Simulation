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


inning <- function(n, ibases, iouts){
  runs <- rep(NA, n)
  for(i in 1:n){
  #  inningNum <- 1
  #  return(inningNum)
  #  for(inning in 1:n){
      #inning <- inning + 1
      play <- 0
      outcome <- rep()
      outs <- iouts
      runs[i] <- 0
      bases <- ibases
      while(outs < 3){
        #print(paste0("outs = ", outs))
        play <- play + 1
        outcome <- c(outcome, NA)
        outcome[play] <-  sample(c("Out","1B", "2B", "3B", "HR", "BB"), 1, 
                 prob = c(1-LEAGUE_AVG$p1B - LEAGUE_AVG$p2B - LEAGUE_AVG$p3B - LEAGUE_AVG$pHR - LEAGUE_AVG$pBB, 
                          LEAGUE_AVG$p1B, LEAGUE_AVG$p2B, LEAGUE_AVG$p3B, LEAGUE_AVG$pHR, LEAGUE_AVG$pBB))
        if(outcome[play] == "1B"){
          #print("Single")
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
          #print(as.character(bases))
        }
        else if(outcome[play] == "2B"){
          #print("Double")
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
          #print(as.character(bases))
        }
        else if(outcome[play] == "3B"){
          #print("Triple")
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
          #print(as.character(bases))
        }
        else if(outcome[play] == "HR"){
          #print("Home run")
          if(bases == 000){
            runs[i] <- runs[i] + 1
          }
          else if(bases == 001){
            bases <- 000
            runs[i] <- runs[i] + 2
          }
          else if(bases == 010){
            bases <- 000
            runs[i] <- runs[i] + 2
          }
          else if(bases == 100){
            bases <- 000
            runs[i] <- runs[i] + 2
          }
          else if(bases == 011){
            bases <- 000
            runs[i] <- runs[i] + 3
          }
          else if(bases == 101){
            bases <- 000
            runs[i] <- runs[i] + 3
          }
          else if(bases == 110){
            bases <- 000
            runs[i] <- runs[i] + 3
          }
          else if(bases == 111){
            bases <- 000
            runs[i] <- runs[i] + 4
          }
          #print(as.character(bases))
        }
        else if(outcome[play] == "BB"){
          #print("Walk")
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
          #print(as.character(bases))
        }
        else{
          #print("Out")
          #print(as.character(bases))
          outs <- outs + 1
        }
      }
    #}
  }
  #return(paste0("runs = ", runs))
  runs
}
gameScenB <- c("000", "001", "010","100","011", "110", "101", "111")
gameScenO <- c("Runners", "0", "0", "1", "1", "2", "2")
gameScenDF <- rbind(rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6))
colnames(gameScenDF) <- c("0", "0", "1", "1", "2", "2")
row.names(gameScenDF) <- c("000", "001", "010","100","011", "110", "101", "111")
gameScen <- function(n,b,o){
  sim <- inning(n,b,o)
  res <- sum(sim)/n
  gameScenDF[]
  return(res)
}
gameScen(10000,111,0)



