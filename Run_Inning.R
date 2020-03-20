source("Fix_Pitching.R")
install.packages("kableExtra")
library(ggplot2)
library(tidyverse)

inning <- function(n, ibases, iouts){
  runs <- rep(NA, n)
  for(i in 1:n){
    #Initializing values that are tracked throughout the inning
    play <- 0
    outcome <- rep()
    outs <- iouts
    runs[i] <- 0
    bases <- ibases
    while(outs < 3){
      #print(paste0("outs = ", outs))
      #Simulating the odds of a type of hit, walk or out
      #Based on league average probilities
      play <- play + 1
      outcome <- c(outcome, NA)
      outcome[play] <-  sample(c("Out","1B", "2B", "3B", "HR", "BB"), 1, 
               prob = c(1-AL_AVG$p1B - AL_AVG$p2B - AL_AVG$p3B - AL_AVG$pHR - AL_AVG$pBB, 
                        AL_AVG$p1B, AL_AVG$p2B, AL_AVG$p3B, AL_AVG$pHR, AL_AVG$pBB))
      if(outcome[play] == "1B"){
        #print("Single")
        if(bases == 000){
          bases <- 001
        }
        else if(bases == 001){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.7,0.3))
          if(adv == "adv"){
            bases <- 101
          }
          else if(adv == "no_adv"){
            bases <- 011
          }
        }
        else if(bases == 010){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.9,0.1))
          if(adv == "adv"){
            bases <- 001
            runs[i] <- runs[i] + 1
          }
          else if(adv == "no_adv"){
            bases <- 101
          }
        }
        else if(bases == 100){
          bases <- 001
          runs[i] <- runs[i] + 1
        }
        else if(bases == 011){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.7,0.3))
          if(adv == "adv"){
            bases <- 101
            runs[i] <- runs[i] + 1
          }
          else if(adv == "no_adv"){
            adv <- sample(c("adv", "no_adv"), 1, prob = c(0.9,0.1))
            if(adv == "adv"){
              bases <- 011
              runs[i] <- runs[i] + 1
            }
            else if(adv == "no_adv"){
              bases <- 111
            }
          }
        }
        else if(bases == 101){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.98,0.02))
          if(adv == "adv"){
            runs[i] <- runs[i] + 1
            adv <- sample(c("adv", "no_adv"), 1, prob = c(0.65,0.35))
            if(adv == "adv"){
              bases <- 101
            }
            else if(adv == "no_adv"){
              bases <- 011
            }
          }
          else if(adv == "no_adv"){
            bases <- 111
          }
        }
        else if(bases == 110){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.9,0.1))
          if(adv == "adv"){
            bases <- 001
            runs[i] <- runs[i]+2
          }
          else if(adv == "no_adv"){
            adv <- sample(c("adv", "no_adv"), 1, prob = c(0.65,0.35))
            if(adv == "adv"){
              bases <- 101
            }
            else if(adv == "no_adv"){
              bases <- 011
            }
          }
        }
        else if(bases == 111){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.7,0.3))
          if(adv == "adv"){
            bases <- 101
            runs[i] <- runs[i]+2
          }
          else if(adv == "no_adv"){
            #Counting run for runner on third
            runs[i] <- runs[i]+1
            #Checking advance for runner on second
            adv <- sample(c("adv", "no_adv"), 1, prob = c(0.9,0.1))
            if(adv == "adv"){
              runs[i] <- runs[i]+1
              #Checking for advance for runner on first
              adv <- sample(c("adv", "no_adv"), 1, prob = c(0.7,0.3))
              if(adv == "adv"){
                bases <- 101
              }
              else if(adv == "no_adv"){
                bases <- 011
              }
            }
            else if(adv == "no_adv"){
              bases <- 111
            }
          }
        }
        #print(as.character(bases))
      }
      else if(outcome[play] == "2B"){
        #print("Double")
        if(bases == 000){
          bases <- 010
        }
        else if(bases == 001){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.85,0.15))
          if(adv == "adv"){
            bases <- 010
            runs[i] <- runs[i]+1
          }
          else if(adv == "no_adv"){
            bases <- 110
          }
        }
        else if(bases == 010){
          adv <- sample(c("adv", "no_adv"), 1, prob = c(0.99,0.01))
          if(adv == "adv"){
            bases <- 010
            runs[i] <- runs[i]+1
          }  
          else if(adv == "no_adv"){
            bases <- 110
          }
        }
        else if(bases == 100){
          bases <- 010
          runs[i] <- runs[i] + 1
        }
        else if(bases == 011){
          #Checking advance for runner on first
          adv1 <- sample(c("adv", "no_adv"), 1, prob = c(0.85,0.15))
          if(adv1 == "adv"){
            runs[i] <- runs[i]+2
            bases <- 010
          }  
          else if(adv1 == "no_adv"){
            runs[i] <- runs[i]+1
            bases <- 110
          }
        }
        else if(bases == 101){
          #Checking advance for runner on first
          adv1 <- sample(c("adv", "no_adv"), 1, prob = c(0.85,0.15))
          if(adv1 == "adv"){
            runs[i] <- runs[i]+2
            bases <- 010
          }  
          else if(adv1 == "no_adv"){
            runs[i] <- runs[i]+1
            bases <- 110
          }
        }
        else if(bases == 110){
          bases <- 010
          runs[i] <- runs[i] + 2
        }
        else if(bases == 111){
          #Checking advance for runner on first
          adv1 <- sample(c("adv", "no_adv"), 1, prob = c(0.85,0.15))
          if(adv1 == "adv"){
            runs[i] <- runs[i]+3
            bases <- 010
          }  
          else if(adv1 == "no_adv"){
            runs[i] <- runs[i]+2
            bases <- 110
          }
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
        #Double Play (probability comes from https://legacy.baseballprospectus.com/sortable/index.php?cid=2861695)
        dp <- sample(c("DP", "No_DP"), 1, prob = c(0.10109, 0.89891))
        if(dp == "DP" & outs < 2 & bases == 001){
          #print(dp)
          outs <- outs + 2
          bases <- 000
        }
        else if(dp == "DP" & outs < 2 & bases == 011){
          #print(dp)
          outs <- outs + 2
          #Deciding which runners got out in the double play
          dpbase <- sample(c("100", "010", "001"), 1, prob = c(1/3, 1/3, 1/3))
          if(dpbase == "100"){
            bases <- 100
          }
          else if(dpbase == "010"){
            bases <- 010
          }
          else{
            bases <- 001
          }
        }
        else if (dp == "DP" & outs == 0 & bases == 111){
          outs <- 2
          #print(dp)
          #These probabilities are infered
          dpbase <- sample(c("101", "110", "011", "001", "010", "100"), 1, prob = c(1/6, 3/12, 1/3, 1/12, 1/12, 1/12))
          if(dpbase == "101"){
            bases <- 101
          }
          else if(dpbase == "110"){
            bases <- 110
          }
          else if(dpbase == "011"){
            bases <- 011
          }
          else if(dpbase == "001"){
            bases <- 001
            runs[i] <- runs[i]+1
          }
          else if(dpbase == "010"){
            bases <- 010
            runs[i] <- runs[i]+1
          }
          else if(dpbase == "100"){
            bases <- 100
            runs[i] <- runs[i]+1
          }
        }
        else if (dp == "DP" & outs == 1 & bases == 111){
          #print(dp)
          outs <- 3
        }
        else{
          #Sacrifice plays, couldn't find documentation about probability
          #so infered probs
          sac <- sample(c("sac", "no_sac"), 1, prob = c(0.075,0.925))
          if(sac == "sac" & outs < 2 & bases == 100){
            #print(sac)
            runs[i] <- runs[i] + 1
            bases <- 000
            outs <- outs + 1
          }
          else if(sac == "sac" & outs < 2 & bases == 010){
            #print(sac)
            bases <- 100
            outs <- outs + 1
          }
          else if(sac == "sac" & outs < 2 & bases == 001){
            #print(sac)
            bases <- 010
            outs <- outs + 1
          }
          else if(sac == "sac" & outs < 2 & bases == 110){
            #print(sac)
            runs[i] <- runs[i] + 1
            bases <- 100
            outs <- outs + 1
          }
          else if(sac == "sac" & outs < 2 & bases == 101){
            #print(sac)
            runs[i] <- runs[i] + 1
            bases <- 010
            outs <- outs + 1
          }
          else if(sac == "sac" & outs < 2 & bases == 011){
            #print(sac)
            bases <- 110
            outs <- outs + 1
          }
          else if(sac == "sac" & outs < 2 & bases == 111){
            #print(sac)
            runs[i] <- runs[i] + 1
            bases <- 110
            outs <- outs + 1
          }
          else{
            #print("Out")
            #print(as.character(bases))
            outs <- outs + 1
          }
        }
      }
    }
  }
  #return(paste0("runs = ", runs))
  runs
}


inning(1, 000, 0)

#Creating dataframe to track how the simulation compares to the prospectus
#in runs expected given the different game scenarios
GameScenDF <- rbind(rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6))
colnames(GameScenDF) <- c("0 - Pros", "0 - Sim", "1 - Pros", "1 - Sim", "2 - Pros", "2- Sim")
row.names(GameScenDF) <- c("000", "001", "010","100","011", "101", "110", "111")
#Function to run an inning given the conditions
gameScen <- function(n,b,o){
  sim <- inning(n,b,o)
  expRun <- sum(sim)/n
  return(expRun)
}
#Prospectus
GameScenDF[,1] <- c(0.5439, 0.9345, 1.1465, 1.3685, 1.4371, 1.7591, 1.9711, 2.3618)
GameScenDF[,3] <- c(0.2983, 0.5641, 0.7134, 0.9528, 0.9792, 1.2186, 1.3679, 1.6337)
GameScenDF[,5] <- c(0.1147, 0.2422, 0.3391, 0.3907, 0.4666, 0.5182, 0.6151, 0.7426)
#Simulating each of the 24 scenarios 10 million times
GameScenDF[1,2] <- gameScen(100000, 000, 0)
GameScenDF[2,2] <- gameScen(100000, 001, 0)
GameScenDF[3,2] <- gameScen(100000, 010, 0)
GameScenDF[4,2] <- gameScen(100000, 100, 0)
GameScenDF[5,2] <- gameScen(100000, 011, 0)
GameScenDF[6,2] <- gameScen(100000, 101, 0)
GameScenDF[7,2] <- gameScen(100000, 110, 0)
GameScenDF[8,2] <- gameScen(100000, 111, 0)
GameScenDF[1,4] <- gameScen(100000, 000, 1)
GameScenDF[2,4] <- gameScen(100000, 001, 1)
GameScenDF[3,4] <- gameScen(100000, 010, 1)
GameScenDF[4,4] <- gameScen(100000, 100, 1)
GameScenDF[5,4] <- gameScen(100000, 011, 1)
GameScenDF[6,4] <- gameScen(100000, 101, 1)
GameScenDF[7,4] <- gameScen(100000, 110, 1)
GameScenDF[8,4] <- gameScen(100000, 111, 1)
GameScenDF[1,6] <- gameScen(100000, 000, 2)
GameScenDF[2,6] <- gameScen(100000, 001, 2)
GameScenDF[3,6] <- gameScen(100000, 010, 2)
GameScenDF[4,6] <- gameScen(100000, 100, 2)
GameScenDF[5,6] <- gameScen(100000, 011, 2)
GameScenDF[6,6] <- gameScen(100000, 101, 2)
GameScenDF[7,6] <- gameScen(100000, 110, 2)
GameScenDF[8,6] <- gameScen(100000, 111, 2)

#Viewing how good of a fit the simulations were
prospectus <- c(GameScenDF[,1], GameScenDF[,3], GameScenDF[,5])
sim <- c(GameScenDF[,2],GameScenDF[,4],GameScenDF[,6])
prosim<- lm(sim ~ prospectus)
summary(prosim)

cor(prospectus, sim)
#Correlation = 0.992 (good)
prosim$coefficients
#Slope = 0.863 (should be closer to 1)
#Intercept = 0.088 (should be closer to 0)

#Ideal line in red
#Prosectus vs sim line in Blue
library(ggplot2)
ggplot(prosim, aes(x = prospectus, y = sim)) + geom_point() +
  geom_smooth(method = 'lm') + geom_abline(slope = 1, color = "red") +
  title(xlab = "Prospectus Expected Runs", ylab = "sim Expected Runs") 
  

library(kableExtra)
GameScenDF%>%
  kable(.)%>%
  kable_styling(.)
