source("Fix_Pitching.R")
#Inning data in format of home_team, home_score, vis_team, vis_score, inning, 
#topbot, outs, runners_pos, home_pitch, vis_pitch
#n = number of simulations

inning <- function(n, ibases, iouts){
  runs <- rep(NA, n)
  for(i in 1:n){
  #  inningNum <- 1
  #  return(inningNum)
  #  for(inning in 1:n){
      #inning <- inning + 1
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

#Creating dataframe to track how the simulation compares to the prospectus
#in runs expected given the different game scenarios
gameScenDF <- rbind(rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6))
colnames(gameScenDF) <- c("0 - Pros", "0 - Model", "1 - Pros", "1 - Model", "2 - Pros", "2- Model")
row.names(gameScenDF) <- c("000", "001", "010","100","011", "101", "110", "111")
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
GameScenDF[1,2] <- gameScen(10000000, 000, 0)
GameScenDF[2,2] <- gameScen(10000000, 001, 0)
GameScenDF[3,2] <- gameScen(10000000, 010, 0)
GameScenDF[4,2] <- gameScen(10000000, 100, 0)
GameScenDF[5,2] <- gameScen(10000000, 011, 0)
GameScenDF[6,2] <- gameScen(10000000, 101, 0)
GameScenDF[7,2] <- gameScen(10000000, 110, 0)
GameScenDF[8,2] <- gameScen(10000000, 111, 0)
GameScenDF[1,4] <- gameScen(10000000, 000, 1)
GameScenDF[2,4] <- gameScen(10000000, 001, 1)
GameScenDF[3,4] <- gameScen(10000000, 010, 1)
GameScenDF[4,4] <- gameScen(10000000, 100, 1)
GameScenDF[5,4] <- gameScen(10000000, 011, 1)
GameScenDF[6,4] <- gameScen(10000000, 101, 1)
GameScenDF[7,4] <- gameScen(10000000, 110, 1)
GameScenDF[8,4] <- gameScen(10000000, 111, 1)
GameScenDF[1,6] <- gameScen(10000000, 000, 2)
GameScenDF[2,6] <- gameScen(10000000, 001, 2)
GameScenDF[3,6] <- gameScen(10000000, 010, 2)
GameScenDF[4,6] <- gameScen(10000000, 100, 2)
GameScenDF[5,6] <- gameScen(10000000, 011, 2)
GameScenDF[6,6] <- gameScen(10000000, 101, 2)
GameScenDF[7,6] <- gameScen(10000000, 110, 2)
GameScenDF[8,6] <- gameScen(10000000, 111, 2)

#Viewing how good of a fit the model 
prospectus <- c(GameScenDF[,1], GameScenDF[,3], GameScenDF[,5])
model <- c(GameScenDF[,2],GameScenDF[,4],GameScenDF[,6])
prosmodel<- lm(model ~ prospectus)
summary(prosmodel)

cor(prospectus, model)
#Correlation = 0.987 (good)
prosmodel$coefficients
#Slope = 0.850 (should be closer to 1)
#Intercept = 0.089 (should be closer to 0)

#Ideal line in red
#Prosectus vs Model line in Blue
ggplot(prosmodel, aes(x = prospectus, y = model)) + geom_point() +
  geom_smooth(method = 'lm') + title(xlab = "Prospectus Expected Runs", ylab = "Model Expected Runs") +
  geom_abline(slope = 1, color = "red")


