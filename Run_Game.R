offense <- 
test <- 

game <- function(n, offense, defense){
  offense <- as.data.frame(team1)
  defense <- as.data.frame(team2)  
  
  
  runs <- rep(NA, n)
  for(i in 1:n){
    inning <- 0
    runs[i] <- 0
    while(inning < 10){
      inning <- inning + 1
      #Initializing values that are tracked throughout the inning
      play <- 0
      outcome <- rep()
      bases <- 000
      while(outs < 3)
        print(paste0("outs = ", outs))
        #Simulating the odds of a type of hit, walk or out
        while (condition) {
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
    }
  }
  #return(paste0("runs = ", runs))
  runs
}