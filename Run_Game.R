#Testing how to get desired pitcher vs batter matchup probabilities
offense <- OAK_Batting[rep(seq_len(nrow(OAK_Batting)), each = 5), ]
defense <- do.call("rbind", replicate(10, TBR_Pitching, simplify = FALSE))
offense <- offense %>% mutate(., Pitcher = rep(TBR_Pitching$Name, times = 10)) %>%
  mutate(., p1B = (((offense$p1B)*(defense$p1B))/(AL_AVG$p1B))/(((((offense$p1B)*(defense$p1B))/(AL_AVG$p1B)))+(((1-offense$p1B)*(1-defense$p1B))/(1-AL_AVG$p1B)))) %>%
  mutate(., p2B = (((offense$p2B)*(defense$p2B))/(AL_AVG$p2B))/(((((offense$p2B)*(defense$p2B))/(AL_AVG$p2B)))+(((1-offense$p2B)*(1-defense$p2B))/(1-AL_AVG$p2B)))) %>%
  mutate(., p3B = (((offense$p3B)*(defense$p3B))/(AL_AVG$p3B))/(((((offense$p3B)*(defense$p3B))/(AL_AVG$p3B)))+(((1-offense$p3B)*(1-defense$p3B))/(1-AL_AVG$p3B)))) %>%
  mutate(., pHR = (((offense$pHR)*(defense$pHR))/(AL_AVG$pHR))/(((((offense$pHR)*(defense$pHR))/(AL_AVG$pHR)))+(((1-offense$pHR)*(1-defense$pHR))/(1-AL_AVG$pHR)))) %>%
  mutate(., pBB = (((offense$pBB)*(defense$pBB))/(AL_AVG$pBB))/(((((offense$pBB)*(defense$pBB))/(AL_AVG$pBB)))+(((1-offense$pBB)*(1-defense$pBB))/(1-AL_AVG$pBB))))

game <- function(n, batting, pitching){
  offense <- batting[rep(seq_len(nrow(batting)), each = 5), ]
  defense <- do.call("rbind", replicate(10, TBR_Pitching, simplify = FALSE))
  offense <- offense %>% mutate(., Pitcher = rep(TBR_Pitching$Name, times = 10)) %>%
    mutate(., p1B = (((offense$p1B)*(defense$p1B))/(AL_AVG$p1B))/(((((offense$p1B)*(defense$p1B))/(AL_AVG$p1B)))+(((1-offense$p1B)*(1-defense$p1B))/(1-AL_AVG$p1B)))) %>%
    mutate(., p2B = (((offense$p2B)*(defense$p2B))/(AL_AVG$p2B))/(((((offense$p2B)*(defense$p2B))/(AL_AVG$p2B)))+(((1-offense$p2B)*(1-defense$p2B))/(1-AL_AVG$p2B)))) %>%
    mutate(., p3B = (((offense$p3B)*(defense$p3B))/(AL_AVG$p3B))/(((((offense$p3B)*(defense$p3B))/(AL_AVG$p3B)))+(((1-offense$p3B)*(1-defense$p3B))/(1-AL_AVG$p3B)))) %>%
    mutate(., pHR = (((offense$pHR)*(defense$pHR))/(AL_AVG$pHR))/(((((offense$pHR)*(defense$pHR))/(AL_AVG$pHR)))+(((1-offense$pHR)*(1-defense$pHR))/(1-AL_AVG$pHR)))) %>%
    mutate(., pBB = (((offense$pBB)*(defense$pBB))/(AL_AVG$pBB))/(((((offense$pBB)*(defense$pBB))/(AL_AVG$pBB)))+(((1-offense$pBB)*(1-defense$pBB))/(1-AL_AVG$pBB))))
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