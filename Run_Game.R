#Testing how to get desired pitcher vs batter matchup probabilities
offense <- OAK_Batting[rep(seq_len(nrow(OAK_Batting)), each = 5), ]
defense <- do.call("rbind", replicate(10, TBR_Pitching, simplify = FALSE))
offense <- offense %>% mutate(., Pitcher = rep(TBR_Pitching$Name, times = 10)) %>%
  mutate(., pNum = rep(1:5, times = 10)) %>%
  arrange(., Pitcher) %>%
  mutate(., p1B = (((offense$p1B)*(defense$p1B))/(AL_AVG$p1B))/(((((offense$p1B)*(defense$p1B))/(AL_AVG$p1B)))+(((1-offense$p1B)*(1-defense$p1B))/(1-AL_AVG$p1B)))) %>%
  mutate(., p2B = (((offense$p2B)*(defense$p2B))/(AL_AVG$p2B))/(((((offense$p2B)*(defense$p2B))/(AL_AVG$p2B)))+(((1-offense$p2B)*(1-defense$p2B))/(1-AL_AVG$p2B)))) %>%
  mutate(., p3B = (((offense$p3B)*(defense$p3B))/(AL_AVG$p3B))/(((((offense$p3B)*(defense$p3B))/(AL_AVG$p3B)))+(((1-offense$p3B)*(1-defense$p3B))/(1-AL_AVG$p3B)))) %>%
  mutate(., pHR = (((offense$pHR)*(defense$pHR))/(AL_AVG$pHR))/(((((offense$pHR)*(defense$pHR))/(AL_AVG$pHR)))+(((1-offense$pHR)*(1-defense$pHR))/(1-AL_AVG$pHR)))) %>%
  mutate(., pBB = (((offense$pBB)*(defense$pBB))/(AL_AVG$pBB))/(((((offense$pBB)*(defense$pBB))/(AL_AVG$pBB)))+(((1-offense$pBB)*(1-defense$pBB))/(1-AL_AVG$pBB))))

offenseS <- filter(offense, pNum == 1)
sample(c("Out","1B", "2B", "3B", "HR", "BB"), 1, 
       prob = c(1-offenseS$p1B[1] - offenseS$p2B[1] - offenseS$p3B[1] - offenseS$pHR[1] - offenseS$pBB[1], 
                offenseS$p1B[1], offenseS$p2B[1], offenseS$p3B[1], offenseS$pHR[1], offenseS$pBB[1]))

game <- function(batting, pitching, pitcher, league){
  offense <- batting[rep(seq_len(nrow(batting)), each = 5), ]
  defense <- do.call("rbind", replicate(10, pitching, simplify = FALSE))
  #Establishing probailities of hits for pitcher vs batter matchups
  offense <- offense %>% mutate(., Pitcher = rep(pitching$Name, times = 10)) %>%
    mutate(., pNum = rep(1:5, times = 10)) %>%
    arrange(., Pitcher) %>%
    mutate(., p1B = (((offense$p1B)*(defense$p1B))/(AL_AVG$p1B))/(((((offense$p1B)*(defense$p1B))/(AL_AVG$p1B)))+(((1-offense$p1B)*(1-defense$p1B))/(1-AL_AVG$p1B)))) %>%
    mutate(., p2B = (((offense$p2B)*(defense$p2B))/(AL_AVG$p2B))/(((((offense$p2B)*(defense$p2B))/(AL_AVG$p2B)))+(((1-offense$p2B)*(1-defense$p2B))/(1-AL_AVG$p2B)))) %>%
    mutate(., p3B = (((offense$p3B)*(defense$p3B))/(AL_AVG$p3B))/(((((offense$p3B)*(defense$p3B))/(AL_AVG$p3B)))+(((1-offense$p3B)*(1-defense$p3B))/(1-AL_AVG$p3B)))) %>%
    mutate(., pHR = (((offense$pHR)*(defense$pHR))/(AL_AVG$pHR))/(((((offense$pHR)*(defense$pHR))/(AL_AVG$pHR)))+(((1-offense$pHR)*(1-defense$pHR))/(1-AL_AVG$pHR)))) %>%
    mutate(., pBB = (((offense$pBB)*(defense$pBB))/(AL_AVG$pBB))/(((((offense$pBB)*(defense$pBB))/(AL_AVG$pBB)))+(((1-offense$pBB)*(1-defense$pBB))/(1-AL_AVG$pBB))))
  inning <- 0
  runs <- 0
  batter <- 1
  while(inning < 7){
    #Using only selected starting pitcher in the game
    offenseS <- filter(offense, pNum == pitcher)
    #Determining whether DH is in play or not
    if(league == "NL"){
      offenseS <- offenseS[c(1:8,10),]
    }
    else{
      offenseS <- offenseS[c(1:9),]
    }
    inning <- inning + 1      
    ##print(paste0("inning = ", inning))
    #Initializing values that are tracked throughout the inning
    play <- 0
    outcome <- rep()
    #Resetting baserunners
    bases <- 000
    outs <- 0
    while(outs < 3){
      #Resetting batting order back to 1
      if(batter > nrow(offenseS)){
        batter <- 1
      }
      #print(paste0("outs = ", outs))
      #Simulating the odds of a type of hit, walk or out
      play <- play + 1
      outcome <- c(outcome, NA)
      outcome[play] <-  sample(c("Out","1B", "2B", "3B", "HR", "BB"), 1, 
                               prob = c(1-offenseS$p1B[batter] - offenseS$p2B[batter] - offenseS$p3B[batter] - offenseS$pHR[batter] - offenseS$pBB[batter], 
                                        offenseS$p1B[batter], offenseS$p2B[batter], offenseS$p3B[batter], offenseS$pHR[batter], offenseS$pBB[batter]))
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
          runs <- runs + 1
        }
        else if(bases == 100){
          bases <- 001
          runs <- runs + 1
        }
        else if(bases == 011){
          bases <- 101
          runs <- runs + 1
        }
        else if(bases == 101){
          bases <- 101
          runs <- runs + 1
        }
        else if(bases == 110){
          bases <- 001
          runs <- runs + 2
        }
        else if(bases == 111){
          bases <- 001
          runs <- runs + 2
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
          runs <- runs + 1
        }
        else if(bases == 010){
          bases <- 010
          runs <- runs + 1
        }
        else if(bases == 100){
          bases <- 010
          runs <- runs + 1
        }
        else if(bases == 011){
          bases <- 010
          runs <- runs + 2
        }
        else if(bases == 101){
          bases <- 010
          runs <- runs + 2
        }
        else if(bases == 110){
          bases <- 010
          runs <- runs + 2
        }
        else if(bases == 111){
          bases <- 010
          runs <- runs + 3
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
          runs <- runs + 1
        }
        else if(bases == 010){
          bases <- 100
          runs <- runs + 1
        }
        else if(bases == 100){
          bases <- 100
          runs <- runs + 1
        }
        else if(bases == 011){
          bases <- 100
          runs <- runs + 2
        }
        else if(bases == 101){
          bases <- 100
          runs <- runs + 2
        }
        else if(bases == 110){
          bases <- 100
          runs <- runs + 2
        }
        else if(bases == 111){
          bases <- 100
          runs <- runs + 3
        }
        #print(as.character(bases))
      }
      else if(outcome[play] == "HR"){
        #print("Home run")
        if(bases == 000){
          runs <- runs + 1
        }
        else if(bases == 001){
          bases <- 000
          runs <- runs + 2
        }
        else if(bases == 010){
          bases <- 000
          runs <- runs + 2
        }
        else if(bases == 100){
          bases <- 000
          runs <- runs + 2
        }
        else if(bases == 011){
          bases <- 000
          runs <- runs + 3
        }
        else if(bases == 101){
          bases <- 000
          runs <- runs + 3
        }
        else if(bases == 110){
          bases <- 000
          runs <- runs + 3
        }
        else if(bases == 111){
          bases <- 000
          runs <- runs + 4
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
          runs <- runs + 1
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
      batter <- batter + 1
    }
  }
  while(inning >= 7 & inning < 10){
    #Using only closing pitcher
    offenseC <- filter(offense, Pitcher == "Reliever")
    #Determining whether DH is in play or not
    if(league == "NL"){
      offenseC <- offenseC[c(1:8,10),]
    }
    else{
      offenseC <- offenseC[c(1:9),]
    }
    inning <- inning + 1      
    #print(paste0("inning = ", inning))
    #Initializing values that are tracked throughout the inning
    play <- 0
    outcome <- rep()
    #Resetting baserunners
    bases <- 000
    outs <- 0
    while(outs < 3){
      #Resetting batting order back to 1
      if(batter > nrow(offenseC)){
        batter <- 1
      }
      #print(paste0("runs = ", runs))
      #print(paste0("outs = ", outs))
      #print(offenseC$Name[batter])
      #Simulating the odds of a type of hit, walk or out
      play <- play + 1
      outcome <- c(outcome, NA)
      outcome[play] <-  sample(c("Out","1B", "2B", "3B", "HR", "BB"), 1, 
                               prob = c(1-offenseC$p1B[batter] - offenseC$p2B[batter] - offenseC$p3B[batter] - offenseC$pHR[batter] - offenseC$pBB[batter], 
                                        offenseC$p1B[batter], offenseC$p2B[batter], offenseC$p3B[batter], offenseC$pHR[batter], offenseC$pBB[batter]))
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
          runs <- runs + 1
        }
        else if(bases == 100){
          bases <- 001
          runs <- runs + 1
        }
        else if(bases == 011){
          bases <- 101
          runs <- runs + 1
        }
        else if(bases == 101){
          bases <- 101
          runs <- runs + 1
        }
        else if(bases == 110){
          bases <- 001
          runs <- runs + 2
        }
        else if(bases == 111){
          bases <- 001
          runs <- runs + 2
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
          runs <- runs + 1
        }
        else if(bases == 010){
          bases <- 010
          runs <- runs + 1
        }
        else if(bases == 100){
          bases <- 010
          runs <- runs + 1
        }
        else if(bases == 011){
          bases <- 010
          runs <- runs + 2
        }
        else if(bases == 101){
          bases <- 010
          runs <- runs + 2
        }
        else if(bases == 110){
          bases <- 010
          runs <- runs + 2
        }
        else if(bases == 111){
          bases <- 010
          runs <- runs + 3
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
          runs <- runs + 1
        }
        else if(bases == 010){
          bases <- 100
          runs <- runs + 1
        }
        else if(bases == 100){
          bases <- 100
          runs <- runs + 1
        }
        else if(bases == 011){
          bases <- 100
          runs <- runs + 2
        }
        else if(bases == 101){
          bases <- 100
          runs <- runs + 2
        }
        else if(bases == 110){
          bases <- 100
          runs <- runs + 2
        }
        else if(bases == 111){
          bases <- 100
          runs <- runs + 3
        }
        #print(as.character(bases))
      }
      else if(outcome[play] == "HR"){
        #print("Home run")
        if(bases == 000){
          runs <- runs + 1
        }
        else if(bases == 001){
          bases <- 000
          runs <- runs + 2
        }
        else if(bases == 010){
          bases <- 000
          runs <- runs + 2
        }
        else if(bases == 100){
          bases <- 000
          runs <- runs + 2
        }
        else if(bases == 011){
          bases <- 000
          runs <- runs + 3
        }
        else if(bases == 101){
          bases <- 000
          runs <- runs + 3
        }
        else if(bases == 110){
          bases <- 000
          runs <- runs + 3
        }
        else if(bases == 111){
          bases <- 000
          runs <- runs + 4
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
          runs <- runs + 1
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
      batter <- batter + 1
    }
  }
  return(runs)
}


