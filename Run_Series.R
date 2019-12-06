source("Run_Game.R")

AL <- c("HOU", "MIN", "NYY", "OAK", "TB")
NL <- c("ATL", "LAD", "MIL", "STL", "WSN")


#Wild Card
#Milwaukee at Washington
#Tampa Bay at Oakland
wildcard <- function(n, home, away){
  results <- cbind(rep(0, n), rep(0, n), rep(NA, n))
  colnames(results) <- c(away, home, "Winner")
  league <- NA
  if(isTRUE(home %in% AL)){
    league <- "AL"
  }else{
    league <- "NL"
  }
  for(i in 1:n){
    #Away on Offense
    print(paste0("Running game: ", i, " of ", n))
    abatting <- get(paste0(away, "_Batting"))
    hpitching <- get(paste0(home, "_Pitching"))
    #Ace will start in Wild Card game
    awayOff <- game(abatting, hpitching, 1, league)
    results[i,1] <- awayOff
    #Home on Offense
    hbatting <- get(paste0(home, "_Batting"))
    apitching <- get(paste0(away, "_Pitching"))
    homeOff <- game(hbatting, apitching, 1, league)
    results[i,2] <- homeOff
    if(homeOff > awayOff){
      results[i,3] <- home
    } else if (homeOff == awayOff){
      #This will easily simulate extra innings, as the simulation gets too complex to take this into account.
      #Accoring to https://www.baseball-reference.com/blog/archives/155.html,
      #home teams win in extra innings 53.1% of the time. This is almost a coin flip, but it is fair to 
      #assume there is a bit of home field advantage in the postseason. This is a simple and effective way to
      #decide ties after 9 innings.
      extraInn <- sample(c(home, away), 1, prob = c(.531, .469))
      results[i,3] <- extraInn
    } else{
      results[i,3] <- away
    }
  }
  print(paste0(home, " is predicted to have a ", (sum(results[,3] == home)/n)*100, "% chance of winning the Wild Card game."))
  return(results)
}
#Simulating each game 100,000 times
nlWC <- wildcard(100000, "WSN", "MIL")
alWC <- wildcard(100000, "OAK", "TBR")


#Divisional Series
divisional <- function(n, home, away){
  results <- cbind(rep(0, n), rep(0, n), rep(NA, n))
  colnames(results) <- c(away, home, "Series Winner")
  #Establishing which league this divisional series is in
  league <- NA
  if(isTRUE(home %in% AL)){
    league <- "AL"
  }else{
    league <- "NL"
  }
  for(i in 1:n){
    #Initializing series info
    print(paste0("Running series: ", i, " of ", n))
    awins <- 0
    hwins <- 0
    pNum <- 1
    while(awins < 3 && hwins < 3) {
      if(pNum > 4){
        pNum <- 1
      }
      #Away on Offense
      abatting <- get(paste0(away, "_Batting"))
      hpitching <- get(paste0(home, "_Pitching"))
      awayOff <- game(abatting, hpitching, pNum, league)
      #Home on Offense
      hbatting <- get(paste0(home, "_Batting"))
      apitching <- get(paste0(away, "_Pitching"))
      homeOff <- game(hbatting, apitching, pNum, league)
      if(homeOff > awayOff){
        hwins <- hwins + 1
      } else if (homeOff == awayOff){
        #This will easily simulate extra innings, as the simulation gets too complex to take this into account.
        #Accoring to https://www.baseball-reference.com/blog/archives/155.html,
        #home teams win in extra innings 53.1% of the time. This is almost a coin flip, but it is fair to 
        #assume there is a bit of home field advantage in the postseason. This is a simple and effective way to
        #decide ties after 9 innings.
        
        #This case determines if the games are hosted by the away team of the first game (the lower seed)
        if(hwins + awins == 3 | hwins + awins == 4){
          extraInn <- sample(c(away, home), 1, prob = c(.531, .469))
          if(extraInn == home){
            hwins <- hwins + 1
          } else{
            awins <- awins + 1
          }
        } else{ #This case is whether the series is in games 1, 2 or 5. In this case, the home team stays the same
          extraInn <- sample(c(home, away), 1, prob = c(.531, .469))
          if(extraInn == home){
            hwins <- hwins + 1
          } else{
            awins <- awins + 1
          }
        }
        
      } else{
        awins <- awins + 1
      }
      pNum <- pNum + 1
    }
    #Reporting series results
    results[i,1] <- awins
    results[i,2] <- hwins
    if(hwins == 3){
      results[i,3] <- home
    } else{
      results[i,3] <- away
    }
  }
  print(paste0(home, " is predicted to have a ", (sum(results[,3] == home)/n)*100, "% chance of winning the series."))
  return(results)
}
#Simulating each series 100,000 times
nlDS1 <- divisional(100000, "ATL", "STL")
nlDS2 <- divisional(100000, "LAD", "MIL")
nlDS3 <- divisional(100000, "LAD", "WSN")
alDS1 <- divisional(100000, "NYY", "MIN")
alDS2 <- divisional(100000, "HOU", "OAK")
alDS3 <- divisional(100000, "HOU", "TBR")

#Championship Series
championship <- function(n, home, away){
  results <- cbind(rep(0, n), rep(0, n), rep(NA, n))
  colnames(results) <- c(away, home, "Series Winner")
  #Establishing which league this championship series is in
  league <- NA
  if(isTRUE(home %in% AL)){
    league <- "AL"
  }else{
    league <- "NL"
  }
  for(i in 1:n){
    #Initializing series info
    print(paste0("Running series: ", i, " of ", n))
    awins <- 0
    hwins <- 0
    pNum <- 1
    while(awins < 4 && hwins < 4) {
      #Changing pitchers each game
      if(pNum > 4){
        pNum <- 1
      }
      #Away on Offense
      abatting <- get(paste0(away, "_Batting"))
      hpitching <- get(paste0(home, "_Pitching"))
      awayOff <- game(abatting, hpitching, pNum, league)
      #Home on Offense
      hbatting <- get(paste0(home, "_Batting"))
      apitching <- get(paste0(away, "_Pitching"))
      homeOff <- game(hbatting, apitching, pNum, league)
      if(homeOff > awayOff){
        hwins <- hwins + 1
      } 
      else if (homeOff == awayOff){
        #This will easily simulate extra innings, as the simulation gets too complex to take this into account.
        #Accoring to https://www.baseball-reference.com/blog/archives/155.html,
        #home teams win in extra innings 53.1% of the time. This is almost a coin flip, but it is fair to 
        #assume there is a bit of home field advantage in the postseason. This is a simple and effective way to
        #decide ties after 9 innings.
        
        #This case determines if the games are hosted by the away team of the first game (the lower seed)
        if(hwins + awins == 3 | hwins + awins == 4 | hwins + awins == 5){
          extraInn <- sample(c(away, home), 1, prob = c(.531, .469))
          if(extraInn == home){
            hwins <- hwins + 1
          } else{
            awins <- awins + 1
          }
        } else{ #This case is whether the series is in games 1, 2, 6 or 7. In this case, the home team stays the same
          extraInn <- sample(c(home, away), 1, prob = c(.531, .469))
          if(extraInn == home){
            hwins <- hwins + 1
          } else{
            awins <- awins + 1
          }
        }
      } else{
        awins <- awins + 1
      }
      pNum <- pNum + 1
    }
    #Reporting series results
    results[i,1] <- awins
    results[i,2] <- hwins
    if(hwins == 4){
      results[i,3] <- home
    } else{
      results[i,3] <- away
    }
  }
  print(paste0(home, " is predicted to have a ", (sum(results[,3] == home)/n)*100, "% chance of winning the series."))
  results
}
#Simulating each series 100,000 times
nlCS1 <- championship(100000, "LAD", "STL")
nlCS2 <- championship(100000, "LAD", "ATL")
nlCS3 <- championship(100000, "ATL", "WSN")
nlCS4 <- championship(100000, "ATL", "MIL")
nlCS5 <- championship(100000, "STL", "MIL")
nlCS6 <- championship(100000, "STL", "WSN")
alCS1 <- championship(100000, "HOU", "MIN")
alCS2 <- championship(100000, "HOU", "NYY")
alCS3 <- championship(100000, "NYY", "TBR")
alCS4 <- championship(100000, "NYY", "OAK")
alCS5 <- championship(100000, "MIN", "OAK")
alCS6 <- championship(100000, "MIN", "TBR")


#World Series
worldseries <- function(n, home, away){
  results <- cbind(rep(0, n), rep(0, n), rep(NA, n))
  colnames(results) <- c(away, home, "Champion")
  for(i in 1:n){
    #Initializing series info
    print(paste0("Running series: ", i, " of ", n)
    awins <- 0
    hwins <- 0
    pNum <- 1
    #Establishing which league this championship series is in
    league <- NA
    if(isTRUE(home %in% AL && (hwins + awins == 3 | hwins + awins == 4 | hwins + awins == 5))){
      league <- "NL"
    }else if (isTRUE(home %in% AL && (hwins + awins == 1 | hwins + awins == 2 | hwins + awins == 6 | hwins + awins == 7))){
      league <- "AL"
    }else if (isTRUE(home %in% NL && (hwins + awins == 1 | hwins + awins == 2 | hwins + awins == 6 | hwins + awins == 7))){
      league <- "NL"
    } else{
      league <- "AL"
    }
    while(awins < 4 && hwins < 4) {
      #Changing pitchers each game
      if(pNum > 4){
        pNum <- 1
      }
      #Away on Offense
      abatting <- get(paste0(away, "_Batting"))
      hpitching <- get(paste0(home, "_Pitching"))
      awayOff <- game(abatting, hpitching, pNum, league)
      #Home on Offense
      hbatting <- get(paste0(home, "_Batting"))
      apitching <- get(paste0(away, "_Pitching"))
      homeOff <- game(hbatting, apitching, pNum, league)
      if(homeOff > awayOff){
        hwins <- hwins + 1
      } 
      else if (homeOff == awayOff){
        #This will easily simulate extra innings, as the simulation gets too complex to take this into account.
        #Accoring to https://www.baseball-reference.com/blog/archives/155.html,
        #home teams win in extra innings 53.1% of the time. This is almost a coin flip, but it is fair to 
        #assume there is a bit of home field advantage in the postseason. This is a simple and effective way to
        #decide ties after 9 innings.
        
        #This case determines if the games are hosted by the away team of the first game (the lower seed)
        if(hwins + awins == 3 | hwins + awins == 4 | hwins + awins == 5){
          extraInn <- sample(c(away, home), 1, prob = c(.531, .469))
          if(extraInn == home){
            hwins <- hwins + 1
          } else{
            awins <- awins + 1
          }
        } else{ #This case is whether the series is in games 1, 2, 6 or 7. In this case, the home team stays the same
          extraInn <- sample(c(home, away), 1, prob = c(.531, .469))
          if(extraInn == home){
            hwins <- hwins + 1
          } else{
            awins <- awins + 1
          }
        }
      } else{
        awins <- awins + 1
      }
      pNum <- pNum + 1
    }
    #Reporting series results
    results[i,1] <- awins
    results[i,2] <- hwins
    if(hwins == 4){
      results[i,3] <- home
    } else{
      results[i,3] <- away
    }
  }
  print(paste0(home, " is predicted to have a ", (sum(results[,3] == home)/n)*100, "% chance of winning the World Series."))
  results
}

#Simulating each series 100,000 times
WS1 <- worldseries(100000, "NYY", "MIL")
WS2 <- worldseries(100000, "HOU", "MIL")
WS3 <- worldseries(100000, "TBR", "STL")
WS4 <- worldseries(100000, "NYY", "STL")
WS5 <- worldseries(100000, "HOU", "STL")
WS6 <- worldseries(100000, "TBR", "MIL")
WS7 <- worldseries(100000, "TBR", "WSN")
WS8 <- worldseries(100000, "OAK", "STL")
WS9 <- worldseries(100000, "TBR", "ATL")
WS10 <- worldseries(100000, "ATL", "OAK")
WS11 <- worldseries(100000, "NYY", "WSN")
WS12 <- worldseries(100000, "OAK", "MIL")
WS13 <- worldseries(100000, "OAK", "WSN")
WS14 <- worldseries(100000, "MIN", "WSN")
WS15 <- worldseries(100000, "LAD", "TBR")
WS16 <- worldseries(100000, "MIN", "STL")
WS17 <- worldseries(100000, "NYY", "ATL")
WS18 <- worldseries(100000, "LAD", "OAK")
WS19 <- worldseries(100000, "MIN", "ATL")
WS20 <- worldseries(100000, "HOU", "ATL")
WS21 <- worldseries(100000, "LAD", "MIN")
WS22 <- worldseries(100000, "HOU", "WSN")
WS23 <- worldseries(100000, "HOU", "LAD")
WS24 <- worldseries(100000, "NYY", "LAD")
WS25 <- worldseries(100000, "MIN", "MIL")




