source("Run_Game.R")

AL <- c("HOU", "MIN", "NYY", "OAK", "TB")
NL <- c("ATL", "LAD", "MIL", "STL", "WSN")

#Wild Card
#Milwaukee at Washington
#Tampa Bay at Oakland
series <- function(n, home, away){
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
  print(paste0(home, " is predicted to have a ", (sum(results[,3] == home)/n)*100, "% chance of winning the series."))
  return(results)
}
nlWC <- series(2000000, "WSN", "MIL")
alWC <- series(2000000, "OAK", "TBR")


n <- 10
results <- cbind(rep(0, n), rep(0, n), rep(NA, n))
away <- "MIL"
home <- "WSN"
colnames(results) <- c(away, home, "Winner")
league <- NA
if(isTRUE(home %in% AL)){
  league <- "AL"
}else{
  league <- "NL"
}
for(i in 1:n){
  #Away on Offense
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
results
