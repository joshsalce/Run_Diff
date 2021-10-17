install.packages("retrosheet")
require(retrosheet)
library(ggplot2)
library(tidyverse)

cont_RD = function(team, year) {
  d4t4 <- getRetrosheet("game", year)
  subject <- subset(d4t4, d4t4$HmTm == team | d4t4$VisTm == team)
  
  v = c()
  
  for (i in 1:nrow(subject)) {
    run_diff <- initial_rd + ifelse(subject$HmTm[i] == team, subject$HmRuns[i] - subject$VisRuns[i], subject$VisRuns[i] - subject$HmRuns[i]) 
    v = c(v, run_diff)
    initial_rd <- run_diff
  }
  print(v)
  
  games = c(1:length(v)) #if the amount of games played were greater than or less than 162
  ggplot(subject, aes(games, v)) +
    ylim (-300, 300) +
    xlab("Games") +
    ylab("Run Differential") +
    geom_line()
}

cont_RD("LAN", 2019)


