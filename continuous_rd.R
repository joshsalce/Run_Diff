install.packages("retrosheet")
require(retrosheet)

cont_RD = function(team, year) {
  d4t4 <- getRetrosheet("game", year)
  subject <- subset(d4t4, d4t4$HmTm == team | d4t4$VisTm == team)
  
  run_diff = 0
  initial_rd = ifelse(subject$HmTm[1] == team,  subject$HmRuns[1] - subject$VisRuns[1], subject$VisRuns[1] - subject$HmRuns[1])
  v = c(initial_rd)
  
  for (i in 2:nrow(subject)) {
    run_diff <- initial_rd + ifelse(subject$HmTm[i] == team, subject$HmRuns[i] - subject$VisRuns[i], subject$VisRuns[i] - subject$HmRuns[i]) 
    v = c(v, run_diff)
    initial_rd <- run_diff
  }

  games = c(1:length(v)) #for the occasional Game 163
  plot(games, v, xlab="Games", ylab="Run Differential", cex = 0.45, pch = 16)
}

cont_RD()


