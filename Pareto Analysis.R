#Pareto Chart 80/20 Rule for "reasons for not using"

#Importing Library
library(qcc)

#Impoting Data
Cable_DTH <- read.csv(file.choose())
Cable_DTH

#Running Pareto..
Not_Using_Cable_DTH = Cable_DTH$Scores
names(Not_Using_Cable_DTH) = Cable_DTH$Reasons
x = pareto.chart(Not_Using_Cable_DTH, cumperc=seq(0,100, by=40),
    main = "Reasons for not using Cable/DTH", col = "plum4")


