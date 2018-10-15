# Copy madmR to Desktop

setwd("M:\\Training and PD\\R (Statistical Computing)\\Libraries\\madmR")
library(ggplot2)
load("data/maut_dm.rda")
load("data/topsis_dm.rda")
load("data/ahp_dm.rda")
load("data/roc_dm.rda")
source("R/RankingAlgorithms.R")
source("R/WeightingAlgorithms.R")
source("R/DB_Globals.R")
source("R/FileIO.R")
source("R/Sensitivity.R")

maut_dm
topsis_dm
AHP_Example
MG


my_new_dm <- read.data.matrix("test.csv", header=TRUE)
my_new_dm

results <- TOPSIS(topsis_dm)
results$Results
results <- MAUT(maut_dm)
results$Results
sens <- sensitivity(maut_dm)
sens


AHP_Weight(AHP_Example, "Trail", 4)

ROC_Weight(MG, 4)
ROC_Weight(S_T[,c(1,2)], 6)

# See ROC_Hierarchy_Example.ppt for more info 

