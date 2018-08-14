# Copy madmR to Desktop
setwd("C:\\Users\\1517766115.CIV\\Desktop")
library(ggplot2)
load("madmR/data/maut_dm.rda")
load("madmR/data/topsis_dm.rda")
source("madmR/R/Algorithms.R")
source("madmR/R/Algorithms.R")
source("madmR/R/DB_Globals.R")
source("madmR/R/FileIO.R")
source("madmR/R/Sensitivity.R")
maut_dm

results <- TOPSIS(maut_dm)
results
sens <- sensitivity(maut_dm)
sens

