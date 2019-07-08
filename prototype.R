# Clear Workspace
rm(list=ls())

# Load Libraries

# Import Datasets
dfTrump <- read.csv("~/Documents/NLP/Trump.csv")
dfBush <- read.csv("~/Documents/NLP/Bush.csv")
dfReagan <- read.csv("~/Documents/NLP/Reagan.csv")

# Delete SYM from Bush
dfBush$SYM = NULL
dfBush$X = NULL
dfReagan$X = NULL
dfTrump$X = NULL



