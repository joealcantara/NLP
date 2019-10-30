###--- SETUP ---###
# Clear Workspace
rm(list=ls())

# Source File
source("Functions.R")

# Options
options(scipen=999)

# Libraries
library(dplyr)
library(ggplot2)
library(mgcv)

###--- IMPORT AND PREPROCESS DATA ---###
# Load Data
dataBush = read.csv("~/NLP/Bush.csv")
dataReagan = read.csv("~/NLP/Reagan.csv")
dataTrump = read.csv("~/NLP/Trump.csv")

# Clean Data 
dataReagan = cleanData(dataReagan)
dataBush = cleanData(dataBush)
dataTrump = cleanData(dataTrump)

# Preprocess Data
dfReagan = preprocess(dataReagan)
dfBush = preprocess(dataBush)
dfTrump = preprocess(dataTrump)

# Generate a list of features we are interested in
# First only look at the features which are significant
subsetReagan = filter(dfReagan, hommel<0.05, BY<0.05, feature!='Days')
subsetBush = filter(dfBush, hommel<0.05, BY<0.05, feature!='Days')
subsetReagan = rbind(subsetReagan, filter(dfReagan, BY<0.05, hommel>0.05, feature!='Days'))
subsetBush = rbind(subsetBush, filter(dfBush, BY<0.05, hommel>0.05,  feature!='Days'))
# Note: Running this on Trump generates an empty set

# Then generate a vector of features 
bushFeatures = subsetBush$feature
reaganFeatures = subsetReagan$feature

for (var in 1:length(bushFeatures)){
  print(bushFeatures[var])
}

