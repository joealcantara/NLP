# Library (cocor)

# To do
# FWER and FDR plots in function
# Automate plots fot significant features

###--- SETUP ---###
# Clear Workspace
rm(list=ls())

# Options
options(scipen=999)

# Source File
source("Functions.R")

# Options
options(scipen=999)

# Libraries
library(dplyr)
library(ggplot2)
library(mgcv)
library(stats)
library(neuralnet)

###--- IMPORT AND PREPROCESS DATA ---###
# Load Data
dataBush = read.csv("Bush.csv")
dataReagan = read.csv("Reagan.csv")
dataTrump = read.csv("Trump.csv")

# Clean Data 
dataReagan = cleanData(dataReagan)
dataBush = cleanData(dataBush)
dataTrump = cleanData(dataTrump)

# Preprocess Data
dfReagan = preprocess(dataReagan)
dfBush = preprocess(dataBush)
dfTrump = preprocess(dataTrump)
dfBush = subset(dfBush, feature != 'SYM')

# Scale the Data
sdfReagan = scale(dataReagan[,2:ncol(dataReagan)])
sdfReagan = cbind(dataReagan[, 1], sdfReagan)
colnames(sdfReagan)[colnames(sdfReagan) == ''] <- 'Days'

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

###--- PLOTS ---###
# Reagan
XReagan = dfReagan$pvalue
YReagan = cbind(dfReagan$Bonferroni, dfReagan$BH, dfReagan$holm, dfReagan$hochberg,
                dfReagan$hommel, dfReagan$BY)
matplot(XReagan, YReagan, xlab = "Reagan Raw p-value", ylab = "Adjusted p-value", type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

# Bush
XBush = dfBush$pvalue
YBush = cbind(dfBush$Bonferroni, dfBush$BH, dfBush$holm, dfBush$hochberg,
              dfBush$hommel, dfBush$BY)
matplot(XBush, YBush, xlab = "Bush Raw p-value", ylab = "Adjusted p-value", type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

# Trump
XTrump = dfTrump$pvalue
YTrump = cbind(dfTrump$Bonferroni, dfTrump$BH, dfTrump$holm, dfTrump$hochberg,
               dfTrump$hommel, dfTrump$BY)
matplot(XTrump, YTrump, xlab = "Trump Raw p-value", ylab = "Adjusted p-value",
        type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

# Load Data
results = numeric(length(dataReagan))
totalResults = data.frame(feature = character (),
                          gamPRESS = numeric(),
                          lmPRESS = numeric())

# for (var in 1:length(reaganFeatures)){
#   tmp <- as.formula(paste0(reaganFeatures[var], ' ~ s(Days, bs = "gp")'))
#   tmp2 = as.formula(paste0(reaganFeatures[var],))
#   for (i in 1:(nrow(df))){
#     gamModel = gam(tmp, 
#                    data = df[-i, ], 
#                    family = Gamma(link = "log"), 
#                    method = "REML")
#     pred = predict.gam(gamModel, df[i, ], 
#                        type = "response")
#     results[i] = (as.numeric(pred) - tmp2) ^ 2
#     store = (as.numeric(pred) - tmp2) ^ 2
#   }
#   totalResults[i] = sum(store)
# }
# PPRON Feature
for (i in 1:(nrow(dataReagan))){
  gamModel = gam(ppron ~ s(Days, bs = "gp"), 
                 data = dataReagan[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, dataReagan[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - dataReagan$ppron[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(dataReagan))
for (i in 1:(nrow(dataReagan))){
  linearModel = lm(ppron ~ Days, data = dataReagan[-i, ])
  pred = predict.lm(linearModel, dataReagan[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - dataReagan$ppron[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)
sdfReagan = as.data.frame(sdfReagan)

# nnresults = numeric(nrow(dataReagan))
# for (i in 1:(nrow(sdfReagan))){
#   train = sdfReagan[-i, ]
#   test = sdfReagan[i,]
#   nn = neuralnet(ppron ~ Days, data = train, hidden=c(2,2))
#   pr.nn <- compute(nn, train)
#   pr.nn_ <- pr.nn$net.result*(max(train$ppron)-min(train$ppron))+min(train$ppron)
#   test.r <- (test$ppron)*(max(test$ppron)-min(test$ppron))+min(test$ppron)
#   nnresults[i] = (test.r -pr.nn_)^2
# }
# nnPress = (sum(nnresults))

# plot(nn)
# pr.nn <- compute(nn, df[i, ])
# pr.nn_ <- pr.nn$net.result*(max(testData$ppron)-min(testData$ppron))+min(testData$ppron)
# test.r <- (predData$ppron)*(max(predData$ppron)-min(predData$ppron))+min(predData$ppron)
# MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(predData)

vectortmp = data.frame("ppron",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

df = dataReagan


### Social Feature
for (i in 1:(nrow(df))){
  gamModel = gam(social ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$social[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(social ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$social[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("social",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Nouns Normalised Feature
for (i in 1:(nrow(df))){
  gamModel = gam(NounsNormalised ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$NounsNormalised[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(NounsNormalised ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$NounsNormalised[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("NounsNormalised",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# function. Feature
for (i in 1:(nrow(df))){
  gamModel = gam(function. ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$function.[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(function. ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$function.[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("function.",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# conj Feature
for (i in 1:(nrow(df))){
  gamModel = gam(conj ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$conj[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(conj ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$conj[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("conj",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# PronounsNormalised Feature
for (i in 1:(nrow(df))){
  gamModel = gam(PronounsNormalised ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$PronounsNormalised[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(PronounsNormalised ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$PronounsNormalised[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("PronounsNormalised",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)


# Analytic Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Analytic ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Analytic[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Analytic ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Analytic[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Analytic",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# pronoun Feature
for (i in 1:(nrow(df))){
  gamModel = gam(pronoun ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$pronoun[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(pronoun ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$pronoun[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("pronoun",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# NN Feature
for (i in 1:(nrow(df))){
  gamModel = gam(NN ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$NN[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(NN ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$NN[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("NN",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)


# male Feature
for (i in 1:(nrow(df))){
  gamModel = gam(male ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$male[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(male ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$male[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("male",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# UniqueWords Feature
for (i in 1:(nrow(df))){
  gamModel = gam(UniqueWords ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$UniqueWords[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(UniqueWords ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$UniqueWords[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("UniqueWords",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# WDT Feature
for (i in 1:(nrow(df))){
  gamModel = gam(WDT ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$WDT[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(WDT ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$WDT[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("WDT",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Nouns Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Nouns ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Nouns[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Nouns ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Nouns[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Nouns",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Nouns.100 Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Nouns.100 ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Nouns.100[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Nouns.100 ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Nouns.100[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Nouns/100",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# UniqueStems Feature
for (i in 1:(nrow(df))){
  gamModel = gam(UniqueStems ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$UniqueStems[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(UniqueStems ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$UniqueStems[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("UniqueStems",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# shehe Feature
for (i in 1:(nrow(df))){
  gamModel = gam(shehe ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$shehe[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(shehe ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$shehe[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("shehe",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# VBZ Feature
for (i in 1:(nrow(df))){
  gamModel = gam(VBZ ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$VBZ[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(VBZ ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$VBZ[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("VBZ",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# JJ Feature
for (i in 1:(nrow(df))){
  gamModel = gam(JJ ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$JJ[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(JJ ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$JJ[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("JJ",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# article Feature
for (i in 1:(nrow(df))){
  gamModel = gam(article ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$article[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(article ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$article[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("article",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Adjectives Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Adjectives ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Adjectives[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Adjectives ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Adjectives[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Adjectives",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Adjectives.100 Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Adjectives.100 ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Adjectives.100[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Adjectives.100 ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Adjectives.100[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Adjectives.100",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Dic Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Dic ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Dic[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Dic ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Dic[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Dic",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

test = totalResults

test = test %>%
  mutate_at(c(2,3), funs(c(scale(.))))

ttest = t.test(totalResults[,2], totalResults[,3], paired = TRUE)
indx <- totalResults[,2] < totalResults[,3]
binom.test(sum(indx), length(indx))

ttest

scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot +  geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm", color = "#CC0033", fill = '#CC0033', lty = 1) + geom_smooth(method="loess", color = "#3399FF", fill = '#3399FF' ,lty = 2) + theme_minimal() +
  ggtitle('Reagan - Use of personal Pronouns over time') + theme(plot.title = element_text(lineheight=.8, face="bold", family="URWGothic"))
ggsave('reaganppron.png')

scatter_plot <- ggplot(dataReagan, aes(Days, NounsNormalised))
scatter_plot +  geom_point() + labs(x = "Days", y = "NounsNormalised") + geom_smooth(method="lm", color = "#CC0033", fill = '#CC0033', lty = 1) + geom_smooth(method="loess", color = "#3399FF", fill = '#3399FF', lty = 2) + theme_minimal() +
  ggtitle('Reagan - Use of Nouns (Normalised) over time') + theme(plot.title = element_text(lineheight=.8, face="bold", family="URWGothic"))
ggsave('reagannouns.png')

scatter_plot <- ggplot(dataBush, aes(Days, ppron))
scatter_plot +  geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm", color = "#CC0033", fill = '#CC0033', lty = 1) + geom_smooth(method="loess", color = "#3399FF", fill = '#3399FF', lty = 2) + theme_minimal() +
  ggtitle('Bush - Use of personal Pronouns over time') + theme(plot.title = element_text(lineheight=.8, face="bold", family="URWGothic"))
ggsave('bushppron.png')

scatter_plot <- ggplot(dataBush, aes(Days, NounsNormalised))
scatter_plot +  geom_point() + labs(x = "Days", y = "NounsNormalised") + geom_smooth(method="lm", color = "#CC0033", fill = '#CC0033', lty = 1) + geom_smooth(method="loess", color = "#3399FF", fill = '#3399FF', lty = 2) + theme_minimal() +
  ggtitle('Bush - Use of Nouns (Normalised) over time') + theme(plot.title = element_text(lineheight=.8, face="bold", family="URWGothic"))
ggsave('bushnouns.png')

# Linear models, comparison from first point to all points after 700 days
df = dataReagan
df2 = df
df = filter(dataReagan, Days > 700 | Days < 1)
df2 = filter(dataReagan, Days > 700 | Days < 37 & Days > 2)
df = df %>% select(Days, ppron)
df2 = df2 %>% select(Days, ppron)
df$diff = df$ppron - 8.86
df2$diff = df2$ppron - 9.44

df$status = 'stable'
df$status[df$diff > 0.10] = 'declining' 
df$status[df$diff < -0.10] = 'improving'

df2$status = 'stable'
df2$status[df2$diff > 0.10] = 'declining' 
df2$status[df2$diff < -0.10] = 'improving'

cmp1 = ggplot(df, aes(x=Days, y=ppron)) + 
  geom_point() + 
  geom_segment(aes(x = 0, y = 8.83, xend = df$Days, yend = df$ppron, color = status), data = df)
cmp1 = cmp1 + scale_color_brewer(palette = "Set1")
cmp1 = cmp1 + scale_y_reverse()
cmp1 = cmp1+ ggtitle('Reagan - Personal Pronouns over time') + theme(plot.title = element_text(lineheight=.8, face="bold", family="URWGothic"))
cmp1
ggsave('comp1.png')

cmp2 = ggplot(df2, aes(x=Days, y=ppron)) + 
  geom_point() + 
  geom_segment(aes(x = 36, y = 9.44, xend = df2$Days, yend = df2$ppron, color = status), data = df2)
cmp2 = cmp2 + scale_color_brewer(palette = "Set1")
cmp2 = cmp2 + scale_y_reverse()
cmp2 = cmp2 + 
  ggtitle('Reagan - Personal Pronouns over time') + theme(plot.title = element_text(lineheight=.8, face="bold", family="URWGothic"))

ggsave('comp2.png')

cmp1
cmp2

# T-tests (Reagan v Bush)
x = dataReagan$WordCount
y = dataBush$WordCount
t.test(x, y)

x = dataReagan$UniqueStems
y = dataBush$UniqueStems
t.test(x, y)

x = dataReagan$MLU
y = dataBush$MLU
t.test(x, y)

x = dataReagan$NSNouns
y = dataBush$NSNouns
t.test(x, y)

x = dataReagan$LIVerbs
y = dataBush$LIVerbs
t.test(x, y)

# T-tests (Reagan v Trump)
x = dataReagan$WordCount
y = dataTrump$WordCount
t.test(x, y)

x = dataReagan$UniqueStems
y = dataTrump$UniqueStems
t.test(x, y)

x = dataReagan$MLU
y = dataTrump$MLU
t.test(x, y)

x = dataReagan$NSNouns
y = dataTrump$NSNouns
t.test(x, y)

x = dataReagan$LIVerbs
y = dataTrump$LIVerbs
t.test(x, y)

# T-tests (Bush v Trump)
x = dataBush$WordCount
y = dataTrump$WordCount
t.test(x, y)

x = dataBush$UniqueStems
y = dataTrump$UniqueStems
t.test(x, y)

x = dataBush$MLU
y = dataTrump$MLU
t.test(x, y)

x = dataBush$NSNouns
y = dataTrump$NSNouns
t.test(x, y)

x = dataBush$LIVerbs
y = dataTrump$LIVerbs
t.test(x, y)

# Fisher's R to Z transformation
library(psych)
dfReagan$r2z = fisherz(dfReagan$r2)
dfBush$r2z = fisherz(dfBush$r2)
dfTrump$r2z = fisherz(dfTrump$r2)

nBush = 132
nReagan = 45
nTrump = 29
x = 1/(nReagan-3)
y = 1/(nBush-3)

dfBush = arrange(dfBush, feature)
dfReagan = arrange(dfReagan, feature)
dfTrump = arrange(dfTrump, feature)

compareRRtoGHWB = sqrt(x + y)
a = data.frame(dfReagan$r2)
a[,2] = data.frame(dfBush$r2)
a[,3] = data.frame(dfBush$feature)
comp = data.frame(result=numeric())[1:200, ]
i = NULL

i = 2

days = dataReagan$Days
total = 0
for (i in 1:length(days)-1) {
  diff = days[i+1] - days[i]
  total = total + diff
  meandiff = total / length(days)
}

