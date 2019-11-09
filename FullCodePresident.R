# To do
# FWER and FDR plots in function
# Automate plots fot significant features

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
library(stats)

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

?###--- PLOTS ---###
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
df = read.csv("~/NLP/Reagan.csv")
results = numeric(length(df))
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
for (i in 1:(nrow(df))){
  gamModel = gam(ppron ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$ppron[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(ppron ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$ppron[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("ppron",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

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

vectortmp = data.frame("Nouns",gamPress, lmPress)
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

