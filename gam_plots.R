# Clear Workspace
rm(list=ls())

# Options
options(scipen=999)
getwd()


# Libraries
library(dplyr)
library(ggplot2)
library(mgcv)

# Load Data
dataBush = read.csv("~/Documents/NLP/Bush.csv")
dataReagan = read.csv("~/Documents/NLP/Reagan.csv")
dataTrump = read.csv("~/Documents/NLP/Trump.csv")

df = filter(dataReagan, Days > 700 | Days < 37)
df = df %>% select(Days, ppron)
df$diff = df$ppron - 8.86
df$diff2 = df$ppron - 9.44

plot(df$Days, df$ppron)

numRows = nrow(df)

for (i in 1:numRows) {
  compareRow = df[1, ]
  row = df[i, ]
  lines(c(compareRow$ppron, compareRow$Days), c(row$ppron, row$Days))
}


gamModel1 = gam(ppron ~ s(Days, bs="gp"), data = dataReagan, family = Gamma(link="log"), method="REML")
coef(gamModel1)
plot(gamModel1, residuals = TRUE, pch = 1)

res = gamModel1$residuals
sse = sum(res^2)
summary.gam(gamModel1)
# Sum of squares as assessment of fit? If so, how to calculate?
# Can I do glm with multiple predictors? How does this work?
# Draw all two points approximately 2 years apart from the first 2 data points.
# Taking this apart

fit = gamModel1$fitted.values
gam.check(gamModel1, pch=19, cex=.3)
plot.gam(gamModel1)
vis.gam(gamModel1)

