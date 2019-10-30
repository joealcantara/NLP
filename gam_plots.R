# Clear Workspace
rm(list=ls())

# Options
options(scipen=999)

# Libraries
library(dplyr)
library(ggplot2)
library(mgcv)

# Load Data
dataBush = read.csv("~/Documents/NLP/Bush.csv")
dataReagan = read.csv("~/Documents/NLP/Reagan.csv")
dataTrump = read.csv("~/Documents/NLP/Trump.csv")

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

ggplot(df, aes(x=Days, y=ppron)) + 
  geom_point() + 
  geom_segment(aes(x = 0, y = 8.83, xend = df$Days, yend = df$ppron, colour = status), data = df) +
  ggtitle("Comparison 1")
  ggsave('comp1.png')

ggplot(df2, aes(x=Days, y=ppron)) + 
  geom_point() + 
  geom_segment(aes(x = 36, y = 9.44, xend = df2$Days, yend = df2$ppron, colour = status), data = df2) +
  ggtitle("Comparison 2")
ggsave('comp2.png')

gamModel1 = gam(ppron ~ s(Days, bs="gp"), data = dataReagan, family = Gamma(link="log"), method="REML")
coef(gamModel1)
plot(gamModel1, residuals = TRUE, pch = 1)
summary.gam(gamModel1)

fit = gamModel1$fitted.values
gam.check(gamModel1, pch=19, cex=.3)
plot.gam(gamModel1)
vis.gam(gamModel1)
preds = predict.gam(gamModel1, df, type = "response")
difference = preds - df$ppron 
sse = sum(difference^2)
linearMod1 = lm(df$ppron ~ df$Days) 
residuals = linearMod1$residuals
sseLinear = sum(residuals^2)
cat(sse, ' : SSE - GAM')
cat(sseLinear, ' : SSE Linear Model')
# press prediction error sum of squares
library(qpcR)
PRESS(gamModel1)







scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm")

scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$ppron, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataBush, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/GHWBppron.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataBush, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="loess")
cor.test(dataBush$Days, dataBush$ppron, method = "pearson", conf.level = 0.95)
#dev.off()

scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/RRUniqueWords.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$UniqueWords, method = "pearson", conf.level = 0.95)
#dev.off()

scatter_plot <- ggplot(dataBush, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/GHWBUniqueWords.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataBush, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="loess")
cor.test(dataBush$Days, dataBush$UniqueWords, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="lm")

scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$UniqueWords, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataBush, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/GHWBUniqueWords.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataBush, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="loess")
cor.test(dataBush$Days, dataBush$UniqueWords, method = "pearson", conf.level = 0.95)
#dev.off()


scatter_plot <- ggplot(dataReagan, aes(Days, social))
scatter_plot + geom_point() + labs(x = "Days", y = "social") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, NounsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "NounsNormalised") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, conj))
scatter_plot + geom_point() + labs(x = "Days", y = "conj") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, PronounsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "PronounsNormalised") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

# T-tests
t.test(dataReagan$UniqueStems, dataBush$UniqueStems, alternative = "two.sided")
t.test(dataReagan$UniqueStems, dataTrump$UniqueStems, alternative = "two.sided")
t.test(dataBush$UniqueStems, dataTrump$UniqueStems, alternative = "two.sided")

t.test(dataReagan$MLU, dataBush$MLU, alternative = "two.sided")
t.test(dataReagan$MLU, dataTrump$MLU, alternative = "two.sided")
t.test(dataBush$MLU, dataTrump$MLU, alternative = "two.sided")

# Means and SD
summary(dataReagan$MLU)
sd(dataReagan$MLU)
summary(dataBush$MLU)
sd(dataBush$MLU)
summary(dataTrump$MLU)
sd(dataTrump$MLU)

loessTest = loess(ppron ~ Days, data=dataReagan)
res = loessTest$residuals
sse = sum(res^2)
plot(loessTest)

gamModel1 = gam(ppron ~ s(Days, bs="gp"), data = dataReagan, family = Gamma(link="log"), method="REML")
coef(gam_mod)
plot(gam_mod, residuals = TRUE, pch = 1)

res = gamModel1$residuals
sse = sum(res^2)
summary.gam(gamModel1)

fit = gamModel1$fitted.values
gam.check(gamModel1, pch=19, cex=.3)
plot.gam(gamModel1)
vis.gam(gamModel1)

# T-tests
t.test(dataReagan$UniqueStems, dataBush$UniqueStems, alternative = "two.sided")
t.test(dataReagan$UniqueStems, dataTrump$UniqueStems, alternative = "two.sided")
t.test(dataBush$UniqueStems, dataTrump$UniqueStems, alternative = "two.sided")

t.test(dataReagan$MLU, dataBush$MLU, alternative = "two.sided")
t.test(dataReagan$MLU, dataTrump$MLU, alternative = "two.sided")
t.test(dataBush$MLU, dataTrump$MLU, alternative = "two.sided")

# Means and SD
summary(dataReagan$MLU)
sd(dataReagan$MLU)
summary(dataBush$MLU)
sd(dataBush$MLU)
summary(dataTrump$MLU)
sd(dataTrump$MLU)

loessTest = loess(ppron ~ Days, data=dataReagan)
res = loessTest$residuals
sse = sum(res^2)

gam_mod = gam(ppron ~ s(Days), data = dataReagan )
coef(gam_mod)
plot(gam_mod, residuals = TRUE, pch = 1)
