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

dfReagan = data.frame(feature = character(),
                r2      = numeric(),
                pvalue  = numeric(),
                stringsAsFactors = FALSE)

dfBush = data.frame(feature = character(),
                      r2      = numeric(),
                      pvalue  = numeric(),
                      stringsAsFactors = FALSE)

dfTrump = data.frame(feature = character(),
                      r2      = numeric(),
                      pvalue  = numeric(),
                      stringsAsFactors = FALSE)

dataBush$Julian = NULL
dataTrump$Julian = NULL
dataReagan$Julian = NULL
dataBush$X = NULL
dataTrump$X = NULL
dataReagan$X = NULL
dataBush$index = NULL
dataTrump$index = NULL
dataReagan$index = NULL


for(i in seq_along(names(dataReagan))){
  if(is.numeric(dataReagan[, i])){
    pearson = cor.test(dataReagan$Days, dataReagan[, i], method = "pearson")
    dfReagan = rbind(dfReagan, data.frame(feature = names(dataReagan)[i],
                              r2      = unname(pearson$estimate),
                              pvalue  = pearson$p.value,
                              stringsAsFactors = FALSE))
    }
  }


for(i in seq_along(names(dataBush))){
  if(is.numeric(dataBush[, i])){
    pearson = cor.test(dataBush$Days, dataBush[, i], method = "pearson")
    dfBush = rbind(dfBush, data.frame(feature = names(dataBush)[i],
                              r2      = unname(pearson$estimate),
                              pvalue  = pearson$p.value,
                              stringsAsFactors = FALSE))
  }
}

for(i in seq_along(names(dataTrump))){
  if(is.numeric(dataTrump[, i])){
    pearson = cor.test(dataTrump$Days, dataTrump[, i], method = "pearson")
    dfTrump = rbind(dfTrump, data.frame(feature = names(dataTrump)[i],
                              r2      = unname(pearson$estimate),
                              pvalue  = pearson$p.value,
                              stringsAsFactors = FALSE))
  }
}

# Order by P Value
dfReagan = dfReagan[order(dfReagan$pvalue),]
dfBush = dfBush[order(dfBush$pvalue),]
dfTrump = dfTrump[order(dfTrump$pvalue),]

## Creation of FDR Columns
# Reagan
dfReagan$Bonferroni = p.adjust(dfReagan$pvalue, method = "bonferroni")
dfReagan$BH = p.adjust(dfReagan$pvalue, method = "BH")
dfReagan$holm = p.adjust(dfReagan$pvalue, method = "holm")
dfReagan$hochberg = p.adjust(dfReagan$pvalue, method = "hochberg")
dfReagan$hommel = p.adjust(dfReagan$pvalue, method = "hommel")
dfReagan$BY = p.adjust(dfReagan$pvalue, method = "BY")

# Bush
dfBush$Bonferroni = p.adjust(dfBush$pvalue, method = "bonferroni")
dfBush$BH = p.adjust(dfBush$pvalue, method = "BH")
dfBush$holm = p.adjust(dfBush$pvalue, method = "holm")
dfBush$hochberg = p.adjust(dfBush$pvalue, method = "hochberg")
dfBush$hommel = p.adjust(dfBush$pvalue, method = "hommel")
dfBush$BY = p.adjust(dfBush$pvalue, method = "BY")

# Trump
dfTrump$Bonferroni = p.adjust(dfTrump$pvalue, method = "bonferroni")
dfTrump$BH = p.adjust(dfTrump$pvalue, method = "BH")
dfTrump$holm = p.adjust(dfTrump$pvalue, method = "holm")
dfTrump$hochberg = p.adjust(dfTrump$pvalue, method = "hochberg")
dfTrump$hommel = p.adjust(dfTrump$pvalue, method = "hommel")
dfTrump$BY = p.adjust(dfTrump$pvalue, method = "BY")

## Plots
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

subsetReagan = filter(dfReagan, hommel<0.05, BY<0.05, feature!='Days')
subsetBush = filter(dfBush, hommel<0.05, BY<0.05, feature!='Days')
subsetTrump = filter(dfTrump,  hommel<0.05, BY<0.05, feature!='Days')

subsetReagan2 = filter(dfReagan, BY<0.05, hommel>0.05, feature!='Days')
subsetBush2 = filter(dfBush, BY<0.05, hommel>0.05,  feature!='Days')
subsetTrump2 = filter(dfTrump, BY<0.05, hommel>0.05, feature!='Days')

rm(subsetTrump)
rm(subsetTrump2)


scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm")

jpeg("~/Documents/NLP/plots/RRppron.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$ppron, method = "pearson", conf.level = 0.95)
dev.off()

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
#dev.off()
jpeg("~/Documents/NLP/plots/RRUniqueWordsNormalisedLM.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="lm")
dev.off()
jpeg("~/Documents/NLP/plots/RRUniqueWordsNormalisedLOESS.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$UniqueWords, method = "pearson", conf.level = 0.95)
dev.off()

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

gam_mod = gam(ppron ~ s(Days), data = dataReagan )
coef(gam_mod)
plot(gam_mod, residuals = TRUE, pch = 1)
