# Clear Workspace
rm(list=ls())

# Options
options(scipen=999)

# Load Libraries
library(FSA)

# Import Datasets
dfTrump <- read.csv("~/Documents/NLP/ResultsTrump.csv")
dfBush <- read.csv("~/Documents/NLP/ResultsBush.csv")
dfReagan <- read.csv("~/Documents/NLP/ResultsReagan.csv")

# Order by P Value
dfReagan = dfReagan[order(dfReagan$P.Value),]
dfBush = dfBush[order(dfBush$P.Value),]
dfTrump = dfTrump[order(dfTrump$P.Value),]

## Creation of FDR Columns
# Reagan
dfReagan$Bonferroni = p.adjust(dfReagan$P.Value, method = "bonferroni")
dfReagan$BH = p.adjust(dfReagan$P.Value, method = "BH")
dfReagan$holm = p.adjust(dfReagan$P.Value, method = "holm")
dfReagan$hochberg = p.adjust(dfReagan$P.Value, method = "hochberg")
dfReagan$hommel = p.adjust(dfReagan$P.Value, method = "hommel")
dfReagan$BY = p.adjust(dfReagan$P.Value, method = "BY")

# Bush
dfBush$Bonferroni = p.adjust(dfBush$P.Value, method = "bonferroni")
dfBush$BH = p.adjust(dfBush$P.Value, method = "BH")
dfBush$holm = p.adjust(dfBush$P.Value, method = "holm")
dfBush$hochberg = p.adjust(dfBush$P.Value, method = "hochberg")
dfBush$hommel = p.adjust(dfBush$P.Value, method = "hommel")
dfBush$BY = p.adjust(dfBush$P.Value, method = "BY")

# Trump
dfTrump$Bonferroni = p.adjust(dfTrump$P.Value, method = "bonferroni")
dfTrump$BH = p.adjust(dfTrump$P.Value, method = "BH")
dfTrump$holm = p.adjust(dfTrump$P.Value, method = "holm")
dfTrump$hochberg = p.adjust(dfTrump$P.Value, method = "hochberg")
dfTrump$hommel = p.adjust(dfTrump$P.Value, method = "hommel")
dfTrump$BY = p.adjust(dfTrump$P.Value, method = "BY")

## Plots
# Reagan
XReagan = dfReagan$P.Value
YReagan = cbind(dfReagan$Bonferroni, dfReagan$BH, dfReagan$holm, dfReagan$hochberg,
          dfReagan$hommel, dfReagan$BY)
matplot(XReagan, YReagan, xlab = "Raw p-value", ylab = "Adjusted p-value", type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

# Bush
XBush = dfBush$P.Value
YBush = cbind(dfBush$Bonferroni, dfBush$BH, dfBush$holm, dfBush$hochberg,
                dfBush$hommel, dfBush$BY)
matplot(XBush, YBush, xlab = "Raw p-value", ylab = "Adjusted p-value", type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

# Trump
XTrump = dfTrump$P.Value
YTrump = cbind(dfTrump$Bonferroni, dfTrump$BH, dfTrump$holm, dfTrump$hochberg,
                dfTrump$hommel, dfTrump$BY)
matplot(XTrump, YTrump, xlab = "Raw p-value", ylab = "Adjusted p-value", type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

dfSubsetReagan = dfReagan[which(dfReagan$hommel<0.05), ]| dfReagan[which(dfReagan$BY<0.05), ]


