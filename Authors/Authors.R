# Clear Workspace
rm(list=ls())

# Load Libraries
library(ggplot2)

# Load Data
PDJ <- read.csv("~/Documents/NLP/Authors/PDJ.csv")
AC <- read.csv("~/Documents/NLP/Authors/AC.csv")
IM <- read.csv("~/Documents/NLP/Authors/IM.csv")


scatter_plot <- ggplot(AC, aes(Year.of.Publication , TTR))
scatter_plot +  geom_point() + labs(x = "Year", y = "ppron") + geom_smooth(method="lm", color = "black", lty = 1) + geom_smooth(method="loess", color = "black", lty = 2) + theme_gray()

scatter_plot <- ggplot(IM, aes(Year.of.Publication , TTR))
scatter_plot +  geom_point() + labs(x = "Year", y = "ppron") + geom_smooth(method="lm", color = "black", lty = 1) + geom_smooth(method="loess", color = "black", lty = 2) + theme_gray()

scatter_plot <- ggplot(PDJ, aes(Year.of.Publication , TTR))
scatter_plot +  geom_point() + labs(x = "Year", y = "ppron") + geom_smooth(method="lm", color = "black", lty = 1) + geom_smooth(method="loess", color = "black", lty = 2) + theme_gray()

scatter_plot <- ggplot(AC, aes(Year.of.Publication , UniqueStems))
scatter_plot +  geom_point() + labs(x = "Year", y = "UniqueStems") + geom_smooth(method="lm", color = "black", lty = 1) + geom_smooth(method="loess", color = "black", lty = 2) + theme_gray()

scatter_plot <- ggplot(IM, aes(Year.of.Publication , UniqueStems))
scatter_plot +  geom_point() + labs(x = "Year", y = "UniqueStems") + geom_smooth(method="lm", color = "#CC0033", fill = '#CC0033', lty = 1) + geom_smooth(method="loess", color = "#3399FF", fill = '#3399FF', lty = 2) + theme_minimal() +
  ggtitle('Iris Murdoch - Use of unique words over time') + theme(plot.title = element_text(lineheight=.8, face="bold", family="URWGothic"))
ggsave('authors.png')

scatter_plot <- ggplot(PDJ, aes(Year.of.Publication , UniqueStems))
scatter_plot +  geom_point() + labs(x = "Year", y = "UniqueStems") + geom_smooth(method="lm", color = "black", lty = 1) + geom_smooth(method="loess", color = "black", lty = 2) + theme_gray()
