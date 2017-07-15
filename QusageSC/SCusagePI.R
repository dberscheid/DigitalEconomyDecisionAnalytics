#start with empty directory
graphics.off()
rm(list = ls(all = TRUE))


# Load & install packages
if (require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (require("dplyr")) install.packages("dplyr")
library(dplyr)
if (require("stringr")) install.packages("stringr")
library(stringr)

# Load data set
dataCompl = read.csv("data.csv")


# Data preprocessing
summary(dataCompl$CATEGORY)
# delete the sum, which would lead to wrong results
dataCompl = dataCompl[-812, ]
dataCompl = dataCompl %>% group_by(CATEGORY) %>% mutate(sumTXs = sum(TXs))

# for part a)
data = dataCompl[!duplicated(dataCompl$CATEGORY), ]
data = data.frame(data$CATEGORY, data$sumTXs)
data = data %>% rename(category = data.CATEGORY, txs = data.sumTXs)


# combining subcategories to final categories
Financial = sum(data$txs[c(2, 5, 7, 14, 20, 21, 18, 27, 23)])
Notary    = sum(data$txs[c(9, 3)])
Wallet    = data$txs[22]
Game      = sum(data$txs[c(19, 4, 19, 10, 13, 16, 17, 20)])
Library   = sum(data$txs[c(6, 26)])
total     = sum(c(Financial, Notary, Wallet, Game, Library))

# setting options
options(digits = 2)


distribution = data.frame(names = c("Financial", "Notary", "Wallet", "Game", "Library"), value = c(Financial, Notary, Wallet, Game, Library), 
                          percentage = c(Financial/total, Notary/total, Wallet/total, Game/total, Library/total))

# plot the distribution of Ethereum smart contract usage
usagePlot = ggplot(data = distribution, aes(distribution$names, distribution$percentage, fill = "red")) + geom_bar(stat = "identity", show.legend = FALSE) + 
  ggtitle("Usage of Ethereum Smart Contracts by Categories") + xlab("Categories") + ylab("Percentage") + 
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20, face = "bold"))

# save plot in a file
ggsave("usageContracts.png", dpi = 600, bg = "transparent")
