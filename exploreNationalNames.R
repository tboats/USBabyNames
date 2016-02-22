####################################################################################
## Goal of script: explore national names data for US Baby Names
#
# Tom Boatwright
# 02/21/16
#
####################################################################################
## Load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(plotly)

####################################################################################
## Load data
df_national <- read.csv("data/NationalNames.csv", stringsAsFactors = FALSE)

####################################################################################
## Explore: most common names through all time
ptm <- proc.time()
df_agg_total <- ddply(df_national, .(Name, Gender), summarize, totalCounts = sum(Count), peakYear = mean(Year[Count == max(Count)]))
proc.time() - ptm


## order the aggregated total
df_agg_total <- df_agg_total[order(df_agg_total$totalCounts, decreasing = TRUE),]

# plot the top 100 names
df_agg_total$Name <- factor(df_agg_total$Name, levels = as.character(unique(df_agg_total$Name)))
q <- ggplot(data = df_agg_total[1:70,], aes(x = Name, y = totalCounts, col = Gender))
q <- q + geom_point(size = 3, alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(q)

# plot_ly(df_agg_total, x = Name, y = totalCounts, color = Gender, mode = "markers")

####################################################################################
## Explore: how the most common names trend through time

# combine name and gender
df_agg_total$identifier <- with(df_agg_total, paste(Name, Gender, sep="_"))
df_national$identifier <- with(df_national, paste(Name, Gender, sep="_"))


topN <- 70
topIdentifiers <- df_agg_total$identifier[1:topN]
topNames <- df_agg_total$Name[1:topN]
df_topNames <- filter(df_national, identifier %in% topIdentifiers)
df_topNames$Name <- factor(df_topNames$Name, levels = as.character(topNames))

# plot count vs. time
# q <- ggplot(data = df_topNames, aes(x = Year, y = Count, col = Name))
# q + geom_point(size = 3, alpha = 0.5)

# boxplot of each name
q <- ggplot(data = df_topNames, aes(x = Name, y = Count, col = Gender))
q + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


####################################################################################
## Explore: what is the most common name of the last 10 years

yearsBack <- 10
currentYear <- max(df_national$Year)
df_national_recent <- filter(df_national, Year > (currentYear - yearsBack))
head(df_national_recent)

ptm <- proc.time()
df_agg_recent <- ddply(df_national_recent, .(Name, Gender), summarize, totalCounts = sum(Count), peakYear = mean(Year[Count == max(Count)]))
proc.time() - ptm

## order the aggregated total
df_agg_recent <- df_agg_recent[order(df_agg_recent$totalCounts, decreasing = TRUE),]

# plot the top 100 names
df_agg_recent$Name <- factor(df_agg_recent$Name, levels = as.character(unique(df_agg_recent$Name)))
q <- ggplot(data = df_agg_recent[1:70,], aes(x = Name, y = totalCounts, col = Gender))
q <- q + geom_point(size = 3, alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(q)


## Explore: parents names
# plot count vs. time
q <- ggplot(data = filter(df_national, Name %in% c("George", "Linda")), aes(x = Year, y = Count, col = Name))
q + geom_point(size = 3, alpha = 0.5)

q <- ggplot(data = filter(df_national, Name %in% c("Thomas", "Julie")), aes(x = Year, y = Count, col = Name))
q + geom_point(size = 3, alpha = 0.5)