####################################################################################
## Goal: explore consistency of baby names through time nationally

####################################################################################
## User settings:
countThresh <- 1000000 # names must have at least this many total counts through time
yLims <- c(1e-05, 1e-1) # y limits on plots

####################################################################################
## Load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
#library(plotly)

####################################################################################
## Load data
df_national <- read.csv("data/NationalNames.csv", stringsAsFactors = FALSE) #"../input/"

## Add features
# combine name with gender to make a unique identifier
df_national$identifier <- with(df_national, paste(Name, Gender, sep="_"))

# normalize the count by the total count within each year
df_national$CountFrac <- ave(df_national$Count, df_national$Year, FUN = function(x) x/sum(x))

####################################################################################
## Explore: most common names through all time
# sum the name counts over all years (takes ~120s)
df_agg_total <- ddply(df_national, .(Name, Gender), summarize, totalCounts = sum(Count), peakYear = mean(Year[Count == max(Count)]), avgFrac = mean(CountFrac))

## order the aggregated total
df_agg_total <- df_agg_total[order(df_agg_total$totalCounts, decreasing = TRUE),]

# combine name and gender
df_agg_total$identifier <- with(df_agg_total, paste(Name, Gender, sep="_"))

####################################################################################
## Which names are the most consistent over time?

# compute the standard deviatin (relative to mean) of the names to find the consistent ones
df_agg_consistent <- ddply(df_national, .(identifier), summarize, sigma = sd(CountFrac), pctSigma = sd(CountFrac/mean(CountFrac)))
df_agg_total_consistent <- merge(df_agg_total, df_agg_consistent, by = "identifier")

# take names above some nominal count value
df_c <- filter(df_agg_total_consistent, totalCounts > countThresh)

# order names with least variable at the top
df_c <- df_c[order(df_c$pctSigma, decreasing = FALSE),]

# plot the most consistent names
q <- ggplot(data = filter(df_national, identifier %in% df_c$identifier[1:8]), aes(x = Year, y = CountFrac, col = Name, shape = Gender))
q + geom_point(size = 2, alpha = 0.5) +
  scale_y_log10(limits = yLims) + 
  ylab("Count normalized by count of all names in year")

# plot the least consistent names
end_dim <- dim(df_c)[1]
q <- ggplot(data = filter(df_national, identifier %in% df_c$identifier[(end_dim-8):end_dim]), aes(x = Year, y = CountFrac, col = Name, shape = Gender))
q + geom_point(size = 2, alpha = 0.5) +
  scale_y_log10(limits = yLims) + 
  ylab("Count normalized by count of all names in year")
