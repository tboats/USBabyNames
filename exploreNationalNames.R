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

## Add features
# combine name with gender to make a unique identifier
df_national$identifier <- with(df_national, paste(Name, Gender, sep="_"))

# normalize the count by the total count within each year
df_national$CountFrac <- ave(df_national$Count, df_national$Year, FUN = function(x) x/sum(x))

####################################################################################
## Explore: most common names through all time
# sum the name counts over all years (takes ~60s)
ptm <- proc.time()
df_agg_total <- ddply(df_national, .(Name, Gender), summarize, totalCounts = sum(Count), peakYear = mean(Year[Count == max(Count)]), avgFrac = mean(CountFrac))
proc.time() - ptm

## order the aggregated total
df_agg_total <- df_agg_total[order(df_agg_total$totalCounts, decreasing = TRUE),]

# plot the top 100 names
df_agg_total$Name <- factor(df_agg_total$Name, levels = as.character(unique(df_agg_total$Name)))
q <- ggplot(data = df_agg_total[1:70,], aes(x = Name, y = totalCounts, col = Gender))
q <- q + geom_point(size = 3, alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(q)

####################################################################################
## Explore: how the most common names trend through time

# combine name and gender
df_agg_total$identifier <- with(df_agg_total, paste(Name, Gender, sep="_"))



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
df_agg_recent <- ddply(df_national_recent, .(Name, Gender), summarize, totalCounts = sum(Count), peakYear = mean(Year[Count == max(Count)]), avgFrac = mean(CountFrac))
proc.time() - ptm

## order the aggregated total
df_agg_recent$identifier <- with(df_agg_recent, paste(Name, Gender, sep="_"))
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

## Explore: weird names
q <- ggplot(data = filter(df_national, Name %in% c("Barack")), aes(x = Year, y = CountFrac, col = Name))
q + geom_point(size = 3, alpha = 0.5)


####################################################################################
## Are people choosing more obscure names in the last 10 years?

# grab top 100 names from last 10 years
topN <- 100
top_recent_identifiers <- df_agg_recent$identifier[1:topN]

# select those top names from the full data set
df_full_topRecent <- filter(df_agg_total, identifier %in% top_recent_identifiers)

# join the recent and full sets
df_join <- full_join(df_full_topRecent, df_agg_recent, by = "identifier")
names(df_join)[names(df_join) %in% "totalCounts.x"] <- "totalCounts_full"
names(df_join)[names(df_join) %in% "totalCounts.y"] <- "totalCounts_recent"

# scatter plot of the counts
names(df_join)

q <- ggplot(data=df_join, aes(x = avgFrac.x, y = avgFrac.y))
q + geom_point(size=5, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1) + 
  xlab("Count fraction over full time range") + 
  ylab("Count fraction from 2005-2014") +
  coord_fixed(ratio = 1)

####################################################################################
## Are the historically most common names being less utilized today?

# grab top 100 names from last 10 years
topN <- 100
top_past_identifiers <- df_agg_total$identifier[1:topN]

# select those top names from the full data set
df_recent_topPast <- filter(df_agg_recent, identifier %in% top_past_identifiers)

# join the recent and full sets
df_join <- full_join(df_agg_total[1:topN,], df_recent_topPast, by = "identifier")
# names(df_join)[names(df_join) %in% "totalCounts.x"] <- "totalCounts_full"
# names(df_join)[names(df_join) %in% "totalCounts.y"] <- "totalCounts_recent"

# scatter plot of the counts
names(df_join)

q <- ggplot(data=df_join, aes(x = avgFrac.x, y = avgFrac.y))
q + geom_point(size=5, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1) + 
  xlab("Count fraction over full time range") + 
  ylab("Count fraction from 2005-2014") +
  coord_fixed(ratio = 1)



####################################################################################
## Which names are the most consistent over time?

# compute the standard deviatin (relative to mean) of the names to find the consistent ones
df_agg_consistent <- ddply(df_national, .(identifier), summarize, sigma = sd(CountFrac), pctSigma = sd(CountFrac/mean(CountFrac)))
df_agg_total_consistent <- merge(df_agg_total, df_agg_consistent, by = "identifier")

# take names above some nominal count value
summary(df_agg_total_consistent$totalCounts)
countThresh <- 1000000
df_c <- filter(df_agg_total_consistent, totalCounts > countThresh)

# order names with least variable at the top
df_c <- df_c[order(df_c$pctSigma, decreasing = FALSE),]

# plot the most consistent names
q <- ggplot(data = filter(df_national, identifier %in% df_c$identifier[1:8]), aes(x = Year, y = CountFrac, col = Name))
q + geom_point(size = 3, alpha = 0.5) +
  scale_y_log10(limits = c(1e-05, 1e-1))

# plot the least consistent names
end_dim <- dim(df_c)[1]
q <- ggplot(data = filter(df_national, identifier %in% df_c$identifier[(end_dim-8):end_dim]), aes(x = Year, y = CountFrac, col = Name))
q + geom_point(size = 3, alpha = 0.5) +
  scale_y_log10(limits = c(1e-05, 1e-1))
