# Preliminary Set-Up

## Installing Necessary Packages
library(dplyr)
install.packages('rstatix'); library(rstatix)
install.packages('ggpubr'); library(ggpubr)
install.packages('gtsummary'); library(gtsummary)

## Loading in data & naming it
uncleaned.data = read.csv("https://raw.githubusercontent.com/abbyvenz/DNPCareConsiderations_Data/main/CLEAN%20DNP%20Care%20Considerations.csv")

## Checking that the data loaded correctly
head(uncleaned.data,5)
names(uncleaned.data)
## *Note that the second 'Satisfaction' is being recognized as a character, not as a number; this needs to be "cleaned"



# Cleaning the data

## Ensuring that both 'Satisfaction' columns are recognized as containing numbers
half.cleaned.data <- transform(uncleaned.data,
                               Pre.Satisfaction = as.numeric(as.factor(uncleaned.data$Satisfaction.with.current.communication.between.health.care.providers)),
                               Post.Satisfaction = as.numeric(as.factor(uncleaned.data$`Satisfaction.with.current.communication.between.health.care.providers.1`)))

## Removing the old 'Satisfaction' columns
cleaned.data <- half.cleaned.data[-c(18, 27)]

## Reordering and double-checking the cleaned data is now correctly displayed 
data <- cleaned.data[, c(1, 12, 13, 14, 15, 16, 17, 27, 20, 21, 22, 23, 24, 25, 28, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 18, 19, 26)]
head(data, 5)
names(data)



# Summary of Demographic Information

## Creating a Subset of the Demographic Information Columns & Participant Num.
data.subset <- data[, 16:28]

## Fixing value names that don't properly match 
data.subset[data.subset$Gender.=="Female", "Gender."] <- "female"
data.subset[data.subset$Number.of.chronic.medical.conditions.=="3", "Number.of.chronic.medical.conditions."] <- "3+"
data.subset[data.subset$Number.of.chronic.medical.conditions.=="4", "Number.of.chronic.medical.conditions."] <- "3+"
data.subset[data.subset$Number.of.chronic.medical.conditions.=="5", "Number.of.chronic.medical.conditions."] <- "3+"
data.subset[data.subset$Number.of.chronic.medical.conditions.=="5+", "Number.of.chronic.medical.conditions."] <- "3+"
data.subset[data.subset$Number.of.chronic.medical.conditions.=="6", "Number.of.chronic.medical.conditions."] <- "3+"
data.subset[data.subset$Number.of.chronic.medical.conditions.=="6+", "Number.of.chronic.medical.conditions."] <- "3+"
data.subset[data.subset$Referral.Source.=="Self ", "Referral.Source."] <- "Self"
data.subset[data.subset$Care.venue.=="IL  ", "Care.venue."] <- "IL"
data.subset[data.subset$Shared.Visit.=="Yes ", "Shared.Visit."] <- "Yes"
data.subset[data.subset$Met.in.home...telehealth...Common.space.at.CCRC.=="Common space", "Met.in.home...telehealth...Common.space.at.CCRC."] <- "Common Space"
data.subset[data.subset$Met.in.home...telehealth...common.space.at.CCRC.=="telehealth", "Met.in.home...telehealth...common.space.at.CCRC."] <- "Telehealth"

## Getting Summary Statistics of that Subset 
data.subset %>%  tbl_summary(
  statistic = list(all_continuous() ~ "{mean} ({sd})", 
                   all_categorical() ~ "{n} ({p}%)"))



# Summary Statistics of Survey Questions (Pre- & Post- Intervention, independently)

get_summary_stats(data[, 2:15], type = "mean_sd")



# Two-Sample Paired t-Tests for Differences in Means, Pre- vs. Post- Intervention 

## Question 1: HHC Knowledge
t.test(data$`HHC.Knowledge.1`, data$HHC.Knowledge,
       paired = TRUE, p.adjust.methods = 'bonferroni')

## Question 2: ALC Knowledge
t.test(data$`ALC.Knowledge.1`, data$ALC.Knowledge,
       paired = TRUE, p.adjust.methods = 'bonferroni')

## Question 3: SLC Knowledge
t.test(data$`SLC.Knowledge.1`, data$SLC.Knowledge,
       paired = TRUE, p.adjust.methods = 'bonferroni')

## Question 4: Preparedness
t.test(data$`Preparedness.1`, data$Preparedness,
       paired = TRUE, p.adjust.methods = 'bonferroni')

## Question 5: Confidence
t.test(data$`Confidence.1`, data$Confidence,
       paired = TRUE, p.adjust.methods = 'bonferroni')

## Question 6: Health/Well-being
t.test(data$`Health.Well.Being.1`, data$Health.Well.Being,
       paired = TRUE, p.adjust.methods = 'bonferroni')

## Question 7: Satisfaction 
t.test(data$Post.Satisfaction, data$Pre.Satisfaction,
       paired = TRUE, p.adjust.methods = 'bonferroni')

