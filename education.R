library(dplyr) # data manipulation 
library(tidyverse) # creating tidy data
library(readxl) # read the excel file
library(car) # regression analysis
library(outliers) # determine any outliers in the dataset

# load the dataset and view it
education <- read_xlsx("C:\\Users\\visha\\Downloads\\EnGen Assessment Data 9.21 to 10.22.xlsx")
View(education)

# remove unnecessary columns and observe the summary of the dataset
educationdf <- education[c(-1,-2,-3,-4,-5,-6,-7)]
view(educationdf)
summary(educationdf)

# filter out test scores that were taken more than 10 minutes
educationdf <- educationdf %>%
  filter(total_time_spent_on_test > 10)
view(educationdf)

# find the means of each category in the overall score column
overall_score1 <- educationdf %>%
  group_by(overall_level)%>%
  summarise(mean_overallscore = mean(round(overall_score)))
view(overall_score1)

# find the means of each category in the reading score column
reading_score1 <- educationdf %>%
  group_by(reading_level)%>%
  summarise(reading_mean = mean(round(reading_score)))
view(reading_score1)

# find the means of each category in the grammar score column
grammar_score1 <- educationdf%>%
  group_by(grammar_level)%>%
  summarise(grammar_mean = mean(round(grammar_score)))
view(grammar_score1)

# find the means of each category in the listening score column
listening_score1 <- educationdf %>%
  group_by(listening_level)%>%
  summarise(listening_mean = mean(round(listening_score)))
view(listening_score1)

# calculate the quantiles of the dataset
quantile(educationdf$overall_score)
quantile(educationdf$reading_score)
quantile(educationdf$grammar_score)
quantile(educationdf$listening_score)

# graph the boxplots of the dataset
boxplot(educationdf$overall_score)
boxplot(educationdf$reading_score)
boxplot(educationdf$grammar_score)
boxplot(educationdf$listening_score)

# Determine outliers of the test scores in the boxplots
out1 <- boxplot.stats(educationdf$overall_score)$out
out2 <- boxplot.stats(educationdf$reading_score)$out
out3 <- boxplot.stats(educationdf$grammar_score)$out
out4 <- boxplot.stats(educationdf$listening_score)$out

# extract out the potential outliers of the following test scores
out_ind <- which(educationdf$grammar_score %in% c(out3))
out_ind1 <- which(educationdf$listening_score %in% c(out4))
educationdf[out_ind,]
ed <- educationdf[out_ind1,]

# grubbs test of each test score
test <- grubbs.test(educationdf$overall_score)
test
test1 <- grubbs.test(educationdf$reading_score)
test1
test2 <- grubbs.test(educationdf$grammar_score)
test2
test3 <- grubbs.test(educationdf$listening_score)
test3

# graph the probability density plots of each test score
a = dnorm(ed$overall_score, mean(ed$overall_score),sd(ed$overall_score))
plot(ed$overall_score, a,col = "orange3",cex = 1.3,pch = 16)

b = dnorm(ed$reading_score, mean(ed$reading_score),sd(ed$reading_score))
plot(ed$reading_score, b,col = "turquoise3",cex = 1.3,pch = 16)

c = dnorm(ed$grammar_score, mean(ed$grammar_score),sd(ed$grammar_score))
plot(ed$grammar_score, c,col = "firebrick3",cex = 1.3,pch = 16)

d = dnorm(ed$listening_score, mean(ed$listening_score),sd(ed$listening_score))
plot(ed$listening_score, d, col = "blue4",cex = 1.3,pch = 16)

# linear regression of each test score
relation <- lm(reading_score ~ grammar_score, data = educationdf)
print(summary(relation))

relation1 <- lm(reading_score ~ listening_score, data = educationdf)
print(summary(relation1))

relation2 <- lm(grammar_score ~ listening_score, data = educationdf)
print(summary(relation2))

# plot of linear regression of each test score
plot(x = educationdf$reading_score, y = educationdf$grammar_score ,main = "Reading & Grammar Score Regression", 
     abline(relation),cex = 1.3,pch = 16, col = "blue",xlab = "Grammar Score",ylab = "Reading Score")

plot(x = educationdf$reading_score, y = educationdf$listening_score,main = "Reading & Listening Score Regression", 
     abline(relation),cex = 1.3,pch = 16,col = "#458B00",xlab = "Listening Score",ylab = "Reading Score")    

plot(x = educationdf$grammar_score, y = educationdf$listening_score,main = "Grammar & Listening Score Regression", 
     abline(relation),cex = 1.3,pch = 16,col = "deeppink2",xlab = "Listening Score",ylab = "Grammar Score")    

model <- lm(overall_score ~ reading_score + grammar_score + listening_score, data = educationdf) 
print(model)

# chi-square test of each test score
chisq.test(educationdf$reading_level, educationdf$grammar_level, correct = FALSE)
chisq.test(educationdf$reading_level, educationdf$listening_level, correct = FALSE)
chisq.test(educationdf$grammar_level, educationdf$listening_level, correct = FALSE)

# shapiro test of each test score
shapiro.test(ed$overall_score)
shapiro.test(ed$reading_score)
shapiro.test(ed$grammar_score)
shapiro.test(ed$listening_score)

# fligner-killen test of each test score
fligner.test(reading_score ~ grammar_score, data = educationdf)
fligner.test(reading_score ~ listening_score, data = educationdf)
fligner.test(grammar_score ~ listening_score, data = educationdf)

# anova test of each test score
aov1 <- aov(reading_score ~ reading_level, data = educationdf)
summary(aov1)
model.tables(aov1,"means")

aov2 <- aov(grammar_score ~ grammar_level, data = educationdf)
summary(aov2)
model.tables(aov2,"means")

aov3 <- aov(listening_score ~ listening_level, data = educationdf)
summary(aov3)
model.tables(aov3,"means")