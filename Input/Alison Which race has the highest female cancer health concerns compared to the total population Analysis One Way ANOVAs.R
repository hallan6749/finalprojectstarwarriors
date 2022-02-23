# Load Libraries
library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("dplyr")
library("ggplot2")

# Load Dataset
library(readxl)
CancerRename <- read_excel("GitHub/finalprojectstarwarriors/Input/CancerRename.xlsx")
View(CancerRename)

# Question
# Which race has the highest female cancer health concerns compared to the total population?

# Data Wrangling
# Ensure the IV (Race & Category) is a factor
str(CancerRename$Race)
CancerRename$Race = as.factor(CancerRename$Race)

# Ensure the CV is a Factor
str(CancerRename$Category)
CancerRename$Category = as.factor(CancerRename$Category)

# Testing Assumptions

# Normality
plotNormalHistogram(CancerRename$Population)
# Error in Summary.factor(c(28L, 28L, 28L, 28L, 28L, 28L, 28L, 28L, 28L,  : ‘min’ not meaningful for factors

CancerRename$Population = as.numeric((as.character((CancerRename$Population))))
CancerRename$Population

plotNormalHistogram(CancerRename$Population)
# Refer to document Alison First Histogram
# Positive Skew, leaning towards the left side

# Removing Infinite Values
library("IDPmisc")
CancerRename2 <- NaRV.omit(CancerRename)

# SQRT transformation
CancerRename2$PopulationSQRT <- sqrt(CancerRename2$Population)
plotNormalHistogram(CancerRename2$PopulationSQRT)
# Refer to document Alison Second Histogram
# Better not there yet

# Log transformation
CancerRename2$PopulationLOG <- log(CancerRename2$Population)
plotNormalHistogram(CancerRename2$PopulationLOG)
# Refer to document Alison Third Histogram
# Looks pretty normal now

# Homogeneity of Variance
bartlett.test(PopulationLOG~Race, data=CancerRename2)
# Data is p-value = 0.1029 which is not < 0.05.
# Passed Homogenity of Variance
fligner.test(PopulationLOG~Race, data=CancerRename2)
# P Value = 0.009612 which did not pass

# Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)
ANOVA <- lm(PopulationLOG ~ Race, data=CancerRename2)
Anova(ANOVA, Type="II", white.adjust=TRUE)
# This is significant at p < 5.829e-07, so you can conclude that there is a significant difference in price somewhere between the levels of your independent variable.

# Computing Post Hocs with Bonferroni Adjustment
pairwise.t.test(CancerRename2$PopulationLOG, CancerRename2$Race, p.adjust="bonferroni")

# Computing Post Hocs with Bonferroni Adjustment Violated the Assumption for Homogeneity of Variance
pairwise.t.test(CancerRename2$PopulationLOG, CancerRename2$Race, p.adjust="bonferroni", pool.sd = FALSE)
# Same Significant Values


d <- ggplot(CancerRename2, aes(x = Race, y = Population))
d + geom_boxplot() + xlab("")
# Alison First Boxplot

summary(CancerRename2$Population)
