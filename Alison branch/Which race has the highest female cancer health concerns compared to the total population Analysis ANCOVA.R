# Load Libraries
library("rcompanion")
library("car")
library("effects")
library("multcomp")

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
leveneTest(PopulationLOG~Race, data=CancerRename2)
# Data is p-value = 0.0026 which is significant as it is less than .05
# Violated the assumption of homegenity of variance

# Homegeneity of Regression Slopes
Homogeneity_RegrSlp = lm(PopulationLOG~Race, data=CancerRename2)
anova(Homogeneity_RegrSlp)
# Does not meet assumption of homogeneity of regression slopes
# Race does not have an impact on the population
# Race is an independent variable in the model

