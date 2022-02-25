library("dplyr")
library("rcompanion")
library("car")

CancerRecoded$RaceR <- as.numeric(CancerRecoded$RaceR)

plotNormalHistogram(CancerRecoded$RaceR)
CancerRecoded$RaceRSQRT <- sqrt(CancerRecoded$RaceR)
plotNormalHistogram(CancerRecoded$RaceRSQRT)

bartlett.test(RaceR ~ Population, data=CancerRecoded)

fligner.test(RaceR ~ Population, data=CancerRecoded)

CancerRecodedANOVA <- aov((CancerRecoded$RaceR ~ CancerRecoded$Population))
summary(CancerRecodedANOVA)