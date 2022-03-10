# Load package

library(tidyverse)

glimpse(CancerCombinedFile3)

# Rename works for Columns

names(CancerCombinedFile3)[names(CancerCombinedFile3) == "State_name"] <- "State"
names(CancerCombinedFile3)[names(CancerCombinedFile3) == "Race_name"] <- "Race"
names(CancerCombinedFile3)[names(CancerCombinedFile3) == "Percentage population below poverty"] <- "Poverty%"
names(CancerCombinedFile3)[names(CancerCombinedFile3) == "Percentage population insured"] <- "Insured"

glimpse(CancerCombinedFile3)

Cancer <- CancerCombinedFile3[, 2:9]
head(Cancer, n=10)

#Re-coding
library(dplyr)
library(gapminder)

Cancer$NameR <- NA

Cancer$NameR[Cancer$Name=='Cervix Uteri'] <- 0
Cancer$NameR[Cancer$Name=='Corpus Uteri'] <- 1
Cancer$NameR[Cancer$Name=='Uterus, NOS'] <- 2
Cancer$NameR[Cancer$Name=='Ovary'] <- 3
Cancer$NameR[Cancer$Name=='Vagina'] <- 4
Cancer$NameR[Cancer$Name=='Vulva'] <- 5
Cancer$NameR[Cancer$Name=='Other Female Genital Organs'] <- 6

Cancer$NameR <- NA

Cancer$NameR[Cancer$Name=='Cervix Uteri'] <- 0
Cancer$NameR[Cancer$Name=='Corpus Uteri'] <- 1
Cancer$NameR[Cancer$Name=='Uterus, NOS'] <- 2
Cancer$NameR[Cancer$Name=='Ovary'] <- 3
Cancer$NameR[Cancer$Name=='Vagina'] <- 4
Cancer$NameR[Cancer$Name=='Vulva'] <- 5
Cancer$NameR[Cancer$Name=='Other Female Genital Organs'] <- 6

library(plyr)
# This rename code works for data in a column

Cancer$Race[Cancer$Race == "American Indian or Alaska Native"] <- "NativeAmer"
Cancer$Race[Cancer$Race == "Black or African American"] <- "AfricanAmer"
Cancer$Race[Cancer$Race == "White"] <- "Caucasian"
Cancer$Race[Cancer$Race == "Other Races and Unknown combined"] <- "Other"
Cancer$Race[Cancer$Race == "Asian or Pacific Islander"] <- "AsianAmer"

glimpse(Cancer)

Cancer$StateR <- NA

Cancer$StateR[Cancer$State=='Alabama'] <- 0
Cancer$StateR[Cancer$State=='Alaska'] <- 1
Cancer$StateR[Cancer$State=='Arizona'] <- 2
Cancer$StateR[Cancer$State=='Arkansas'] <- 3
Cancer$StateR[Cancer$State=='California'] <- 4
Cancer$StateR[Cancer$State=='Colorado'] <- 5
Cancer$StateR[Cancer$State=='Connecticut'] <- 6
Cancer$StateR[Cancer$State=='District of Columbia'] <- 7
Cancer$StateR[Cancer$State=='Georgia'] <- 8
Cancer$StateR[Cancer$State=='Idaho'] <- 9
Cancer$StateR[Cancer$State=='Indiana'] <- 10
Cancer$StateR[Cancer$State=='Kansas'] <- 11
Cancer$StateR[Cancer$State=='Louisiana'] <- 12
Cancer$StateR[Cancer$State=='Maryland'] <- 13
Cancer$StateR[Cancer$State=='Michigan'] <- 14
Cancer$StateR[Cancer$State=='Mississippi'] <- 15
Cancer$StateR[Cancer$State=='Montana'] <- 16
Cancer$StateR[Cancer$State=='Nevada'] <- 17
Cancer$StateR[Cancer$State=='New Jersey'] <- 18
Cancer$StateR[Cancer$State=='New York'] <- 19
Cancer$StateR[Cancer$State=='North Dakota'] <- 20
Cancer$StateR[Cancer$State=='Oklahoma'] <- 21
Cancer$StateR[Cancer$State=='Pennsylvania'] <- 22
Cancer$StateR[Cancer$State=='South Carolina'] <- 23
Cancer$StateR[Cancer$State=='Tennessee'] <- 24
Cancer$StateR[Cancer$State=='Utah'] <- 25
Cancer$StateR[Cancer$State=='Virginia'] <- 26
Cancer$StateR[Cancer$State=='West Virginia'] <- 27
Cancer$StateR[Cancer$State=='Wyoming'] <- 28
Cancer$StateR[Cancer$State=='Delaware'] <- 29
Cancer$StateR[Cancer$State=='Florida'] <- 30
Cancer$StateR[Cancer$State=='Hawaii'] <- 31
Cancer$StateR[Cancer$State=='Illinois'] <- 32
Cancer$StateR[Cancer$State=='Iowa'] <- 33
Cancer$StateR[Cancer$State=='Kentucky'] <- 34
Cancer$StateR[Cancer$State=='Maine'] <- 35
Cancer$StateR[Cancer$State=='Massachusetts'] <- 36
Cancer$StateR[Cancer$State=='Minnesota'] <- 37
Cancer$StateR[Cancer$State=='Missouri'] <- 38
Cancer$StateR[Cancer$State=='Nebraska'] <- 39
Cancer$StateR[Cancer$State=='New Hampshire'] <- 40
Cancer$StateR[Cancer$State=='New Mexico'] <- 41
Cancer$StateR[Cancer$State=='North Carolina'] <- 42
Cancer$StateR[Cancer$State=='Ohio'] <- 43
Cancer$StateR[Cancer$State=='Oregon'] <- 44
Cancer$StateR[Cancer$State=='Rhode Island'] <- 45
Cancer$StateR[Cancer$State=='South Dakota'] <- 46
Cancer$StateR[Cancer$State=='Texas'] <- 47
Cancer$StateR[Cancer$State=='Vermont'] <- 48
Cancer$StateR[Cancer$State=='Washington'] <- 49
Cancer$StateR[Cancer$State=='Wisconsin'] <- 50

unique(Cancer$Race)
Cancer$RaceR <- NA

Cancer$RaceR[Cancer$Race=="NativeAmer"]<-0
Cancer$RaceR[Cancer$Race=="AfricanAmer"]<-1
Cancer$RaceR[Cancer$Race=="Caucasian"]<-2
Cancer$RaceR[Cancer$Race=="Other"]<-3
Cancer$RaceR[Cancer$Race=="AsianAmer"]<-4

Cancer2 <- Cancer[, 5:11]
head(Cancer2, n=10)

