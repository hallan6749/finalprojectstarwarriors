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

library(plyr)
# This rename code works for data in a column

Cancer$Race[Cancer$Race == "American Indian or Alaska Native"] <- "NativeAmer"
Cancer$Race[Cancer$Race == "Black or African American"] <- "AfricanAmer"
Cancer$Race[Cancer$Race == "White"] <- "Caucasian"
Cancer$Race[Cancer$Race == "Other Races and Unknown combined"] <- "Other"
Cancer$Race[Cancer$Race == "Asian or Pacific Islander"] <- "AsianAmer"

glimpse(Cancer)

# Export code to xlsx
install.packages("xlsx")
install.packages("writexl")
library("xlsx")
library("writexl")
write.xlsx(Cancer, file, sheetName = "Sheet1", col.names = TRUE,
            row.names = TRUE, append = FALSE, showNA = TRUE, password = NULL)
write.csv(Cancer, "C:/Users/music/Desktop/CancerRename.csv")
write.xlsx(Cancer, "C:/Users/music/Desktop/CancerRename.xlsx")