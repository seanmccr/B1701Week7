# ----- B1701 Week 7 | Lecture Practical | 01.11.23 -----

library(tidyverse)


LoIWing <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 7/ClassPractical/LoIWingers.csv")
str(LoIWing)

# Convert 'Contract.expires' to Date format
LoIWing$`Contract.expires` <- as.Date(LoIWing$`Contract.expires`, "%Y-%m-%d")


# Create a new filtered dataset
FilteredLoIWing <- LoIWing %>%
  select(1:7,'Contract.expires', 'Duels.won...', 'Minutes.played','Goals', 'Assists', 'xG', 'xA', 'Crosses.per.90','Shots.on.target...', 'Accurate.crosses...', 'Crosses.from.left.flank.per.90', 'Crosses.from.right.flank.per.90', 'Key.passes.per.90', 'Through.passes.per.90', 'Passes.to.final.third.per.90') %>%
  filter(`Minutes.played` > 1200, `Contract.expires` < as.Date("2024-01-30"))
        


  
