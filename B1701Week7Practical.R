# ----- B1701 Week 7 | Lecture Practical | 01.11.23 -----

library(tidyverse)


LoIWing <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 7/ClassPractical/LoIWingers.csv")
str(LoIWing)

# Convert 'Contract.expires' to Date format
LoIWing$`Contract.expires` <- as.Date(LoIWing$`Contract.expires`, "%Y-%m-%d")

# Create a new variable 'Shots.on.target.per.90'
LoIWing <- LoIWing %>%
  mutate(`Shots.on.target.per.90` = `Shots.on.target...` / `Minutes.played` * 90,
         `Duels.won.per.90` = `Duels.won...` / `Minutes.played` * 90)

# Filter data where 'Position' includes only the specified positions
FilteredData <- LoIWing %>%
  filter(`Position` %in% c('LAMF, LW','LW, LAMF', 'RW, RAMF', 'RAMF, RW', 'LAMF, RAMF', 'RAMF, LAMF', 'RAMF, RW, LAMF', 'LWF', 'RAMF, RW, RWF', 'RW, RB, RWB'))


# Create a new filtered dataset
FilteredLoIWing <- LoIWing %>%
  select(1:7,'Contract.expires', 'Duels.won.per.90', 'Minutes.played','Goals', 'Assists', 'xG', 'xA', 'Crosses.per.90','Shots.on.target.per.90', 'Accurate.crosses...', 'Crosses.from.left.flank.per.90', 'Crosses.from.right.flank.per.90', 'Key.passes.per.90', 'Through.passes.per.90', 'Passes.to.final.third.per.90') %>%
  filter(`Minutes.played` > 1000, `Contract.expires` < as.Date("2024-01-30"), (`Position` %in% c('LAMF, LW','LW, LAMF', 'RW, RAMF', 'RAMF, RW', 'LAMF, RAMF', 'RAMF, LAMF', 'RAMF, RW, LAMF', 'LWF', 'RAMF, RW, RWF', 'RW, RB, RWB', 'RWB, LWB', 'LWB, RWB', 'RB, RWB', 'RWB, RB', 'LWB, LB', 'LB, LWB')))
        

Filtered2LoIWing <- LoIWing %>%
  select(1:7,'Contract.expires', 'Duels.won.per.90', 'Minutes.played','Goals', 'Assists', 'xG', 'xA', 'Crosses.per.90','Shots.on.target.per.90', 'Accurate.crosses...', 'Crosses.from.left.flank.per.90', 'Crosses.from.right.flank.per.90', 'Key.passes.per.90', 'Through.passes.per.90', 'Passes.to.final.third.per.90') %>%
  filter(`Minutes.played` > 1000, `Contract.expires` < as.Date("2024-01-30"))




# Create a new filtered dataset
FilteredLoIWing <- LoIWing %>%
  select(1:7,'Contract.expires', 'Duels.won...', 'Minutes.played','Goals', 'Assists', 'xG', 'xA', 'Crosses.per.90','Shots.on.target...', 'Accurate.crosses...', 'Crosses.from.left.flank.per.90', 'Crosses.from.right.flank.per.90', 'Key.passes.per.90', 'Through.passes.per.90', 'Passes.to.final.third.per.90', 'Shots.on.target.per.90') %>%
  filter(`Minutes.played` > 1200, `Contract.expires` < as.Date("2024-01-30"))