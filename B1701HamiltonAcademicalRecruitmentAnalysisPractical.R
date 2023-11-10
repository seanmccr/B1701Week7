# ----- B1701 Week 7 | Lecture Practical | 01.11.23 -----

library(tidyverse)


LoIWing <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 7/ClassPractical/LoIWingers.csv")
str(LoIWing)

# Convert 'Contract.expires' to Date format
LoIWing$`Contract.expires` <- as.Date(LoIWing$`Contract.expires`, "%Y-%m-%d")

# Create a new variable 'Shots.on.target.per.90'
LoIWing <- LoIWing %>%
  mutate(`Shots.on.target.per.90` = `Shots.on.target...` / `Minutes.played` * 90,
         `Duels.won.per.90` = `Duels.won...` / `Minutes.played` * 90,
         `Successful.dribbles.per.90` = `Successful.dribbles...` / `Minutes.played` * 90)

# Code to create a filtered dataset
FilteredLoIWing <- LoIWing %>%
  select(1:7,'Contract.expires', 'Duels.won.per.90', 'Successful.dribbles.per.90', 'Minutes.played','Goals', 'Assists', 'xG', 'xA', 'Crosses.per.90','Shots.on.target.per.90', 'Accurate.crosses...', 'Crosses.from.left.flank.per.90', 'Crosses.from.right.flank.per.90', 'Key.passes.per.90', 'Through.passes.per.90', 'Passes.to.final.third.per.90') %>%
  filter(`Minutes.played` > 1000, `Contract.expires` < as.Date("2024-01-30"), (`Position` %in% c('LAMF, LW','LW, LAMF', 'RW, RAMF', 'RAMF, RW', 'LAMF, RAMF', 'RAMF, LAMF', 'RAMF, RW, LAMF', 'LWF', 'RAMF, RW, RWF', 'RW, RB, RWB', 'RWB, LWB', 'LWB, RWB', 'RB, RWB', 'RWB, RB', 'LWB, LB', 'LB, LWB')))
        
# Define the weight for each variable
weightVector <- c('Goals_rank' = 1, 'Assists_rank' = 1, 'xG_rank' = 1, 'xA_rank' = 1, 'Shots.on.target.per.90_rank' = 1, 
                  'Duels.won.per.90_rank' = 2, 'Successful.dribbles.per.90_rank' = 2, 
                  'Crosses.per.90_rank' = 3, 'Accurate.crosses..._rank' = 3, 'Crosses.from.left.flank.per.90_rank' = 3, 
                  'Crosses.from.right.flank.per.90_rank' = 3,
                  'Key.passes.per.90_rank' = 4, 'Through.passes.per.90_rank' = 4, 'Passes.to.final.third.per.90_rank' = 4)

# Code to rank players and create 'rank' variables
RankedLoIWing <- FilteredLoIWing %>%
  mutate_at(
    vars(8, 9, 11:22), 
    list(rank = ~rank(desc(.)))
  )

# Code to create an aggregated score by weight
RankedLoIWing <- RankedLoIWing %>%
  mutate(Aggregate_Score = rowSums(select(., ends_with("rank")) * weightVector))

# Code to re-order players in aggregate order
FinalRanking <- RankedLoIWing %>%
  arrange(desc(Aggregate_Score))

# Create the bar plot
ggplot(FinalRanking, aes(x=reorder(Player, Aggregate_Score), y=Aggregate_Score, fill=Player)) +
  geom_bar(stat='identity') +
  scale_fill_brewer(palette="Spectral") +
  xlab("Player") +
  ylab("Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





