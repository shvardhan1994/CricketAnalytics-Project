# importing original datasets
library(readr)
matches <- read_csv("matches.csv")
View(matches)

deliveries <- read_csv("deliveries.csv")
View(deliveries)

#Selecting the matches of season
Season1_matches <- matches[matches$season==2008,]
View(Season1_matches)

# Filtering the matches data from deleveries dataset using the id in season1_matches
Season1_deliveries <- deliveries[deliveries$match_id >= min(Season1_matches$id) & deliveries$match_id <= max(Season1_matches$id),]
View(Season1_deliveries)

#Deleting unwanted columns from the data
Season1_deliveries <- Season1_deliveries[,-c(2,3,4,5,6,8,9,10,11,12,13,14,15,17,18,19,21)]
View(Season1_deliveries)

#selecting all the players from whole season 
Batsmen_name<- unique(Season1_deliveries$batsman)

boundaries <- c()
Hard_hitting_ability <- c()
Finisher <- c()
for (i in 1:length(Batsmen_name)) {
  
  #Attribute1 Hard Hitting Ability
  
  # getting single player stats
  individualplayer <- Season1_deliveries[Season1_deliveries$batsman == Batsmen_name[i],]
  #Counting number of fours + sixes
  boundaries[i] <- dim(individualplayer[individualplayer$batsman_runs == 4 | individualplayer$batsman_runs == 6,])[1]
  # dim(individualplayer)[1] gives the total number of balls faced by the player 
  Hard_hitting_ability[i] <-  boundaries[i] / dim(individualplayer)[1]
  
  #Attribute2 Finisher
  
  # Total innings played
  Total_innings_played <- length(unique(individualplayer$match_id))
  # number of innings not out
  outs <- na.omit(individualplayer)
  Not_out_innings <- Total_innings_played - length(unique(outs$match_id))
  # calculating Finisher attribute
  Finisher[i] <- Not_out_innings / Total_innings_played
}

boundaries
#Creating dataframe for atrributes
df <- data.frame(Batsmen_name,Hard_hitting_ability,Finisher,boundaries)
colnames(df) <- c("player_name","Hard Hitting Ability","Finisher", "Total Boundaries")
View(df)
