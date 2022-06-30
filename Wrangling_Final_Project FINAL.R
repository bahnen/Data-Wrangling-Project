#Final Wrangling Project - Mark Conway, Jarrin Flores, Jonas Geerdes, Ben Ahnen and Natalie Lopez 
rm(list=ls())
library(rvest)
library(dplyr)

#Importing Kaggle data for the 2019 College Football statistics  
df = read.csv("CFB2019_Edited2.csv", stringsAsFactors = F,)
df$Conference = as.factor(df$Conference)

#Webscraping football statistics for the 2019 season from Sports Reference 
s<-html("https://www.sports-reference.com/cfb/years/2019-standings.html")

#The follwoing code cleans and merges the data 
s2<-html_table(s, header = T, trim = TRUE, fill = FALSE, dec = ".")

s3<-data.frame(s2)

subset(s3, Var.1=="")
subset(s3, Var.1=="Rk")

s3<-s3[!(s3$Var.1==""),]
s3<-s3[!(s3$Var.1=="Rk"),]
s3<-s3[,-17]

s3<-mutate_all(s3, list(~na_if(.,"")))

colnames(s3)

names(s3)[which(names(s3) == "Var.1")] = "Rank"
names(s3)[which(names(s3) == "Var.2")] = "Team"
names(s3)[which(names(s3) == "Overall")] = "Overall.Wins"
names(s3)[which(names(s3) == "Overall.1")] = "Overall.Losses"
names(s3)[which(names(s3) == "Overall.2")] = "Overall.Wins.Loss.Percentage"
names(s3)[which(names(s3) == "Conference")] = "Conference.Wins"
names(s3)[which(names(s3) == "Var.3")] = "Conference"
names(s3)[which(names(s3) == "Conference.1")] = "Conference.Losses"
names(s3)[which(names(s3) == "Conference.2")] = "Conference.Win.Loss.Percentage"
names(s3)[which(names(s3) == "SRS")] = "Simple.Rating.System"
names(s3)[which(names(s3) == "SRS.1")] = "Strength.of.Schedule"
names(s3)[which(names(s3) == "Polls")] = "AP.Preseason.Rank"
names(s3)[which(names(s3) == "Polls.1")] = "AP.Highest.Rank"
names(s3)[which(names(s3) == "Polls.2")] = "AP.Final.Rank"
colnames(s3)

s3$Rank = NULL
s3$Conference = NULL
s3$Overall.Wins = NULL
s3$Overall.Losses = NULL
s3$Overall.Wins.Loss.Percentage = NULL
s3$Points.Per.Game = NULL
s3$Points.Per.Game.1 = NULL
df$Sack.Rank = NULL
colnames(s3)

df$Team = trimws(df$Team,"r")


#Regional Win Percentage 
df$Team[which(df$Team=="Appalachian St.")] = "Appalachian State"
df$Team[which(df$Team=="Arizona St.")] = "Arizona State"
df$Team[which(df$Team=="Appalachian St.")] = "Appalachian State"
df$Team[which(df$Team=="Arkansas St.")] = "Arkansas State"
df$Team[which(df$Team=="Appalachian St.")] = "Appalachian State"
df$Team[which(df$Team=="Army West Point")] = "Army"
df$Team[which(df$Team=="Ball St.")] = "Ball State"
df$Team[which(df$Team=="Boise St.")] = "Boise State"
df$Team[which(df$Team=="Bowling Green")] = "Bowling Green State"
df$Team[which(df$Team=="Central Mich.")] = "Central Michigan"
df$Team[which(df$Team=="Colorado St.")] = "Colorado State"
df$Team[which(df$Team=="Eastern Mich.")] = "Eastern Michigan"
df$Team[which(df$Team=="FIU")] = "Florida International"
df$Team[which(df$Team=="Fla. Atlantic")] = "Florida Atlantic"
df$Team[which(df$Team=="Florida St.")] = "Florida State"
df$Team[which(df$Team=="Fresno St.")] = "Fresno State"
df$Team[which(df$Team=="Ga. Southern")] = "Georgia Southern"
df$Team[which(df$Team=="Georgia St.")] = "Georgia State"
df$Team[which(df$Team=="Iowa St.")] = "Iowa State"
df$Team[which(df$Team=="Kansas St.")] = "Kansas State"
df$Team[which(df$Team=="Kent St.")] = "Kent State"
df$Team[which(df$Team=="La.-Monroe")] = "Louisiana-Monroe"
df$Team[which(df$Team=="Michigan St.")] = "Michigan State"
df$Team[which(df$Team=="Middle Tenn.")] = "Middle Tennessee State"
df$Team[which(df$Team=="Mississippi St.")] = "Mississippi State"
df$Team[which(df$Team=="NC State")] = "North Carolina State"
df$Team[which(df$Team=="New Mexico St.")] = "New Mexico State"
df$Team[which(df$Team=="Northern Ill.")] = "Northern Illinois"
df$Team[which(df$Team=="Ohio St.")] = "Ohio State"
df$Team[which(df$Team=="Oklahoma St.")] = "Oklahoma State"
df$Team[which(df$Team=="Oregon St.")] = "Oregon State"
df$Team[which(df$Team=="Penn St.")] = "Penn State"
df$Team[which(df$Team=="Pittsburgh")] = "Pitt"
df$Team[which(df$Team=="San Diego St.")] = "San Diego State"
df$Team[which(df$Team=="San Jose St.")] = "San Jose State"
df$Team[which(df$Team=="South Fla.")] = "South Florida"
df$Team[which(df$Team=="Southern California")] = "USC"
df$Team[which(df$Team=="Southern Miss.")] = "Southern Mississippi"
df$Team[which(df$Team=="TCU")] = "Texas Christian"
df$Team[which(df$Team=="Texas St.")] = "Texas State"
df$Team[which(df$Team=="UConn")] = "Connecticut"
df$Team[which(df$Team=="UNLV")] = "Nevada-Las Vegas"
df$Team[which(df$Team=="Utah St.")] = "Utah State"
df$Team[which(df$Team=="Washington St.")] = "Washington State"
df$Team[which(df$Team=="Western Ky.")] = "Western Kentucky"
df$Team[which(df$Team=="Western Mich.")] = "Western Michigan"


cfb_df = merge(df, s3,
               by.x = "Team", by.y = "Team")

cfb_df$Conference.Wins = as.integer(cfb_df$Conference.Wins)
cfb_df$Conference.Losses = as.integer(cfb_df$Conference.Losses)
cfb_df$Conference.Win.Loss.Percentage = as.numeric(cfb_df$Conference.Win.Loss.Percentage)
cfb_df$Simple.Rating.System = as.numeric(cfb_df$Simple.Rating.System)
cfb_df$Strength.of.Schedule = as.numeric(cfb_df$Strength.of.Schedule)
cfb_df$AP.Preseason.Rank = as.numeric(cfb_df$AP.Preseason.Rank)
cfb_df$AP.Highest.Rank = as.numeric(cfb_df$AP.Highest.Rank)
cfb_df$AP.Final.Rank = as.numeric(cfb_df$AP.Final.Rank)


cfb_df$State = c("CO", "OH", "AL", "NC", "AZ", "AZ", "AR", "AR", "NY", "AL", "IN", "TX", "ID", "MA",
                 "OH", "NY", "UT", "CA", "MI", "NC", "OH", "SC", "SC", "CO", "CO", "CT",
                 "NC", "NC", "MI", "FL", "FL", "FL", "FL", "CA", "GA", "GA",
                 "GA", "GA", "HI", "TX", "IL", "IN", "IA", "IA", "KS", "KS", "OH", "KY", "VA",
                 "LA", "LA", "LA", "KY", "LA", "WV", "MD", "MA", "TN", "FL", "OH", "MI",
                 "MI", "TN", "MN", "MS", "MO", "MD", "NE", "NV", "NV", "NM", "NM",
                 "NC", "NC", "TX", "IL", "IL", "IN", "OH", "OH", "OK", "OK",
                 "VA", "MS", "OR", "OR", "PA", "PA", "IN", "TX", "NJ", "CA", "CA", "TX", "AL",
                 "SC", "FL", "MS", "CA", "NY", "PA", "TN", "TX", "TX", "TX", "TX", "TX", "OH",
                 "AL", "LA", "OK", "AL", "FL", "CA", "CA", "UT", "UT", "TX", "TX", "TN", "VA", "VA", "NC", "WA",
                 "WA", "WV", "KY", "MI", "WI", "WY")

cfb_df = group_by(cfb_df, Conference)
Conference_Summary = summarize(cfb_df,
                               TotalGames = sum(Games),
                               TotalWins = sum(Wins),
                               TotalLosses = sum(Losses),
                               MeanWins = mean(Wins),
                               WinPerc = (TotalWins/TotalGames)*100)


Conference_Summary$MeanWins = round(Conference_Summary$MeanWins, digits = 3)
Conference_Summary$WinPerc = round(Conference_Summary$WinPerc, digits = 1)

cfb_df$Region = cfb_df$State
cfb_df$Region = as.factor(cfb_df$Region)

levels(cfb_df$Region)[which(levels(cfb_df$Region) == "OH" | levels(cfb_df$Region) == "MI" | levels(cfb_df$Region) == "IN" |
                              levels(cfb_df$Region) == "IL" | levels(cfb_df$Region) == "MN" | levels(cfb_df$Region) == "IA" |
                              levels(cfb_df$Region) == "MO" | levels(cfb_df$Region) == "ND" | levels(cfb_df$Region) == "WI" |
                              levels(cfb_df$Region) == "SD" | levels(cfb_df$Region) == "NE" |
                              levels(cfb_df$Region) == "KS")] = "Midwest"
levels(cfb_df$Region)[which(levels(cfb_df$Region) == "ME" | levels(cfb_df$Region) == "NH" | levels(cfb_df$Region) == "VT" |
                              levels(cfb_df$Region) == "MA" | levels(cfb_df$Region) == "RI" | levels(cfb_df$Region) == "CT" |
                              levels(cfb_df$Region) == "NY" | levels(cfb_df$Region) == "NJ" | 
                              levels(cfb_df$Region) == "PA")] = "Northeast"
levels(cfb_df$Region)[which(levels(cfb_df$Region) == "DE" | levels(cfb_df$Region) == "MD" | levels(cfb_df$Region) == "VA" |
                              levels(cfb_df$Region) == "VA" | levels(cfb_df$Region) == "WV" | levels(cfb_df$Region) == "KY" |
                              levels(cfb_df$Region) == "NC" | levels(cfb_df$Region) == "SC" | 
                              levels(cfb_df$Region) == "TN" | levels(cfb_df$Region) == "GA" |
                              levels(cfb_df$Region) == "FL" | levels(cfb_df$Region) == "AL" | levels(cfb_df$Region) == "MS" |
                              levels(cfb_df$Region) == "AR" | levels(cfb_df$Region) == "LA" | levels(cfb_df$Region) == "TX" |
                              levels(cfb_df$Region) == "OK")] = "South"
levels(cfb_df$Region)[which(levels(cfb_df$Region) == "MT" | levels(cfb_df$Region) == "ID" | levels(cfb_df$Region) == "WY" |
                              levels(cfb_df$Region) == "CO" | levels(cfb_df$Region) == "NM" | levels(cfb_df$Region) == "AZ" |
                              levels(cfb_df$Region) == "UT" | levels(cfb_df$Region) == "NV" | 
                              levels(cfb_df$Region) == "CA" | levels(cfb_df$Region) == "OR" |
                              levels(cfb_df$Region) == "WA" | levels(cfb_df$Region) == "AK" | levels(cfb_df$Region) == "HI")] = "West"

levels(cfb_df$Region)

cfb_df = ungroup(cfb_df)
cfb_df = group_by(cfb_df, Region)
Region_Summary = summarize(cfb_df,
                           TotalGames = sum(Games),
                           TotalWins = sum(Wins),
                           TotalLosses = sum(Losses),
                           MeanWins = mean(Wins),
                           WinPerc = (TotalWins/TotalGames)*100)

Region_Summary$MeanWins = round(Region_Summary$MeanWins, digits = 3)
Region_Summary$WinPerc = round(Region_Summary$WinPerc, digits = 1)

library(ggplot2)

qplot(Conference,
      MeanWins,
      data = Conference_Summary,
      geom = "col",
      aes(reorder(Conference, -MeanWins)),
      fill = I("gold"),
      colour = I("black"),
      main = "Mean Number of Wins by Conference") +
  theme(axis.text.x = element_text(
    angle = 90, 
    vjust=0.5)) +
  geom_text(aes(label=MeanWins), size=2.75, vjust=-0.4)


qplot(Region,
      MeanWins,
      data = Region_Summary,
      geom = "col",
      aes(reorder(Region, -MeanWins)),
      fill = I("gold"),
      colour = I("black"),
      main = "Mean Number of Wins by Region") +
  theme(axis.text.x = element_text(
    angle = 90, 
    vjust=0.5)) +
  geom_text(aes(label=MeanWins), size=2.75, vjust=-0.4)


str(cfb_df)

#Statistical Analysis on Time of Possession 
#H0: Time of Possession and Offesnvie rank will be highly correlated p > .8
#H1: TOP and Offensive rank will not be correlated p < .8
cor(cfb_df$Time.of.Possession.Rank, cfb_df$Off.Rank)
#.0545 is a very weak correlation between the time of possession and the team's offensive rank
marks_mod = lm(cfb_df$Off.Rank ~ cfb_df$Time.of.Possession.Rank)
marks_mod
summary(marks_mod)
#p-value indicates that time of possession rank is not a good indicator of a team's overall offense rank
#Null Hypothesis Wrong



#H0: Time of Possession and Offensive rank will be highly correlated p > .5
#H1: TOP and Offensive Rank will not be highly correlated p < .5
cor(cfb_df$Time.of.Possession.Rank, cfb_df$Def.Rank)
#.5272 indicates that correlation between the time of possession and the team's defensive rank is strong
marks_mod2 = lm(cfb_df$Def.Rank ~ cfb_df$Time.of.Possession.Rank)
marks_mod2
summary(marks_mod2)
#p-value (.5377) indicates that time of possession rank is a good indicator of a team's overall defensive ranking
#more time of possession -> less time on defense -> more refreshed defense

#AP Poll Top 25 by Conference 
Ranking_df<- data.frame(Team = cfb_df$Team, Conference = cfb_df$Conference, 
                        AP.Preseason.Rank = cfb_df$AP.Preseason.Rank, 
                        Final.Rank = cfb_df$AP.Final.Rank)
Ranking_df<- Ranking_df %>% filter(!is.na(AP.Preseason.Rank) | !is.na(Final.Rank))
#This is a subset of the AP Preseason Ranking that show each team at the beginning of the 
#season. With this subset we are able to see how many teams each conference had in the 
#preseason top 25 without NA values. The legend also shows us what rankings each team had. 
Pre_Ranking_Subset <- subset(Ranking_df,
                             AP.Preseason.Rank >= 1)
qplot(Conference, 
      data = Pre_Ranking_Subset,
      geom = "bar",
      ylab = "Number of Teams",
      fill = AP.Preseason.Rank,
      colour = I("black")) + 
  theme(axis.text.x = element_text(
    angle = 90, 
    vjust=0.5))

#This is a subset of the final rankings of the 2019 college football season. The plot shows
#how many teams ended in each conference and what their rankings were, represented 
#in the legend. The split of the graph allows the graph to be displayed without NA values. 
Final_Ranking_Subset<- subset(Ranking_df, 
                              Final.Rank >= 1)
qplot(Conference, 
      data = Final_Ranking_Subset,
      geom = "bar",
      ylab = "Number of Teams",
      fill = Final.Rank,
      colour = I("black")) +
  theme(axis.text.x = element_text(
    angle = 90, 
    vjust=0.5))















