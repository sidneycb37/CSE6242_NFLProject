# This script will focus on integrating and cleaning CFB game data
# To run on new machines, the data folder names (lines 11-13) should be edited
# last updated: 4/13/2020 by Sidney
# Changes: Cleaned up code for final submission. Changed some comments, revised folder locations

# remove past variables
rm(list=ls())

# need to specify folder holding all data to be cleaned and where you want clean data to be placed
### IF RUNNING ON A NEW MACHINE, CHANGE THE BELOW LINE TO REFLECT DATA LOCATION ###
dataFolder = 'C:/Users/sidne/OneDrive/Documents/GeorgiaTech/DVA/Project/CODE/Data';
dirtyDataLoc = paste(dataFolder,'/rawData/',sep="");
cleanDataLoc = paste(dataFolder,'/cleanData/',sep="");

library(dplyr)

# go to the dirty data location
setwd(dirtyDataLoc)

load(paste(cleanDataLoc,"nfl_cfb_combine.rdata",sep=""));


# find all rdata files with the letters 'CFB' in them 
# note: rdata files use capital CFB, csv files use lowercase cfb
rDataFiles <- list.files(pattern = "\\.rdata$");
cfbRFiles <- intersect(list.files(pattern = "CFB"),rDataFiles);
csvFiles <- list.files(pattern="\\.csv$");
cfbCsvFiles <- intersect(list.files(pattern="cfb"),csvFiles);



#dataset 1: import and clean the team rankings for conference use
teamRankDf = data.frame();
# find all stat, cfb, csv files
rankFiles = intersect(cfbCsvFiles, list.files(pattern="rank"));
# iterate throguh each stat file
for( fileName in rankFiles){
  # load the csv file
  data_file = read.csv(fileName, stringsAsFactors = FALSE);
  # find the season it corresponds to
  data_file$season = substr(fileName,(nchar(fileName)+1)-8,nchar(fileName)-4); #dont need this?
  # append to the overall data frame
  teamRankDf = rbind(teamRankDf, data_file);
  # clear teh data file to avoid any issues
  rm(data_file)
}
summary(teamRankDf);

#List of college and the conferences they are associated with in a particular year
school_conf <- unique(teamRankDf[,c("school","conference","season")])

#Including only post season
teamRankDf = teamRankDf[teamRankDf$seasonType=='postseason',] 
#Including only Associated Press Poll
teamRankDf = teamRankDf[teamRankDf$poll_name=='AP Top 25',] 
# Including only relevant fields:
teamRankDf <- teamRankDf %>% select(rank,
                                    school,
                                    conference,
                                    season)

summary(teamRankDf)



# Extracting the ranking of college of NFL players of interest in their draft_year

school_rank <- merge(x=recruitRoster_final,y=teamRankDf %>% select(-conference),by.x=c("college","draft_year"),by.y=c("school","season"),all.x=TRUE)

#Lets see how many rows have missing ranks:
nrow(school_rank[is.na(school_rank$rank),])
#88 out of 130 rows have ranks missing. That means the respective colleges are not in AP top 25. Going to fill in them with rank 26.
school_rank[is.na(school_rank$rank),"rank"]=26

#dataset 2: import and clean drive data
driveDf = data.frame();
# find all stat, cfb, csv files
driveFiles = intersect(cfbRFiles, list.files(pattern="Drive"));
# iterate throguh each stat file
for( fileName in driveFiles){
  # load the rdata file
  load(fileName);
  # find the season it corresponds to
  data_file$season = substr(fileName,(nchar(fileName)+1)-10,nchar(fileName)-6);
  # append to the overall data frame
  driveDf = rbind(driveDf, data_file);
  # clear teh data file to avoid any issues
  rm(data_file)
}
summary(driveDf)

#Extracting conferences for schools associated with NFL players of interest
driveDf_o <- driveDf %>% select(offense,offense_conference,season) %>% rename(team=offense,conf=offense_conference)
driveDf_d <- driveDf %>% select(defense,defense_conference,season) %>% rename(team=defense,conf=defense_conference)

team_conf <- unique(rbind(driveDf_o[complete.cases(driveDf_o),],driveDf_d[complete.cases(driveDf_d),]))

#Merge with existing NFL players data to add in conference associated with player's college in draft year
nfl_conf <- merge(x=school_rank,y=team_conf,by.x=c("college","draft_year"),by.y=c("team","season"),all.x=TRUE)

#Which rows are missing conferences
test_conf <-unique(nfl_conf[,c("college","draft_year","conf")])
test_conf[is.na(test_conf$conf),]
#Only 1 row missing conference whose college is wake forest. Are there any other rows with same name but different draft year
nfl_conf[nfl_conf$college=="Wake Forest",]
#Assuming same conference in the 2 years, let us assign missing conference
nfl_conf[nfl_conf$college=="Wake Forest"&nfl_conf$draft_year==2012,"conf"]="ACC"

save(nfl_conf,file=paste(cleanDataLoc,"NFL_Featureselection_w_imputeflags.rdata",sep=""))

#Let us have a dataset without the impute flags in case we need for brevity
col_impute <- grep("impute",colnames(nfl_conf))
nfl_conf_noimp <- nfl_conf[,-col_impute]

save(nfl_conf_noimp,file=paste(cleanDataLoc,"NFL_Featureselection_wo_imputeflags.rdata",sep=""))

