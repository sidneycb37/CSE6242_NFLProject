# This script will focus on integrating and cleaning NFL and combine data
# To run on new machines, the data folder names (lines 13-15) should be edited
# last updated: 4/13/2020 by Sidney
# Changes: Cleaned up code for final submission. Changed some comments, revised folder locations

# ensure clean workspace
rm(list=ls())

# need to specify folder holding all data to be cleaned and where you want clean data to be placed
### IF RUNNING ON A NEW MACHINE, CHANGE THE BELOW LINE TO REFLECT DATA LOCATION ###
dataFolder = 'C:/Users/sidne/OneDrive/Documents/GeorgiaTech/DVA/Project/CODE/Data';
dirtyDataLoc = paste(dataFolder,'/rawData/',sep="");
cleanDataLoc = paste(dataFolder,'/cleanData/',sep="");


#list of required additional packages beyond base R
require(dplyr)
require(tidyr)


# go to the dirty data location
setwd(dirtyDataLoc)

# find all rdata files with the letters 'nfl' in them
rDataFiles <- list.files(pattern = "\\.rdata$");
nflRFiles <- intersect(list.files(pattern = "NFL"),rDataFiles);
csvFiles <- list.files(pattern = "\\.csv$");
nflCsvFiles <- intersect(list.files(pattern = "NFL"),csvFiles);



# dataset 1: read in all NFL roster files
allRosterDf = data.frame();
# find all roster, NFL, rData files
rosterFiles = intersect(nflRFiles, list.files(pattern="Roster"));
# iterate through each roster file
for( fileName in rosterFiles ){
  # load the rdata file
  load(fileName)
  data_file <- as.data.frame(data_file)
  # append to the overall data frame
  allRosterDf <- rbind(data_file, allRosterDf);
  # clear the datafile to avoid any issues
  rm(data_file);
}
# check the positions
unique(allRosterDf$Pos)
# looks like "WR" is the only possible position
# remove any players that are not a wide receiver
# in the roster data set, it is the 'Pos' position
rosterWR = allRosterDf[allRosterDf$Pos=="WR",];
# list rows of data that have missing values
length(rosterWR[!complete.cases(rosterWR),'name'])
# no longer need position
rosterWR <- rosterWR %>% select(-Pos)
# looks good, no incomplete cases
summary(rosterWR)

#remove unnecessary dataframes
rm(allRosterDf)

# dataset 2: read in all NFL player stats files
allStatDf = data.frame();
# find all stat, NFL, rData files
statFiles = intersect(nflRFiles, list.files(pattern="Stat"));
# iterate through each stat file
for(fileName in statFiles){
  # load the file
  load(fileName)
  # append to the overall data frame
  allStatDf = rbind(allStatDf,data_file)
  # clear the datafile to avoid any issues
  rm(data_file);
}
# check for any incomplete rows
allStatDf[!complete.cases(allStatDf),];
# one player (ID = 1) does not have a team listed and their name is '0'
# assuming this is a fluke, so omit these lines
allStatDf = allStatDf[complete.cases(allStatDf),];
#remove fields we don't need
allStatDf <- allStatDf %>% select(-tackles,
                                  -asst.tackles,
                                  -sacks,
                                  -defints,
                                  -forced.fumbs,
                                  -xpmade,
                                  -xpmissed,
                                  -xpa,
                                  -xpb,
                                  -xppts.tot)
summary(allStatDf)






# join the stat dataset to the roster data set
# keep all of the rows of the x/roster set (this makes sure that only WR are selected)
# make sure the players line up along the id, name, season, and team
rosterStatWR = merge(x = rosterWR, y = allStatDf, by.x = c("GSIS_ID","Season","Team"), by.y=c("playerID","Season","Team"), all.x = TRUE)

# delete one of the name columns
rosterStatWR$name.y = NULL;
# rename the other name column
names(rosterStatWR)[names(rosterStatWR) == 'name.x'] <- 'name'
# sanity check join quality
summary(rosterStatWR)
#got 51 no hits - who are they
#after review and research of a few - these are garbage seasons where a player made a roster but did not record a stat :(
#fix these by setting all their stats to 0 and move on
test <- rosterStatWR[is.na(rosterStatWR$pass.att),]
rm(test)
for(i in 1:ncol(rosterStatWR)){
  if (is.numeric(rosterStatWR[,i])){
    rosterStatWR[is.na(rosterStatWR[,i]), i] <- 0
  }
}
summary(rosterStatWR)



# dataset 3: add the draft data (csv only, not rdata)
# list all files
draftFiles = intersect(list.files(pattern = "Draft"),nflCsvFiles);
# should only be one file
if (length(draftFiles)>1)
{
  stop('too many draft files found');
}
# load the file
draftDf = read.csv(draftFiles, stringsAsFactors = F)
# check all the positions
unique(draftDf$pos)
# filter by "WR" position
draftDf = draftDf[draftDf$pos=='WR',];
# take out players that don't have teams (they weren't drafted)
draftDf = draftDf[complete.cases(draftDf$team),];

#*****-edits by SS-*****

#Let us see if defense characteristics are all 0 for WR
draftDf[draftDf$defense.games!=0,]
#After detailed look through, defense characteristics are 0 for all players except one - Buster Davis who may have been misplaced
#in the data as WR. This can be confirmed as his receiving charateristics are all 0. Also wiki says he is a linebacker.
draftDf[draftDf$player=='Buster Davis',]
#Removing this data point from the set

draftDf = draftDf[draftDf$player!='Buster Davis',]

#*****-end of edits by SS-*****

# check that there are no missing values
draftDf[draftDf$missing_combine_vals,]
# Almost every player has a value/multiple combine stats missing
#remove fields we don't need #edit by SS - removed url,pos and defense seasons further
draftDf <- draftDf %>% select(-url,
                              -pos,
                              -defense.ast.tackles,
                              -defense.fum.forced,
                              -defense.fum.rec,
                              -defense.fum.tds,
                              -defense.fum.yds,
                              -defense.games,
                              -defense.int,
                              -defense.int.td,
                              -defense.int.yards,
                              -defense.loss.tackles,
                              -defense.pd,
                              -defense.sacks,
                              -defense.seasons,
                              -defense.solo.tackes,
                              -defense.tackles,
                              -num_murders,
                              -purple.drank.incidents,
                              -passing.attempts,
                              -passing.comp.pct,
                              -passing.completions,
                              -passing.games,
                              -passing.pass.ints,
                              -passing.pass.tds,
                              -passing.pass.yards,
                              -passing.seasons,
                              -receiving.games,
                              -receiving.rec.td,
                              -receiving.rec.yards,
                              -receiving.receptions,
                              -receiving.rush.att,
                              -receiving.rush.td,
                              -receiving.rush.yds,
                              -receiving.scrim.plays,
                              -receiving.scrim.tds,
                              -receiving.scrim.yds,
                              -receiving.seasons,
                              -rushing.games,
                              -rushing.rec.td,
                              -rushing.rec.yards,
                              -rushing.receptions,
                              -rushing.rush.att,
                              -rushing.rush.td,
                              -rushing.rush.yds,
                              -rushing.scrim.plays,
                              -rushing.scrim.tds,
                              -rushing.scrim.yds,
                              -rushing.seasons)

summary(draftDf)

# Assume missing values are averages
# add a flag to show which values were imputed
for(i in 1:ncol(draftDf)){
  #numeric fields only
  if (is.numeric(draftDf[,i])){
    #flag missing values and create imputation flag in case we need it later
    varname <- paste(colnames(draftDf)[i], "_impute_flag", sep = "")
    assign('temp', is.na(draftDf[,i]) * 1) #this turns boolean to numeric flag and assigns to variable temp
    draftDf$tempcol <- temp
    colnames(draftDf) <- sub("tempcol", varname, colnames(draftDf))
    
    #impute to average
    draftDf[is.na(draftDf[,i]), i] <- mean(draftDf[,i], na.rm = TRUE)
    }
}
summary(draftDf)


#****EDITS BY SS****
#Merging draftDf and rosterStatWR w.r.t player and team name first

#Checking if teams are equal in draftdf and rosterStatWR 
sort(unique(rosterStatWR$Team))
sort(unique(draftDf$team))

#JAC name was officially changed to JAX
rosterStatWR[rosterStatWR$Team=="JAC","Team"]="JAX"


#Resolving team names in draftDf file to match with rosterstatWR team names
draftDf[draftDf$team=="GNB","team"]="GB"
draftDf[draftDf$team=="LAR","team"]="LA"
draftDf[draftDf$team=="KAN","team"]="KC"
draftDf[draftDf$team=="NOR","team"]="NO"
draftDf[draftDf$team=="NWE","team"]="NE"
draftDf[draftDf$team=="SDG","team"]="SD"
draftDf[draftDf$team=="SFO","team"]="SF"
draftDf[draftDf$team=="TAM","team"]="TB"

all(sort(unique(rosterStatWR$Team))==sort(unique(draftDf$team)))
#Now the team names match

#Merging first with both team and player name
nflDf_attempt1=merge(x=rosterStatWR, y=draftDf, by.x=c("Team","Player"), by.y=c("team","player"), all.x=TRUE) 
summary(nflDf_attempt1)
nflDf_attempt1$team=nflDf_attempt1$Team

#There are 1326 rows that couldnt be matched. Lets try merging them by matching them by player name only.
match_cases = nflDf_attempt1[(complete.cases(nflDf_attempt1[,(ncol(rosterStatWR)+1):ncol(nflDf_attempt1)])),]
NA_cases = nflDf_attempt1[!(complete.cases(nflDf_attempt1[,(ncol(rosterStatWR)+1):ncol(nflDf_attempt1)])),1:ncol(rosterStatWR)]

nflDf_attempt2 = merge(x=NA_cases, y=draftDf, by.x=c("Player"), by.y=c("player"), all.x=TRUE);
summary(nflDf_attempt2)
#There are still 938 rows that couldnt be matched

#Checking if multiple players have same name
length(unique(nflDf_attempt2$Player))
length(unique(nflDf_attempt2$GSIS_ID))
#Looks like few players have same names. Lets check who they are
temp = merge(x=nflDf_attempt2[,c("GSIS_ID","Player","Season","Team","name")],y=rosterStatWR[,c("GSIS_ID","Player")],by=c("Player"))
unique(temp[temp$GSIS_ID.x!=temp$GSIS_ID.y,])

#Attach nflDf_attempt1 and nflDf_attempt2 to consolidate both methods followed above
nflDf=rbind(nflDf_attempt1[(complete.cases(nflDf_attempt1[,(ncol(rosterStatWR)+1):ncol(nflDf_attempt1)])),],nflDf_attempt2)

#2 people with same name Mike Williams, 2 people with same name Steve Smith, 2 people with same name Mike Thomas

#Cleaning up the stats related to these people 
draftDf[draftDf$player=="Mike Thomas",]
nflDf[nflDf$Player=="Mike Thomas",c("Team","Player","GSIS_ID","Season")]
#Mike Thomas with id 00-0027076 is the one in draft file. Remove values for other one
for (col in colnames(draftDf)){
  nflDf[nflDf$GSIS_ID=="00-0033114",col]=NA
}

draftDf[draftDf$player=="Steve Smith",]
nflDf[nflDf$Player=="Steve Smith",c("Team","Player","GSIS_ID","Season")]
#Match appropriate players
for (col in colnames(draftDf)){
  nflDf[(nflDf$GSIS_ID=="00-0020337")&(nflDf$Team=="BAL"),col]=nflDf[(nflDf$GSIS_ID=="00-0020337")&(nflDf$Team=="CAR")&(nflDf$Season==2010),col]
  nflDf[(nflDf$GSIS_ID=="00-0025438")&((nflDf$Team=="STL")|(nflDf$Team=="PHI")),col]=nflDf[(nflDf$GSIS_ID=="00-0025438")&(nflDf$Team=="NYG"),col]
}

nflDf=nflDf[!duplicated(nflDf),]

draftDf[draftDf$player=="Mike Williams",]
nflDf[nflDf$Player=="Mike Williams",c("Team","Player","GSIS_ID","Season")]
#Match appropriate players
for (col in colnames(draftDf)){
  nflDf[(nflDf$GSIS_ID=="00-0027702")&(nflDf$Team=="BUF"),col]=nflDf[(nflDf$GSIS_ID=="00-0027702")&(nflDf$Team=="TB")&(nflDf$Season==2010),col]
}

nflDf=nflDf[!((nflDf$GSIS_ID=="00-0023452")&((nflDf$height_inches==76)|(nflDf$height_inches==73))),]

nflDf=nflDf[!duplicated(nflDf),]
nflDf$player=NULL;

#Lets check how many players in stat file have combine measures missing
combine_NA_cases = nflDf[!(complete.cases(nflDf[,(ncol(rosterStatWR)+1):ncol(nflDf_attempt1)])),]
length(unique(combine_NA_cases$Player))
length(unique(nflDf$Player))

#Note - Combine data of 387 players missing out of 655 players in the nfl player stat data

#****END OF EDITS BY SS****

# save player nfl dataframe in the clean data location
save(nflDf,file=paste(cleanDataLoc,"nflPlayerData.rdata", sep = ""));

# last dataset: load nfl game day data
allGameDf = data.frame();
# find all stat, cfb, csv files
gameFiles = intersect(nflCsvFiles, list.files(pattern="Game"));
# iterate throguh each game day file
yr <- 2009
for( fileName in gameFiles){
  # load the csv file
  data_file = read.csv(fileName, stringsAsFactors = F);
  data_file$Season <- yr
  #remove fields we don't need
  data_file <- data_file %>% select(-X,
                                    -field_goal_result,
                                    -kick_distance,
                                    -extra_point_result,
                                    -no_score_prob,
                                    -opp_fg_prob,
                                    -opp_safety_prob,
                                    -opp_td_prob,
                                    -fg_prob,
                                    -safety_prob,
                                    -td_prob,
                                    -extra_point_prob,
                                    -two_point_conversion_prob,
                                    -punt_blocked,
                                    -first_down_rush,
                                    -first_down_pass,
                                    -first_down_penalty,
                                    -third_down_converted,
                                    -third_down_failed,
                                    -fourth_down_converted,
                                    -fourth_down_failed,
                                    -touchback,
                                    -punt_inside_twenty,
                                    -punt_in_endzone,
                                    -punt_out_of_bounds,
                                    -punt_downed,
                                    -punt_fair_catch,
                                    -kickoff_inside_twenty,
                                    -kickoff_in_endzone,
                                    -kickoff_out_of_bounds,
                                    -kickoff_downed,
                                    -kickoff_fair_catch,
                                    -solo_tackle,
                                    -safety,
                                    -own_kickoff_recovery,
                                    -own_kickoff_recovery_td,
                                    -extra_point_attempt,
                                    -field_goal_attempt,
                                    -kickoff_attempt,
                                    -punt_attempt,
                                    -assist_tackle,
                                    -defensive_extra_point_conv,
                                    -defensive_extra_point_attempt,
                                    -defensive_two_point_conv,
                                    -defensive_two_point_attempt,
                                    -return_yards,
                                    -return_team,
                                    -assist_tackle_4_team,
                                    -assist_tackle_4_player_name,
                                    -assist_tackle_4_player_id,
                                    -assist_tackle_3_team,
                                    -assist_tackle_3_player_name,
                                    -assist_tackle_3_player_id,
                                    -assist_tackle_2_team,
                                    -assist_tackle_2_player_name,
                                    -assist_tackle_2_player_id,
                                    -assist_tackle_1_team,
                                    -assist_tackle_1_player_name,
                                    -assist_tackle_1_player_id,
                                    -solo_tackle_2_player_name,
                                    -solo_tackle_2_player_id,
                                    -solo_tackle_2_team,
                                    -solo_tackle_1_player_name,
                                    -solo_tackle_1_player_id,
                                    -solo_tackle_1_team,
                                    -pass_defense_2_player_name,
                                    -pass_defense_2_player_id,
                                    -pass_defense_1_player_name,
                                    -pass_defense_1_player_id,
                                    -forced_fumble_player_2_player_name,
                                    -forced_fumble_player_2_player_id,
                                    -forced_fumble_player_2_team,
                                    -forced_fumble_player_1_player_name,
                                    -forced_fumble_player_1_player_id,
                                    -forced_fumble_player_1_team,
                                    -qb_hit_2_player_name,
                                    -qb_hit_2_player_id,
                                    -qb_hit_1_player_name,
                                    -qb_hit_1_player_id,
                                    -tackle_for_loss_2_player_name,
                                    -tackle_for_loss_2_player_id,
                                    -tackle_for_loss_1_player_name,
                                    -tackle_for_loss_1_player_id,
                                    -blocked_player_name,
                                    -blocked_player_id,
                                    -own_kickoff_recovery_player_name,
                                    -own_kickoff_recovery_player_id,
                                    -kicker_player_id,
                                    -kicker_player_name,
                                    -punter_player_name,
                                    -punter_player_id,
                                    -lateral_kickoff_returner_player_name,
                                    -lateral_kickoff_returner_player_id,
                                    -kickoff_returner_player_id,
                                    -kickoff_returner_player_name,
                                    -lateral_punt_returner_player_name,
                                    -lateral_punt_returner_player_id,
                                    -interception_player_name,
                                    -interception_player_id,
                                    -lateral_sack_player_name,
                                    -lateral_sack_player_id,
                                    -rusher_player_name,
                                    -rusher_player_id,
                                    -passer_player_name,
                                    -passer_player_id,
                                    -two_point_attempt,
                                    -qb_kneel,
                                    -qb_scramble,
                                    -two_point_conv_result,
                                    -no_huddle,
                                    -run_gap,
                                    -home_timeouts_remaining,
                                    -away_timeouts_remaining,
                                    -timeout,
                                    -timeout_team,
                                    -posteam_timeouts_remaining,
                                    -defteam_timeouts_remaining,
                                    -tackled_for_loss,
                                    -rush_attempt,
                                    -total_home_rush_epa,
                                    -total_away_rush_epa,
                                    -total_home_rush_wpa,
                                    -total_away_rush_wpa,
                                    -rush_touchdown,
                                    -return_touchdown,
                                    -lateral_reception,
                                    -lateral_rush,
                                    -lateral_return,
                                    -lateral_recovery,
                                    -lateral_receiver_player_id,
                                    -lateral_interception_player_id,
                                    -punt_returner_player_id,
                                    -lateral_receiver_player_name,
                                    -lateral_interception_player_name,
                                    -punt_returner_player_name)

  # append to the overall data frame,
  allGameDf = rbind(allGameDf, data_file);
  # clear the data file to avoid any issues
  rm(data_file)
  #move season counter forward a year
  yr <- yr + 1
}
rm(yr)
# isolate only passing plays on offense
allGameDf <- allGameDf %>% filter(play_type == "pass")
# isolate plays that involve a wide receiver
gameDfWr = data.frame();
# first, find all columns in the game day data that involve a player id
allCols = names(allGameDf);
playerIdCols = allCols[grepl( "player_id" , names(allGameDf) )]
# compare list of all WR id's to all id's mentioned in game play columns
for (colName in playerIdCols){
  i <- grep(colName, colnames(allGameDf))
  # add any matching rows to the larger dataframe
  temp <- allGameDf %>% drop_na(colName)
  #colnames(temp)[i] <- "GSIS_ID"
  temp <- merge(temp, nflDf %>% select(GSIS_ID) %>% distinct(), by.x = colName, by.y = "GSIS_ID")
  temp$GSIS_ID <- temp[,c(colnames(allGameDf)[i])]
  gameDfWr <- rbind(gameDfWr,
                   #allGameDf[allGameDf[colName] %in% nflDf$GSIS_ID,]
                   temp);
  print(colName)
  print(nrow(temp))
  # do inside loop to prevent data from getting overly large and crashing limited system resources
  # remove any duplicate rows (could have been accidentally been added twice if a WR did multiple things in a play)
  gameDfWr <- gameDfWr %>% distinct(play_id, game_id, game_date, .keep_all = T);
  
}
#memory management - remove unfiltered large play by play dataset
rm(allGameDf)

# in this dataset, an 'NA' is valid, meaning that no player with the column name category contributed to the play
# ex. sack column will be na if no one was sacked
# check if any of these columns is always empty and delete it
for (colName in playerIdCols){
  if (length(gameDfWr[!is.na(gameDfWr[,colName]),colName]) == 0){
    gameDfWr[,colName] = NULL;
  }
}

#save play by play data with only WRs
save(gameDfWr,file=paste(cleanDataLoc,"nflPlayerPlayByPlayData.rdata", sep = ""))

#allocate win probability and EPA to WRs
#as long as the player doesn't commit a penalty, allocate WRs 14.65% of pass play EPA/WPA
#based on relative player salaries
#if a player commits a penalty, they deserve all the blame and recieve 100% of the negative EPA/WPA
gameDfWr <- gameDfWr %>% mutate(epa_allocate = ifelse((penalty == 1) & (GSIS_ID == penalty_player_id), 0, epa * .1465),
                                wpa_allocate = ifelse((penalty == 1) & (GSIS_ID == penalty_player_id), 0, wpa * .1465))


#roll up by GSIS ID
gsis_id_data <- gameDfWr %>% group_by(GSIS_ID, Season) %>% summarise(epa_allocate = sum(epa_allocate),
                                                                     wpa_allocate = sum(wpa_allocate))
gsis_id_data <- gsis_id_data %>% mutate(epa_allocate = ifelse(is.na(epa_allocate), 0, epa_allocate),
                                        wpa_allocate = ifelse(is.na(wpa_allocate), 0, wpa_allocate))

#for drafted players - roll up the next 3 seasons after a player was drafted as target
gsis_id_data <- merge(gsis_id_data, nflDf %>% filter(!(is.na(year))) %>% select(GSIS_ID, year, Player, Season) %>% rename(draft_year = year), by = c("GSIS_ID", "Season"))
gsis_player_data <- gsis_id_data %>% filter((Season - draft_year >= 0) & (Season - draft_year <= 3)) %>% 
                                      group_by(GSIS_ID, Player, draft_year) %>%
                                      summarise(epa_3season = sum(epa_allocate),
                                                wpa_3season = sum(wpa_allocate))

#focus on draft year in 2012 or later
gsis_player_data <- gsis_player_data %>% filter(draft_year >= 2012) 

#save season level EPA and WPA allocated data
save(gsis_player_data, file = paste(cleanDataLoc, "nflPlayerEPAWPA.rdata", sep = ""))


Feature_selec <- merge(x=gsis_player_data,y=nflDf[,c("GSIS_ID","pick","college","height_inches","weight",
                                                     "forty","vertical","bench","broad","threecone",
                                                     "shuttle","height_inches_impute_flag","weight_impute_flag",
                                                     "forty_impute_flag","vertical_impute_flag","bench_impute_flag",
                                                     "broad_impute_flag","threecone_impute_flag",
                                                     "shuttle_impute_flag")],by="GSIS_ID",all.x=TRUE)
Feature_selec <- Feature_selec[!duplicated(Feature_selec),]

save(Feature_selec, file = paste(cleanDataLoc, "nflPlayerEPAWPA_combine.rdata", sep = ""))



#remove intermediate data we don't need
rm(allStatDf)
rm(rosterStatWR)
rm(NA_cases)
rm(combine_NA_cases)
rm(nflDf_attempt1)
rm(nflDf_attempt2)
rm(temp)
rm(match_cases)
