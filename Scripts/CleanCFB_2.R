
# This script will focus on integrating and cleaning CFB data
# if running on a new machine, change lines 12-14 as necessary
# last updated: 4/13/2020 by Sidney
# Changes: Cleaned up code for final submission. Changed some comments,  revised folder locations

# remove past variables
rm(list=ls())

# need to specify folder holding all data to be cleaned and where you want clean data to be placed
### IF RUNNING ON A NEW MACHINE, CHANGE THE BELOW LINE TO REFLECT DATA LOCATION ###
dataFolder = 'C:/Users/sidne/OneDrive/Documents/GeorgiaTech/DVA/Project/CODE/Data';
dirtyDataLoc = paste(dataFolder,'/rawData/',sep="");
cleanDataLoc = paste(dataFolder,'/cleanData/',sep="");

# go to the dirty data location
setwd(dirtyDataLoc)

#list of required additional packages beyond base R
require(dplyr)
require(tidyr)
require(openxlsx)


########## MAIN ##########

# Use the nfl data to pull out the people we really care about
load(paste(cleanDataLoc,"nflPlayerEPAWPA_combine.rdata",sep=""));

# find all rdata files with the letters 'CFB' in them 
# note: rdata files use capital CFB, csv files use lowercase cfb
rDataFiles <- list.files(pattern = "\\.rdata$");
cfbRFiles <- intersect(list.files(pattern = "CFB"),rDataFiles);
csvFiles <- list.files(pattern="\\.csv$");
cfbCsvFiles <- intersect(list.files(pattern="cfb"),csvFiles);


# dataset 1: player rosters
allRosterDf = data.frame();
# find all roster, cfb, csv files
rosterFiles = intersect(cfbCsvFiles, list.files(pattern="rosters"));
# iterate throguh each roster file
for( fileName in rosterFiles){
  # load the csv file
  data_file = read.csv(fileName, stringsAsFactors = FALSE);
  # append to the overall data frame
  allRosterDf = rbind(allRosterDf, data_file);
  # clear the data file to avoid any issues
  rm(data_file)
}
# only keep players that have a blank, 'WR', or '?' position 
allRosterDf = allRosterDf[allRosterDf$position=="WR"|allRosterDf$position==""|allRosterDf$position=="?",]

# create a full name field using the first and last name, remove other name fields
allRosterDf$full_name = paste(allRosterDf$first_name,allRosterDf$last_name);
# note that some of teh names are blank or erroneous. Should stick with ID being the identifying variable
allRosterDf$first_name = NULL;
allRosterDf$last_name = NULL;

summary(allRosterDf)

#We do not need college weight height. We have this information from combine data.
allRosterDf$X=NULL;
allRosterDf$height=NULL;
allRosterDf$weight=NULL;
allRosterDf$jersey=NULL;
allRosterDf$year=NULL;

# only keep players that are referenced in our nfl dataset. Let us merge w.r.t player name and school attended
# to do that we need to make sure college names dont have any discrepancies between them.

schoollist <- unique(Feature_selec$college)

cntr=0
for (i in schoollist){
  cntr=cntr+1
  result <- i %in% unique(allRosterDf$school)[grep(i,unique(allRosterDf$school))]
  if (result==FALSE){
    cat(i,'-',paste(unique(allRosterDf$school)[grep(i,unique(allRosterDf$school))],collapse=", "),"\n")
  }
}

unique(allRosterDf$school)

#After some diligence, some of the names were changed to match between the files. This is useful in later merges as well.

Feature_selec[Feature_selec$college=="Miami (FL)","college"]="Miami"
Feature_selec[Feature_selec$college=="Central Florida","college"]="UCF"
Feature_selec[Feature_selec$college=="Southern California","college"]="USC"
Feature_selec[Feature_selec$college=="Massachusetts","college"]="UMass"
Feature_selec[Feature_selec$college=="Mississippi","college"]="Ole Miss"
Feature_selec[Feature_selec$college=="Texas Christian","college"]="TCU"



# Let us merge now. There is not much information in homecity, homecountry and home state. 
# Hence removing them. These can be obtained from high school recruiting files
rosterNflPlayers = merge(x=Feature_selec,y=allRosterDf %>% select(-season,
                                                                  -position,
                                                                  -home_city,
                                                                  -home_country,
                                                                  -home_state), by.y=c("full_name","school"), by.x=c("Player","college"), all.x=TRUE);


#Lets investigate the rows that did not find a match
#Rows that found a match are below
rosterNflPlayers1 = rosterNflPlayers[complete.cases(rosterNflPlayers$id),]
#Rows that did not find a match are below
rosterNflPlayers2 = rosterNflPlayers[!complete.cases(rosterNflPlayers$id),1:23]

#Can we try finding the match of latter by merging by name only. Let us see if we can!
rosterNflPlayers2 = merge(x=rosterNflPlayers2,y=allRosterDf %>% select(-season,
                                                                       -position,
                                                                       -home_city,
                                                                       -home_country,
                                                                       -home_state), by.y=c("full_name"), by.x=c("Player"), all.x=TRUE);
rosterNflPlayers2 = rosterNflPlayers2[!duplicated(rosterNflPlayers2),]
#Any players with duplicate names?
rosterNflPlayers2[duplicated(rosterNflPlayers2$Player),c("Player","college","school","id")]
#Looks like we are good to go.
#But why couldnt they be merged with college names? Lets investigate and compare colleges

rosterNflPlayers2[,c("Player","college","school")]
#Out of 3 players who found a match this way, only 1 is authentic. Dorial Green-Beckham shifted colleges
#Lets remove stats for other 2 players.


rosterNflPlayers2[rosterNflPlayers2$Player=="Mike Evans","id"] = NA;

rosterNflPlayers2[rosterNflPlayers2$Player=="Josh Reynolds","id"] = NA;

rosterNflPlayers2$school = NULL;

rosterNflPlayers_final = rbind(rosterNflPlayers1,rosterNflPlayers2)

rosterNflPlayers_final=rosterNflPlayers_final[!duplicated(rosterNflPlayers_final),]

colnames(rosterNflPlayers_final)[ncol(rosterNflPlayers_final)] = "college_id"

rm(rosterNflPlayers)
rm(rosterNflPlayers1)
rm(rosterNflPlayers2)
rm(allRosterDf)
rm(Feature_selec)

#How many college ids missing?
rosterNflPlayers_final[is.na(rosterNflPlayers_final$college_id),]
#So far 5 players could not find match. Will let it go for now. Maybe we will find matches later.

# link with stat data to fill in

# dataset 2: find all player stat files
allStatDf = data.frame();
# find all stat, cfb, csv files
statFiles = intersect(cfbCsvFiles, list.files(pattern="stats"));
# iterate throguh each stat file
for( fileName in statFiles){
  # load the csv file
  data_file = read.csv(fileName, stringsAsFactors = FALSE);
  # append to the overall data frame
  allStatDf = rbind(allStatDf, data_file);
  # clear teh data file to avoid any issues
  rm(data_file)
}
allStatDf$X = NULL;

summary(allStatDf)
#Let us investigate statistics and related category
unique(allStatDf$category)
unique(allStatDf$statistic)

# only include the category "receiving"
#statReceiveDf = allStatDf[allStatDf$category=="receiving",];
#Should we consider other categories? Need to discuss
statReceiveDf=allStatDf[(allStatDf$category=="receiving")|(allStatDf$category=="passing")|(allStatDf$category=="rushing"),];

unique(statReceiveDf$statistic)

#Just retaining characteristics not the category
statReceiveDf$category = NULL;

# use the nfl data to again pull out only players that made it to our NFL data set
#Let us merge using both name and college id
statNflPlayers1 = merge(x=rosterNflPlayers_final[!is.na(rosterNflPlayers_final$college_id),], y = statReceiveDf, by.y=c("athlete_name","athlete_id"), by.x=c("Player","college_id"), all.x=TRUE);

#Let us check if any of the players did not find matches
statNflPlayers1[!complete.cases(statNflPlayers1),]
#There are no such players

#Now to players who have not found match yet. These are the ones with no college ids
#Let us merge by name only
statNflPlayers2 = merge(x=rosterNflPlayers_final[is.na(rosterNflPlayers_final$college_id),], y = statReceiveDf, by.y=c("athlete_name"), by.x=c("Player"), all.x=TRUE);

# Lets investigate players that dont match in terms of school name
statNflPlayers2[statNflPlayers2$college!=statNflPlayers2$school,]

# Only Josh Reynolds is mismatched here. Let us remove his stats
statNflPlayers2[statNflPlayers2$Player=="Josh Reynolds",25:30]=NA;
#Other players who have been matched. Resolve the college id column
statNflPlayers2$college_id=statNflPlayers2$athlete_id
statNflPlayers2$athlete_id=NULL;



statNflPlayers_final = rbind(statNflPlayers1,statNflPlayers2)

#Remove intermediate files
rm(allStatDf)
rm(statReceiveDf)
rm(statNflPlayers1)
rm(statNflPlayers2)

#Investigate players whose college and school dont match
unique(statNflPlayers_final[statNflPlayers_final$college!=statNflPlayers_final$school,][,c("Player","college","school")])
#These players shifted college midway. So resolved the issues of college names not matching
statNflPlayers_final$school=NULL;
summary(statNflPlayers_final)

statNflPlayers_final = statNflPlayers_final[!duplicated(statNflPlayers_final),]
statNflPlayers_final[!complete.cases(statNflPlayers_final),]

#4 players still do not have stats. Let us remove them
statNflPlayers_final = statNflPlayers_final[complete.cases(statNflPlayers_final),]


summary(statNflPlayers_final)

#Let us remove conferenses for now:
statNflPlayers_final$conference=NULL;

# Now let us extract stats for each player as mean over the games they played.
statlist <- unique(statNflPlayers_final$statistic); statlist

statNflPlayers_refine <- statNflPlayers_final
#Converting C/ATT ratio to decimal and renaming statistics to PCT_COMP (meaning percentage of passes completed)
statNflPlayers_refine[statNflPlayers_refine$statistic=="C/ATT","statistic"]="PCT_COMP"
temp11 = statNflPlayers_refine[statNflPlayers_refine$statistic=="PCT_COMP","stat"]
temp12 = as.data.frame(matrix(unlist(strsplit(temp11,"/")),ncol=2,byrow=TRUE))
statNflPlayers_refine[statNflPlayers_refine$statistic=="PCT_COMP","stat"]=as.numeric(as.character(temp12$V1))/as.numeric(as.character(temp12$V2))

statlist[statlist=="C/ATT"]="PCT_COMP"
statNflPlayers_refine$stat <- as.numeric(statNflPlayers_refine$stat)
for (statname in statlist){
  temp=statNflPlayers_refine %>% filter(statistic==statname) %>% group_by(Player) %>% summarise(mean(stat))
  statNflPlayers_refine <- merge(x=statNflPlayers_refine,y=temp,by="Player",all.x = TRUE);
  colnames(statNflPlayers_refine)[ncol(statNflPlayers_refine)]=statname
}

statNflPlayers_refine$stat=NULL;
statNflPlayers_refine$statistic=NULL;
statNflPlayers_refine$game_id=NULL;

statNflPlayers_refine = statNflPlayers_refine[!duplicated(statNflPlayers_refine),]

#Too many missing quantities for statistic INT,QBR and PCT_COMP
#INT, QBR and PCT_COMP ARE SET TO 0 where missing
for (col in c("INT","QBR","PCT_COMP")){
  varname <- paste(col, "_impute_flag", sep = "")
  assign('temp', is.na(statNflPlayers_refine[,col]) * 1) #this turns boolean to numeric flag and assigns to variable temp
  statNflPlayers_refine[,varname] <- temp
  #impute to 0
  statNflPlayers_refine[is.na(statNflPlayers_refine[,col]), col] <- 0;
}

#CAR statistic missing rows replaced by average. There are 26 such rows. Dont know what characteristic this is
statNflPlayers_refine$CAR_impute_flag = ifelse(is.na(statNflPlayers_refine$CAR),1,0)
statNflPlayers_refine[is.na(statNflPlayers_refine$CAR),"CAR"] = mean(statNflPlayers_refine[!is.na(statNflPlayers_refine$CAR),"CAR"])

rm(temp12)
rm(temp)
rm(statNflPlayers_final)

# let's focus on cleaning up and adding recruitment data for the players and go back to the stat data later

# dataset 3: college recruiting data
allRecruitDf = data.frame();
# find all recruiting, cfb, rdata files
recruitFiles = intersect(cfbRFiles, list.files(pattern="Recruiting"));
# iterate throguh each roster file
for( fileName in recruitFiles){
  # load the r file
  load(fileName);
  summary(final)
  # append to the overall data frame
  allRecruitDf = rbind(allRecruitDf, final);
  # clear the data file to avoid any issues
  rm(final);
}
# check recruittype
unique(allRecruitDf$recruitType);
# They're all high schoolers, so remove this column
allRecruitDf$recruitType = NULL;
#We are not interested in height and weight of high schoolers.
allRecruitDf$height=NULL;
allRecruitDf$weight=NULL;
#Assuming NA country is US
allRecruitDf[is.na(allRecruitDf$country),"country"]="US"

# rename some columns so when we merge datasets, it's not confusing
names(allRecruitDf)[names(allRecruitDf) == 'school'] <- 'highSchool'
names(allRecruitDf)[names(allRecruitDf) == 'year'] <- 'gradHighSchoolYear'
names(allRecruitDf)[names(allRecruitDf) == 'ranking'] <- 'highSchoolRank'
names(allRecruitDf)[names(allRecruitDf) == 'city'] <- 'highSchoolCity'
names(allRecruitDf)[names(allRecruitDf) == 'stateProvince'] <- 'highSchoolState'
names(allRecruitDf)[names(allRecruitDf) == 'stars'] <- 'highSchoolStars'
names(allRecruitDf)[names(allRecruitDf) == 'rating'] <- 'highSchoolRating'
names(allRecruitDf)[names(allRecruitDf) == 'country'] <- 'highSchoolCountry'
names(allRecruitDf)[names(allRecruitDf) == 'position'] <- 'highSchoolPosition'

#Lets come back to NFL players of interest and try to extract high school information related to them
#Let us merge by both player name and college name
recruitRoster = merge(x=statNflPlayers_refine, y=allRecruitDf, by.x=c("Player","college"), by.y=c("name","committedTo"), all.x=TRUE);


nrow(recruitRoster[!complete.cases(recruitRoster),])
#41 players could not be matched. Lets try matching by name only for these players

# Matched players 
recruitRoster1=recruitRoster[complete.cases(recruitRoster),]
#Unmatched players
recruitRoster2=recruitRoster[!complete.cases(recruitRoster),1:ncol(statNflPlayers_refine)]
#Merge unmatched players by player name only
recruitRoster2=merge(x=recruitRoster2, y=allRecruitDf, by.x=c("Player"), by.y=c("name"), all.x=TRUE)

#Lets investigate if there is any mistake while matching players by name only
recruitRoster2[complete.cases(recruitRoster2),c("Player","college","committedTo","draft_year","gradHighSchoolYear","highSchool")]

#Resolving Incorrect merges after due diligence
recruitRoster2=recruitRoster2[!(recruitRoster2$Player=="Chris Harper"&recruitRoster2$committedTo=="California"),]
recruitRoster2[recruitRoster2$Player=="Corey Davis",(ncol(statNflPlayers_refine)+1):ncol(recruitRoster2)]=NA
recruitRoster2[recruitRoster2$Player=="Kevin White",(ncol(statNflPlayers_refine)+1):ncol(recruitRoster2)]=NA
recruitRoster2[recruitRoster2$Player=="Trent Taylor",(ncol(statNflPlayers_refine)+1):ncol(recruitRoster2)]=NA
recruitRoster2=recruitRoster2[!(duplicated(recruitRoster2)),]

nrow(recruitRoster2[!complete.cases(recruitRoster2),])
#34 players could not be matched still.
#Who are those players
recruitRoster2[!complete.cases(recruitRoster2),c("Player","college","draft_year","committedTo","highSchool")]
#Tried finding some of these players by different combinations of name in high school data. Did not have much luck
recruitRoster2$committedTo=NULL;

recruitRoster_final=rbind(recruitRoster1,recruitRoster2)
recruitRoster_final$hometown <- paste(recruitRoster_final$highSchoolCity,recruitRoster_final$highSchoolState,recruitRoster_final$highSchoolCountry)

recruitRoster_final$highSchoolCity <- NULL;
recruitRoster_final$highSchoolState <- NULL;
recruitRoster_final$highSchoolCountry <- NULL;
recruitRoster_final$highSchoolPosition <- NULL; #Note not all players in high school are WR
recruitRoster_final$highSchoolRank <- NULL;
recruitRoster_final$gradHighSchoolYear<-NULL;
recruitRoster_final$highSchool <- NULL;

summary(recruitRoster_final)

#Adding the missing data of highschool stars, rating and hometown from a different source
missschoolinfo <- read.xlsx("Players_missing_highschool_hometown.xlsx")
summary(missschoolinfo)

recruitRoster_final <- merge(x=recruitRoster_final,y=missschoolinfo,by=c("Player","college","college_id"),all.x = T)

recruitRoster_final[recruitRoster_final$hometown=="NA NA NA","hometown"] <- recruitRoster_final[recruitRoster_final$hometown=="NA NA NA","Home"]
recruitRoster_final[is.na(recruitRoster_final$highSchoolStars),"highSchoolStars"] <- recruitRoster_final[is.na(recruitRoster_final$highSchoolStars),"Stars"]
recruitRoster_final[is.na(recruitRoster_final$highSchoolRating),"highSchoolRating"] <- recruitRoster_final[is.na(recruitRoster_final$highSchoolRating),"Rating"]
recruitRoster_final$Stars=NULL;
recruitRoster_final$Rating=NULL;
recruitRoster_final$Home=NULL;


#What do we do about missing data? Fill in unknown rating and stars with mean for now.
for (col in c("highSchoolStars","highSchoolRating")){
  varname <- paste(col, "_impute_flag", sep = "")
  assign('temp', is.na(recruitRoster_final[,col]) * 1) #this turns boolean to numeric flag and assigns to variable temp
  recruitRoster_final[,varname] <- temp
  #impute to average
  recruitRoster_final[is.na(recruitRoster_final[,col]), col] <- mean(recruitRoster_final[,col], na.rm = TRUE)
}

recruitRoster_final=recruitRoster_final[!duplicated(recruitRoster_final),]
rm(allRecruitDf)
rm(recruitRoster)
rm(recruitRoster1)
rm(recruitRoster2)


# save the player data
save(recruitRoster_final,file=paste(cleanDataLoc,"nfl_cfb_combine.rdata",sep=""))



