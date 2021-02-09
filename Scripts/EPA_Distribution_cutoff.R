# This script looks at the distribution of our target metric and identifies an appropriate cutoff value to determine a good or bad player
# The determined cutoff value is used in later scripts, but this script is not necessary to run for the rest of analysis
# Last edited 4/14 by Sidney for final submission

# If running on a new machine, change lines 14 and 15



# remove past variables
rm(list=ls())

# set appropriate folders
# CHANGE THE BELOW LINES IF RUNNING ON A NEW MACHINE
setdir = 'C:/Users/sidne/OneDrive/Documents/GeorgiaTech/DVA/Project/CODE';
inputLoc = './Data/cleanData/';


setwd(setdir)

load(paste(inputLoc, "NFL_Featureselection_wo_imputeflags.rdata", sep = ""))

data_model_epa_wpa <- nfl_conf_noimp %>% mutate(height=height_inches,
                                                collg_rank=rank,conf=as.factor(conf)) %>% select(epa_3season,
                                                                                                 wpa_3season,pick,conf,collg_rank,height,weight,forty,vertical,bench,broad,threecone,
                                                                                                 shuttle,AVG,REC,YDS,CAR,TD,LONG,INT,QBR,PCT_COMP,highSchoolStars,highSchoolRating)

attach(data_model_epa_wpa)
summary(data_model_epa_wpa$wpa_3season)
summary(data_model_epa_wpa$epa_3season)

quantile(data_model_epa_wpa$wpa_3season)
quantile(data_model_epa_wpa$epa_3season)


library(ggplot2)

#epa_3season distribution

#QQplot
par(mfrow=c(1,1))
qqnorm(wpa_3season)
qqline(wpa_3season,col="blue",lwd=2)

#Histogram of wpa (black line - median, red line - 79 percentile)

p1 <- ggplot(data=data_model_epa_wpa,aes(x=wpa_3season))+geom_histogram(color='black',fill='white',binwidth = 0.008)
p1 <- p1 + ggtitle("wpa_3season")+geom_vline(aes(xintercept=quantile(data_model_epa_wpa$wpa_3season,0.788)),color="red", linetype="dashed", size=0.5)
p1 <- p1 + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$wpa_3season,0.5)),color="black", linetype="dashed", size=0.5)
p1

#Histogram of wpa with lower binwidth (black line - median, red line - 79 percentile)

p11 <- ggplot(data=data_model_epa_wpa,aes(x=wpa_3season))+geom_histogram(color='black',fill='white',binwidth = 0.001)
p11 <- p11 + ggtitle("wpa_3season")+geom_vline(aes(xintercept=quantile(data_model_epa_wpa$wpa_3season,0.788)),color="red", linetype="dashed", size=0.5)
p11 <- p11 + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$wpa_3season,0.5)),color="black", linetype="dashed", size=0.5)
p11


#wpa_3season distribution

par(mfrow=c(1,1))
qqnorm(epa_3season)
qqline(epa_3season,col="blue",lwd=2)

#Histogram of epa (black line - median, red line - 78 percentile, blue line - 71 percentile)

p2 <- ggplot(data=data_model_epa_wpa,aes(x=epa_3season))+geom_histogram(color='black',fill='white',binwidth = 0.25)
p2 <- p2 + ggtitle("wpa_3season") + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$epa_3season,0.78)),color="red", linetype="dashed", size=0.5)
p2 <- p2 + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$epa_3season,0.71)),color="blue", linetype="dashed", size=0.5)
p2 <- p2 + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$epa_3season,0.5)),color="black", linetype="dashed", size=0.5)
p2


#****MAIN PLOT******#

#Histogram of epa with lower binwidth
#(black line - median, red line - 78 percentile, blue line - 71 percentile, purple line - 53 percentile)

p22 <- ggplot(data=data_model_epa_wpa,aes(x=epa_3season))+geom_histogram(color='black',fill='white',binwidth = 0.1)
p22 <- p22 + ggtitle("wpa_3season") + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$epa_3season,0.78)),color="red", linetype="dashed", size=0.5)
p22 <- p22 + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$epa_3season,0.71)),color="blue", linetype="dashed", size=1)
p22 <- p22 + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$epa_3season,0.5)),color="black", linetype="dashed", size=0.5)
p22 <- p22 + geom_vline(aes(xintercept=quantile(data_model_epa_wpa$epa_3season,0.53)),color="purple", linetype="dashed", size=0.5)
p22



#Now lets take a number for example. 
# Say we take 71 percentile - rounding off to 70 percentile

epa_cutoff <- quantile(data_model_epa_wpa$epa_3season,0.7)
names(epa_cutoff) <- NULL

#The cutoff score for epa is 3.97 at 70 percentile cutoff

# Let us see how many players have value above the 70% quantile

temp <- data_model_epa_wpa[data_model_epa_wpa$epa_3season>=epa_cutoff,]

#There are 39 players who will be categorized as good based on their epa score

#Adding a extra column "good" to categorize the players
#Players whose epa score is equal and above the cutoff value will be given 1 and who are below the cutoff will be 0
data_model_epa_wpa$good <- ifelse(data_model_epa_wpa$epa_3season>=epa_cutoff,1,0)

#Does the classification show significant difference in the predictor variables?
par(mfrow=c(2,2))
boxplot(weight~good,data=data_model_epa_wpa)
boxplot(height~good,data=data_model_epa_wpa)
boxplot(highSchoolRating~good,data=data_model_epa_wpa)
boxplot(highSchoolStars~good,data=data_model_epa_wpa)

#We do see some difference in weight, height, high school rating and stars for good players

par(mfrow=c(2,2))
boxplot(REC~good,data=data_model_epa_wpa)
boxplot(YDS~good,data=data_model_epa_wpa)
boxplot(TD~good,data=data_model_epa_wpa)
boxplot(LONG~good,data=data_model_epa_wpa)

#We do see some difference in REC, YDS, TD, LONG for good players

par(mfrow=c(1,2))
boxplot(collg_rank~good,data=data_model_epa_wpa)
boxplot(pick~good,data=data_model_epa_wpa)

# Not much difference in college rank, but expected difference observed in pick
#Lower picked players are good ones

par(mfrow=c(2,2))
boxplot(bench~good,data=data_model_epa_wpa)
boxplot(broad~good,data=data_model_epa_wpa)
boxplot(threecone~good,data=data_model_epa_wpa)
boxplot(shuttle~good,data=data_model_epa_wpa)

#Some difference observed in threecone and shuttle combine measures

par(mfrow=c(1,2))
boxplot(vertical~good,data=data_model_epa_wpa)
boxplot(forty~good,data=data_model_epa_wpa)

par(mfrow=c(2,2))
boxplot(CAR~good,data=data_model_epa_wpa)
boxplot(QBR~good,data=data_model_epa_wpa)
boxplot(INT~good,data=data_model_epa_wpa)
boxplot(PCT_COMP~good,data=data_model_epa_wpa)

#Not much difference observed in other measures

#From above plots, looks like 70 percentile cutoff is a good choice.
#The epa score cutoff for 70 percentile is 3.97

#Now lets take other cutoffs and compare against 70% cutoff 
# Say we take - 53 and 78 percentile cutoff

#53 percentile cutoff
epa_cutoff2 <- quantile(data_model_epa_wpa$epa_3season,0.53)
names(epa_cutoff2) <- NULL

# Let us see how many players have value above the 53% quantile

temp2 <- data_model_epa_wpa[data_model_epa_wpa$epa_3season>=epa_cutoff2,]

#There are 61 good players according to this criterion
#adding a column good_0p53 for this classification
data_model_epa_wpa$good_0p53 <- ifelse(data_model_epa_wpa$epa_3season>=epa_cutoff2,1,0)

#Higher percentile can be taken - 0.78 but then there will be less number of players in the dataset that 
#are considered good

#78 percentile cutoff
epa_cutoff3 <- quantile(data_model_epa_wpa$epa_3season,0.78)
names(epa_cutoff3) <- NULL

# Let us see how many players have value above the 78% quantile

temp3 <- data_model_epa_wpa[data_model_epa_wpa$epa_3season>=epa_cutoff3,]

#There are 29 good players according to this criterion
#adding a column good_0p78 for this classification
data_model_epa_wpa$good_0p78 <- ifelse(data_model_epa_wpa$epa_3season>=epa_cutoff3,1,0)

#Does the classification show significant difference in the predictor variables?
par(mfrow=c(1,3))
boxplot(weight~good_0p53,data=data_model_epa_wpa)
boxplot(weight~good,data=data_model_epa_wpa)
boxplot(weight~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(height~good_0p53,data=data_model_epa_wpa)
boxplot(height~good,data=data_model_epa_wpa)
boxplot(height~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(highSchoolRating~good_0p53,data=data_model_epa_wpa)
boxplot(highSchoolRating~good,data=data_model_epa_wpa)
boxplot(highSchoolRating~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(highSchoolStars~good_0p53,data=data_model_epa_wpa)
boxplot(highSchoolStars~good,data=data_model_epa_wpa)
boxplot(highSchoolStars~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(REC~good_0p53,data=data_model_epa_wpa)
boxplot(REC~good,data=data_model_epa_wpa)
boxplot(REC~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(YDS~good_0p53,data=data_model_epa_wpa)
boxplot(YDS~good,data=data_model_epa_wpa)
boxplot(YDS~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(TD~good_0p53,data=data_model_epa_wpa)
boxplot(TD~good,data=data_model_epa_wpa)
boxplot(TD~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(LONG~good_0p53,data=data_model_epa_wpa)
boxplot(LONG~good,data=data_model_epa_wpa)
boxplot(LONG~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(collg_rank~good_0p53,data=data_model_epa_wpa)
boxplot(collg_rank~good,data=data_model_epa_wpa)
boxplot(collg_rank~good_0p78,data=data_model_epa_wpa)


par(mfrow=c(1,3))
boxplot(pick~good_0p53,data=data_model_epa_wpa)
boxplot(pick~good,data=data_model_epa_wpa)
boxplot(pick~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(threecone~good_0p53,data=data_model_epa_wpa)
boxplot(threecone~good,data=data_model_epa_wpa)
boxplot(threecone~good_0p78,data=data_model_epa_wpa)

par(mfrow=c(1,3))
boxplot(shuttle~good_0p53,data=data_model_epa_wpa)
boxplot(shuttle~good,data=data_model_epa_wpa)
boxplot(shuttle~good_0p78,data=data_model_epa_wpa)


#Summary - overall difference is higher with 78 percentile cutoff 
#70 percentile cutoff is still a good choice since the difference between 70 and 78 cutoff is not
#significant

