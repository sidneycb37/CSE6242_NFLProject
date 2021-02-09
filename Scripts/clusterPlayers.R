# This script reads in previously cleaned data and uses available features to cluster players
# created: 3/26
# last updated: 4/13/2020 by Sidney
# Changes: Cleaned up code for final submission. Changed some comments, revised folder locations
# To run on new machines, the data folder names (lines 13-15) should be edited
# used https://www.statmethods.net/advstats/cluster.html for reference

# remove past variables
rm(list=ls())

# need to specify folder holding all data to be processed and a location to save model results
### IF RUNNING ON A NEW MACHINE, CHANGE THE BELOW LINE TO REFLECT DATA LOCATION ###
dataFolder = 'C:/Users/sidne/OneDrive/Documents/GeorgiaTech/DVA/Project/CODE/Data';
inputLoc = paste(dataFolder,'/modelResults/',sep="");
outputLoc = paste(dataFolder,'/modelResults/',sep="");

#load required packages
#install.packages("fpc")
library(dplyr)
require(openxlsx)
library(fpc)
library(cluster)

# load data
load(paste(inputLoc, "data_scored_rf4.rdata", sep = ""))

# select numeric fields for clustering
#numerics <- nfl_conf_noimp %>% dplyr::select_if(is.numeric) %>% select(-college_id, -draft_year)
numerics <- data %>% select(YDS, pick, highSchoolRating, REC, height_inches, TD, prob_good_rf)

# normalize data
mydata = scale(numerics);

# Determine number of clusters w/elbow graph
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#looks like the "elbow is really at 2 - can pick any number between 2 and 10
#lets stick with 5 as a starting point

# K-Means Cluster Analysis
set.seed(1)
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
#mydata <- data.frame(mydata, fit$cluster)

# K-Means Clustering with 5 clusters
set.seed(1)
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(mydata, fit$cluster)

#to much overlap between clusters
#lets try 6 clusters instead - make the groups a little more distinct from each other in terms of profiles
# K-Means Cluster Analysis
set.seed(1)
fit <- kmeans(mydata, 6) # 6 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)
set.seed(1)
# K-Means Clustering with 6 clusters
fit <- kmeans(mydata, 6)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(mydata, fit$cluster)

# merge datasets to see similar player's names
similarPlayers = data.frame(mydata[ , c("fit.cluster")], data[ , c("Player", "GSIS_ID")])
# rename the column that is super long
names(similarPlayers)[names(similarPlayers) == 'mydata...c..fit.cluster...'] = 'clusterNumber';

# separate similar players into separate character
clusterDf <- data.frame(cluster1 = character(),
                        cluster2 = character(),
                        cluster3 = character(),
                        cluster4 = character(),
                        cluster5 = character(),
                        cluster6 = character(),stringsAsFactors = FALSE)

for (numCluster in 1:max(similarPlayers$clusterNumber)){
  tempPlayers = filter(similarPlayers, clusterNumber==numCluster);
  clusterDf[1:nrow(tempPlayers),numCluster] = tempPlayers$Player;
}

#add cluster to master data and save in excel as a sheet
data2 <- data %>% mutate(player_cluster = ifelse(Player %in% clusterDf$cluster1, 1,
                                                 ifelse(Player %in% clusterDf$cluster2, 2,
                                                        ifelse(Player %in% clusterDf$cluster3, 3,
                                                               ifelse(Player %in% clusterDf$cluster4, 4,
                                                                      ifelse(Player %in% clusterDf$cluster5, 5, 6))))))

# write to excel
# #output data to excel file
OUT = createWorkbook()
addWorksheet(OUT, "clusters")
writeData(OUT, sheet = "clusters", x = clusterDf)
addWorksheet(OUT, sheet = "Data")
writeData(OUT, sheet = "Data", x = data2)
saveWorkbook(OUT, paste(outputLoc, "clusterResults.xlsx", sep = ""), overwrite = T)