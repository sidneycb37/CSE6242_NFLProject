# This script will focus on checking for correlations and variance in the finalized dataset
# To run on new machines, the data folder names (lines 11-13) should be edited
# last updated: 4/13/2020 by Sidney
# Changes: Cleaned up code for final submission. Changed some comments, revised folder locations

# remove past variables
rm(list=ls())

# need to specify folders holding clean data to process and where to produce model results
### IF RUNNING ON A NEW MACHINE, CHANGE THE BELOW LINE TO REFLECT DATA LOCATION ###
dataFolder = 'C:/Users/sidne/OneDrive/Documents/GeorgiaTech/DVA/Project/CODE/Data';
inputLoc = paste(dataFolder,'/cleanData/',sep="");
outputLoc = paste(dataFolder,'/modelResults/',sep="");



#load required packages
require(tidyverse)
require(corrplot)
require(openxlsx)
require(stats)

#load data
#load without imputation flags
load(paste(inputLoc, "NFL_Featureselection_wo_imputeflags.rdata", sep = ""))

#select numeric fields for correlation testing
numerics <- nfl_conf_noimp %>% dplyr::select_if(is.numeric) %>% select(-college_id, -draft_year)

#build correlation matrix
#stick with pearson correlation for now
correlations <- cor(numerics, method = "pearson")

#visualize as a heat map
corrplot(correlations, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#save correlation table to csv and heatmap to png
write.csv(correlations, file = paste(outputLoc, "correlation_matrix.csv", sep = ""))
png(paste(outputLoc, "correlation_heatmap.png", sep = ""))
corrplot(correlations, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()

#PCA test the numeric fields also
pca <- prcomp(numerics %>% select(-wpa_3season, -epa_3season))
#summary to see how much variance each ocmponent explains
summary(pca)
#first 2 components have the majority of the variance - check most important fields based on absolute value of the loadings
loadings <- as.data.frame(pca$rotation)
loadings$fieldname <- rownames(loadings)
loadings <- loadings[, sort(names(loadings))]
loadings

#save pca object to rdata object
save(pca, file = paste(outputLoc, "pca.rdata", sep = ""))

#most predictive element of future nfl production is actually where a player got picked
#suggests nfl teams pick more productive players AND better picks are given more opportunities to produce
#component 2 is basically collegel players that show different phyisical size and produced in college
#component 3 is similar to component 2
#general idea: conditional on where a player was picked, players that produced better in college produce better in the nfl
#this matches the correlation testing


#for character fields - check if any of the fields vary greatly (on average) by character fields
#test both targets for now
chars <- nfl_conf_noimp[, !(names(nfl_conf_noimp) %in% names(numerics)) | (names(nfl_conf_noimp) %in% c("epa_3season", "wpa_3season"))]

colleges <- chars %>% group_by(college) %>% summarise(units = n(), avg_wpa_3season = mean(wpa_3season), avg_epa_3season = mean(epa_3season))
draft_years <- chars %>% group_by(draft_year) %>% summarise(units = n(), avg_wpa_3season = mean(wpa_3season), avg_epa_3season = mean(epa_3season))
hometowns <- chars %>% group_by(hometown) %>% summarise(units = n(), avg_wpa_3season = mean(wpa_3season), avg_epa_3season = mean(epa_3season))
conferences <- chars %>% group_by(conf) %>% summarise(units = n(), avg_wpa_3season = mean(wpa_3season), avg_epa_3season = mean(epa_3season))

#output data to excel file
OUT = createWorkbook()
addWorksheet(OUT, "PCA - Loadings")
addWorksheet(OUT, "correlation")
addWorksheet(OUT, "colleges")
addWorksheet(OUT, "draft year")
addWorksheet(OUT, "hometowns")
addWorksheet(OUT, "conferences")
writeData(OUT, sheet = "PCA - Loadings", x = loadings)
writeData(OUT, sheet = "correlation", x = correlations)
writeData(OUT, sheet = "colleges", x = colleges)
writeData(OUT, sheet = "draft year", x = draft_years)
writeData(OUT, sheet = "hometowns", x = hometowns)
writeData(OUT, sheet = "conferences", x = conferences)
saveWorkbook(OUT, paste(outputLoc, "correlation_pca_results.xlsx", sep = ""), overwrite = T)