# Script to use good/bad labeled players to develop classification framework
# Tests a couple different models, random forest most accurate
# This script uses a cutoff value of 70th quantile as determined in the EPA_Distribution_cutoff.R script
# If running on a new machine, change lines 27-29

# Created by Sidney on 4/7
# last updated: 4/13/2020 by Sidney
# Changes: Cleaned up code for final submission. Changed some comments, revised folder locations


library(glmnet)
library(tidyverse)
library(randomForest)
library(e1071)
library(corrplot)
library(stats)
library(car)
library(ROCR)
library(scales)
library(reshape2)

# remove past variables
rm(list=ls())

# need to specify folder holding data
### IF RUNNING ON A NEW MACHINE, CHANGE THE BELOW LINE TO REFLECT DATA LOCATION ###
dataFolder = 'C:/Users/sidne/OneDrive/Documents/GeorgiaTech/DVA/Project/CODE/Data';
inputLoc = paste(dataFolder,'/cleanData/',sep="");
outputLoc = paste(dataFolder,'/modelResults/',sep="");

# re-load the data
load(paste(inputLoc, "NFL_Featureselection_wo_imputeflags.rdata", sep = ""))

#add transformations from previous attempts at models on continuous versions of fields
#don't forget to reclassify target!!!
epa_cutoff <- quantile(nfl_conf_noimp$epa_3season,0.7)
data <- nfl_conf_noimp %>% mutate(power5_flag = ifelse(conf %in% c("SEC", "Pac-12", "Big 12", "Big 10", "ACC"), 1, 0),
                                  ranked_end_season_flag = ifelse(rank <= 25, 1, 0),
                                  bmi = 703 * (weight/(height_inches * height_inches)),
                                  speed_score = (weight * 200)/(forty*forty*forty*forty),
                                  height_adjusted_speed_score = (weight * 200)/(forty*forty*forty*forty)/73, #avg WR height in NFL 
                                  explosion_score = bench + vertical + broad/12,
                                  agility_score = threecone + shuttle,
                                  good = ifelse(epa_3season >= epa_cutoff,1,0))%>% select_if(is.numeric)

#rebuild correlation plots to show it here as well before we begin looking at our "best" fields
correlations <- cor(data, method = "pearson")
corrplot(correlations, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
correlations <- cor(data, method = "spearman")
corrplot(correlations, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#turn good to factor with reference level 0 - predict values of 1 (good)
data$good <- relevel(as.factor(data$good), ref = '0')

#ok to make this easier - looks like the big factors are college production, where you got picked, and maybe a few other weak effects
#lets subset for those first and see how we do
data_skinny <- data %>% select(good, REC, TD, YDS, LONG, ranked_end_season_flag, power5_flag, highSchoolRating, 
                                shuttle, threecone, agility_score, forty, bmi, height_inches, pick)

#ok lets throw these into a logistic regression and see who is significant
#both statistical AND practically significant
#NOTE: AGILITY SCORE IS COMBO OF SHUTTLE AND 3 CONE - GETS SET TO NA NATURALLY - REMOVE
logit1 <- glm(formula = as.formula(good ~ .), data = data_skinny, family = binomial(link = "logit"))
summary(logit1)
#this is odd. Does not match intuition and correlation testing showing production fields - perhaps we have some high VIFs here
#lets try a really simple "production only" version of this
logit2 <- glm(formula = as.formula(good ~ REC + TD + YDS), data = data_skinny, family = binomial(link = "logit"))
summary(logit2)
#this is a bit unintuitive - perhaps logistic regression (linear discriminant) is not a good way to solve this problem
#lets try this again with just receptions and agility score
logit3 <- glm(formula = as.formula(good ~ REC + agility_score), data = data_skinny, family = binomial(link = "logit"))
summary(logit3)
#alright this looks a little stronger and more intuitive - agile players who catch lots of passes are good NFL players
#add the yardage component back in
logit4 <- glm(formula = as.formula(good ~ REC + agility_score + YDS), data = data_skinny, family = binomial(link = "logit"))
summary(logit4)
#this doesn't make sense...yards should also matter...issue is correlation of the 2 fields
#what if we normalize this to yards per reception - it becomes an efficiency stat
data_skinny <- data_skinny %>% mutate(ypc = YDS/REC)
#try another logistic regression with this and agility score
logit5 <- glm(formula = as.formula(good ~ agility_score + ypc), data = data_skinny, family = binomial(link = "logit"))
summary(logit5)
#the data seems to be pointing us towards raw volume mattering...that seems odd but makes sense in some ways
#more receptions in college would correlate higher with more usage and EPA and WPA in college
#so it makes sense that players whose college teams emphasize them in terms of touches are more likely to be good NFL players
#it is a "coaches know their guys" type of result
#the coefficient for agility score being positive is weird - implies less agile players (slower change of direction) are better
#see if we can flip that to "correct" intuition - otherwise we can't use it
logit6 <- glm(formula = as.formula(good ~ REC + agility_score + highSchoolRating + pick + 
                                     forty + bmi + ranked_end_season_flag + power5_flag), data = data_skinny, family = binomial(link = "logit"))
summary(logit6)
#take out agility score - not intuitive in terms of the result
#also remove non-10% significant fields
logit7 <- glm(formula = as.formula(good ~ REC + pick + 
                                     forty + bmi), data = data_skinny, family = binomial(link = "logit"))
summary(logit7)
#this does not look how we expect
#lets try a lasso logistic regression and see what variables make it through
#10 fold validation - since there isn't true training vs validation this should help prevent over fitting
set.seed(1)
cv.out <- cv.glmnet(as.matrix(data_skinny %>% select(-good)), data_skinny$good, alpha = 1,
                    family = "binomial", type.measure = "auc", nfolds = 10)
plot(cv.out)
#ok lets see what kind of models we get at the min lambda and 1se lambda
coef(cv.out, s = "lambda.min")
coef(cv.out, s = "lambda.1se")
#ok the 1SE model has an AUC of around .8 based on the graph above, so it actually seems like a good bet
#lets run that group of fields with a coefficient into traditional logistic regression to see what we get
logit8 <- glm(formula = as.formula(good ~ REC + ranked_end_season_flag + power5_flag + highSchoolRating + 
                                         threecone + forty + bmi + height_inches + pick),
             data = data_skinny, family = binomial(link = "logit"))
summary(logit8)
#remove the speed features - it doesn't make sense for worse athletes to be more likely to be good
set.seed(1)
cv.out <- cv.glmnet(as.matrix(data_skinny %>% select(-good, -forty, -threecone, -agility_score, -shuttle)), data_skinny$good, alpha = 1,
                    family = "binomial", type.measure = "auc", nfolds = 10)
plot(cv.out)
#ok lets see what kind of models we get at the min lambda and 1se lambda
coef(cv.out, s = "lambda.min")
coef(cv.out, s = "lambda.1se")
#try it with a regular logistic regression to see if that regression has everything being significant
logit9 <- glm(formula = as.formula(good ~ REC + ranked_end_season_flag + highSchoolRating + 
                                          height_inches + pick),
              data = data_skinny, family = binomial(link = "logit"))
summary(logit9)
#coefficient magnitudes grow with logistic regression and significance is lost
#stick with the lasso regression and dump traditional logistic regression
#for completeness - try ridge regression also and see if we get a similar answer
#if we do - stick with the lasso since we want to make sure our features are correct
#10 fold validation - since there isn't true training vs validation this should help prevent over fitting
set.seed(1)
cv.out <- cv.glmnet(as.matrix(data_skinny %>% select(-good, -forty, -threecone, -agility_score, -shuttle)), data_skinny$good, alpha = 0,
                    family = "binomial", type.measure = "auc", nfolds = 10)
plot(cv.out)
coef(cv.out, s = "lambda.min")
coef(cv.out, s = "lambda.1se")
#this isn't agggressively enough eliminating features
#lets try it with a "balanced" elastic net (alpha = .5)
set.seed(1)
cv.out <- cv.glmnet(as.matrix(data_skinny %>% select(-good, -forty, -threecone, -agility_score, -shuttle)), data_skinny$good, alpha = 0.5,
                    family = "binomial", type.measure = "auc", nfolds = 10)
plot(cv.out)
coef(cv.out, s = "lambda.min")
coef(cv.out, s = "lambda.1se")




#so the balanced EN actually eliminates height in inches, which is somewhat intuitive: plenty of shorter WRs suceed in the NFL from the slot
#lets keep this as our champion regression framework for now
#performance is good and results are intuitive





#move on to random forest
#do not do true tree approaches - small sample will likely result in over fitting
#also - limit the trees in the forest because of that same thing
##### random forest #####
set.seed(1)
rf1 <- randomForest(good ~ ., data=data_skinny,
                          ntree = 100,
                          mtry = 3,
                          importance = TRUE,
                          nodesize = 10,
                          sampsize = 100,
                          replace = TRUE,
                          keepforest = T)
#show importance
varImpPlot(rf1)
#show confusion matrix (threshold = 0.5 at baseline)
rf1$confusion
#check C-statistic
# See how well random forest predicts across our categories using AUC and ROC curve
plot.AUC <- function(trn_data_pred, trn_data_actual, val_data_pred, val_data_actual, title){
  # Get prediction and performance objects for training and validation
  trn_pred <- prediction(trn_data_pred, trn_data_actual)
  val_pred <- prediction(val_data_pred, val_data_actual)
  trn_perf <- performance(trn_pred, 'tpr', 'fpr')
  val_perf <- performance(val_pred, 'tpr', 'fpr')
  # Get AUC for training and validation
  trn.auc <- performance(trn_pred, 'auc')
  val.auc <- performance(val_pred, 'auc')
  # Store true positive and false positive rates in vectors
  trn.tpr <- trn_pred@tp[[1]]/max(trn_pred@tp[[1]])
  val.tpr <- val_pred@tp[[1]]/max(val_pred@tp[[1]])
  trn.fpr <- trn_pred@fp[[1]]/max(trn_pred@fp[[1]])
  val.fpr <- val_pred@fp[[1]]/max(val_pred@fp[[1]])
  # Store actual AUC values
  trn.auc2 <- trn.auc@y.values[[1]]
  val.auc2 <- val.auc@y.values[[1]]
  # Store actual KS values
  trn.ks <- max(attr(trn_perf, "y.values")[[1]] - (attr(trn_perf, "x.values")[[1]]))
  val.ks <- max(attr(val_perf, "y.values")[[1]] - (attr(val_perf, "x.values")[[1]]))
  
  trn.data <- data.frame(cbind(trn.tpr, trn.fpr))
  val.data <- data.frame(cbind(val.tpr, val.fpr))
  trn.data$dataset <- "Training"
  val.data$dataset <- "Validation"
  
  colnames(trn.data) <- c("tpr", "fpr", "dataset")
  colnames(val.data) <- c("tpr", "fpr", "dataset")
  
  graph.data <- rbind(trn.data, val.data)
  colnames(graph.data) <- c("tpr", "fpr", "dataset")
  
  graph <- ggplot(data = graph.data, aes(x = fpr, y = tpr, color =
                                           dataset)) + ggtitle(title) +
    geom_line() + geom_abline(intercept = 0, slope = 1) + 
    annotate("text", label = paste("Training AUC = ", percent(trn.auc2), sep=""), x = .8, y = .4) + 
    annotate("text", label = paste("Validation AUC = ", percent(val.auc2), sep=""), x = .8, y = .2) +
    annotate("text", label = paste("Training KS = ", percent(trn.ks), sep=""), x = .8, y = .3) + 
    annotate("text", label = paste("Validation KS = ", percent(val.ks), sep=""), x = .8, y = .1) +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) +
    xlab("False Positive Rate") + ylab("True Positive Rate")
  #   print(graph)
  return(graph)
}
forestPred <- as.data.frame(predict(rf1, data_skinny, type="prob"))
names(forestPred) <- c('prob_bad_rf', "prob_good_rf")
data_skinny$prob_good_rf <- forestPred$prob_good_rf
auc_rf1 <- plot.AUC(data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    "Random Forest 1 - Good WR AUC")
auc_rf1
#we gotta dial back the forest a bit since this is overfit
#lets scale back to 25 trees, force more observations into end nodes, put more obs in OOB
set.seed(1)
data_skinny <- data_skinny %>% select(-prob_good_rf)
rf2 <- randomForest(good ~ ., data=data_skinny,
                    ntree = 25,
                    mtry = 3,
                    importance = TRUE,
                    nodesize = 20,
                    sampsize = 75,
                    replace = TRUE,
                    keepforest = T)
#show importance
varImpPlot(rf2)
#show confusion matrix (threshold = 0.5 at baseline)
rf2$confusion
#actually this made it better somehow...
#based on this we are gonna get a sick c-statistic
forestPred <- as.data.frame(predict(rf2, data_skinny, type="prob"))
names(forestPred) <- c('prob_bad_rf', "prob_good_rf")
data_skinny$prob_good_rf <- forestPred$prob_good_rf
auc_rf2 <- plot.AUC(data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    "Random Forest 1 - Good WR AUC")
auc_rf2
#lets turn the dials down again to see if we can further generalize this framework
#if we can get something in the 80s that is very general, we can call it a champion and say it is the winner
#force more obs to OOB but leave node size alone - slightly more general nodes
#remove variables that actually decrease or do not increase accuracy
#remove variables that are below TDs in terms of gini decrease
#less vars - only try 2 variables at each split
set.seed(1)
data_skinny <- data_skinny %>% select(-prob_good_rf, -ranked_end_season_flag, -power5_flag, -LONG, -bmi, -shuttle,
                                      -ypc)
rf3 <- randomForest(good ~ ., data=data_skinny,
                    ntree = 25,
                    mtry = 2,
                    importance = TRUE,
                    nodesize = 20,
                    sampsize = 70,
                    replace = TRUE,
                    keepforest = T)
#show importance
varImpPlot(rf3)
#show confusion matrix (threshold = 0.5 at baseline)
rf3$confusion
forestPred <- as.data.frame(predict(rf3, data_skinny, type="prob"))
names(forestPred) <- c('prob_bad_rf', "prob_good_rf")
data_skinny$prob_good_rf <- forestPred$prob_good_rf
auc_rf3 <- plot.AUC(data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    "Random Forest 1 - Good WR AUC")
auc_rf3
#the perfomance on this is really really really strong
#lets make sure the intuition on the forest is correct
#break data fields into quantiles and check prediction by quintiles for expected relationships
plot.quints <- function(trn_df, val_df, pred, actual, quintfield, title, xlab, ylab, cols){
  p <- enquo(pred)
  a <- enquo(actual)
  q <- enquo(quintfield)
  #base quintiles on training
  quints <- quantile(trn_df %>% pull(!!q), c(.2, .4, .6, .8))
  #place quintiles in OOT data
  val_df <- val_df %>% mutate(quints_eval = ifelse(!!q <= quints[[1]], 1,
                                                   ifelse(!!q <= quints[[2]], 2,
                                                          ifelse(!!q <= quints[[3]], 3,
                                                                 ifelse(!!q <= quints[[4]], 4, 5)))))
  #summarise by quintile for OOT data
  val_sum <- val_df %>% group_by(quints_eval) %>% summarise(actual_rate = mean(!!a), pred_rate = mean(!!p))
  #melt for graphing
  melted <- melt(val_sum, id.vars = 'quints_eval', measure_vals = c('actual_rate', 'pred_rate'))
  #graph
  graph <- ggplot(melted, aes(x = quints_eval, y = value, fill = variable)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_y_continuous(labels = percent) +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    scale_fill_manual(name = "Rate Type", labels = c("Actual 18M Rate", "Predicted 18M Rate"),
                      values = cols)
  return(graph)
 
  
}
#send to numeric before doing this - need to be able to take the avg of the field
data_skinny$good <- as.numeric(as.character(data_skinny$good))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, pick, 
            "Predicted vs Actual by Quintile", "Draft Pick Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, YDS, 
            "Predicted vs Actual by Quintile", "Yards Gained Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, REC, 
            "Predicted vs Actual by Quintile", "Receptions Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, forty, 
            "Predicted vs Actual by Quintile", "40 Yard Dash Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, agility_score, 
            "Predicted vs Actual by Quintile", "Agility Score Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, TD, 
            "Predicted vs Actual by Quintile", "College TDs Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, highSchoolRating, 
            "Predicted vs Actual by Quintile", "High School Rating Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, threecone, 
            "Predicted vs Actual by Quintile", "3 Cone Shuttle Time Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, height_inches, 
            "Predicted vs Actual by Quintile", "Height in Inches Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))

#we gotta check the distribution of 40s, 3 cones, and shuttles - this is odd that slower players are better
#maybe our distribution is truncated because we are only looking at drafted players?
quantile(data_skinny$forty, probs = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
quantile(data_skinny$threecone, probs = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))

#we have to (once again) remove these speed related fields - these results are non-intuitive
#the data is suggesting slower players are better, which is non-intuitive and suggests there is a "missing" link in here
#some of these fields also were heavily imputed, so that doesn't help either
#maybe we load the imputation data and use that to calculate 2 new fields
#field 1 - combine complete - did the player compete in every event in the combine
#field 2 - number of combine events completed - did players who "held out" of a drill do so to hide a deficiency that later showed up in the NFL?
# load the data
load(paste(inputLoc, "NFL_Featureselection_w_imputeflags.rdata", sep = ""))

new_skinny <- nfl_conf %>%mutate(power5_flag = ifelse(conf %in% c("SEC", "Pac-12", "Big 12", "Big 10", "ACC"), 1, 0),
                                ranked_end_season_flag = ifelse(rank <= 25, 1, 0),
                                bmi = 703 * (weight/(height_inches * height_inches)),
                                speed_score = (weight * 200)/(forty*forty*forty*forty),
                                height_adjusted_speed_score = (weight * 200)/(forty*forty*forty*forty)/73, #avg WR height in NFL 
                                explosion_score = bench + vertical + broad/12,
                                agility_score = threecone + shuttle,
                                good = ifelse(epa_3season >= epa_cutoff,1,0),
                                combine_events_completed = 8 - (height_inches_impute_flag + weight_impute_flag + forty_impute_flag +
                                  vertical_impute_flag + bench_impute_flag + broad_impute_flag + 
                                  threecone_impute_flag + shuttle_impute_flag)) %>% select_if(is.numeric)%>%
                                mutate(combine_complete = ifelse(combine_events_completed == 8, 1, 0))
#frequency table to check 2 new variables and if they make a difference in separating good from bad
table(new_skinny$good, new_skinny$combine_complete)
table(new_skinny$good, new_skinny$combine_events_completed)
#there isn't a huge difference here. in fact you could actually argue more good players skip an event (as a percentage)
#maybe have a selection bias issue here: only drafted players so by nature they are better
#most college players with bad speed times won't even get drafted at WR
#hence the correlations/expectations should maybe change
#remove these fields from the models since we aren't sure what is going on here
set.seed(1)
data_skinny <- data_skinny %>% select(-prob_good_rf, -forty,-threecone, -agility_score)
data_skinny$good <-  relevel(as.factor(data_skinny$good), ref = '0')
rf4 <- randomForest(good ~ ., data=data_skinny,
                    ntree = 25,
                    mtry = 2,
                    importance = TRUE,
                    nodesize = 20,
                    sampsize = 70,
                    replace = TRUE,
                    keepforest = T)
#show importance
varImpPlot(rf4)
#show confusion matrix (threshold = 0.5 at baseline)
rf4$confusion
forestPred <- as.data.frame(predict(rf4, data_skinny, type="prob"))
names(forestPred) <- c('prob_bad_rf', "prob_good_rf")
data_skinny$prob_good_rf <- forestPred$prob_good_rf
auc_rf4 <- plot.AUC(data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    data_skinny$prob_good_rf, as.numeric(data_skinny$good),
                    "Random Forest 4 - Good WR AUC")
auc_rf4
data_skinny$good <- as.numeric(as.character(data_skinny$good))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, pick, 
            "Predicted vs Actual by Quintile", "Draft Pick Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, YDS, 
            "Predicted vs Actual by Quintile", "Yards Gained Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, REC, 
            "Predicted vs Actual by Quintile", "Receptions Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, TD, 
            "Predicted vs Actual by Quintile", "College TDs Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, highSchoolRating, 
            "Predicted vs Actual by Quintile", "High School Rating Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))
plot.quints(data_skinny, data_skinny, prob_good_rf, good, height_inches, 
            "Predicted vs Actual by Quintile", "Height in Inches Quintile", "Probability of being a good player", 
            c("#311D00", '#FF3C00'))

#ok lets reload and re-transform the raw data and score everyone to see how we did on actual player names
# re-load the data
load(paste(inputLoc, "NFL_Featureselection_wo_imputeflags.rdata", sep = ""))

#add transformations from previous attempts at models on continuous versions of fields
#don't forget to reclassify target!!!
epa_cutoff <- quantile(nfl_conf_noimp$epa_3season,0.7)
data <- nfl_conf_noimp %>% mutate(power5_flag = ifelse(conf %in% c("SEC", "Pac-12", "Big 12", "Big 10", "ACC"), 1, 0),
                                  ranked_end_season_flag = ifelse(rank <= 25, 1, 0),
                                  bmi = 703 * (weight/(height_inches * height_inches)),
                                  speed_score = (weight * 200)/(forty*forty*forty*forty),
                                  height_adjusted_speed_score = (weight * 200)/(forty*forty*forty*forty)/73, #avg WR height in NFL 
                                  explosion_score = bench + vertical + broad/12,
                                  agility_score = threecone + shuttle,
                                  good = ifelse(epa_3season >= epa_cutoff,1,0))
forestPred <- as.data.frame(predict(rf4, data, type="prob"))
names(forestPred) <- c('prob_bad_rf', "prob_good_rf")
data$prob_good_rf <- forestPred$prob_good_rf
auc_rf4 <- plot.AUC(data$prob_good_rf, as.numeric(data$good),
                    data$prob_good_rf, as.numeric(data$good),
                    "Random Forest 4 - Good WR AUC")
auc_rf4

#save data for visualization usage
save(data, file = paste(outputLoc, "data_scored_rf4.rdata", sep = ""))
write.csv(data, file = paste(outputLoc, "data_scored_rf4.csv", sep = ""))