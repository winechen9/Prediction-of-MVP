oldw<-getOption("warn")
options(warn=-1)
#wd and libraries----
setwd("C:/Users/Mike/Documents/Data_Mining/Milestone_4/1/All")
# library 
library(dplyr)
library(car)
library(psych)
library(glmnet)
library(ggplot2)
library(earth)
library(mice)
library(kernlab)
library(ipred)
library(randomForest)
library(caret)
library(nnet)
library(rpart)
library(flux)

seeds<-sample(1:1000,100,replace=F)
seeds<-c(912,825,440,11)
area_roc<-data.frame(seed=1:4,area=1:4)
#read the bat data assign it to batting----
batting <- read.csv("3-Batting.csv")
# subset the data which is after 1910 , because the scale award data
batting <- filter(batting, yearID >1910 )
# delete which column we do not need
batting <- select(batting,-CS,-GIDP,-SO)

# check the data
md.pattern(batting)
# there are 5149 player only have the G data, so delete them
local_v2 <- !is.na(batting$HBP)
batting <- batting[local_v2,]




#model the SF missing data needed for wOBA calculations----
SF_NA_vector <- is.na(batting$SF)
SF_nonNA_vector <- !SF_NA_vector
battingSF_pred_set <- filter(batting,SF_NA_vector)
battingSF_exist_set <- filter(batting,SF_nonNA_vector)
#divide train and test set
battingSF_Train_Size <- floor(0.70 * nrow(battingSF_exist_set))
set.seed(1)
battingSF_train_index <- sample(seq_len(nrow(battingSF_exist_set)), size = battingSF_Train_Size)
battingSF_test_index <- (1:60096)[-battingSF_train_index]
battingSF_trainset <- battingSF_exist_set[battingSF_train_index,]
battingSF_testset <- battingSF_exist_set[battingSF_test_index,]



#predict SF use KSVM  mse = 0.5033399  
# fit model takes a lot time
#fit <- ksvm(SF~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+HBP+SH, data = battingSF_trainset)
#fit_ksvm <- fit 
# summarize the fit
#summary(fit_ksvm)
# make predictions
#predictions <- predict(fit_ksvm, battingSF_trainset)
#predictions <- round(predictions)
# summarize accuracy
#rmse <- mean((battingSF_trainset$SF - predictions)^2)
#print(rmse)
#hist(battingSF_trainset$SF)
#hist(predictions)

#battingSF_testset_pred <- predict(fit_ksvm,battingSF_testset)
#rmse <- mean((battingSF_testset$SF - battingSF_testset_pred)^2)
#print(rmse)  #   is 1.698957 
#hist(battingSF_testset_pred)
# the result takes too much time

#predict SF use MARS and mes
# fit model
fit <- earth(SF~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+HBP+SH, data = battingSF_trainset)
mars_fit <-fit
# summarize the fit
summary(mars_fit)
# summarize the importance of input variables
evimp(mars_fit)
# make predictions
predictions <- predict(mars_fit, battingSF_trainset)
predictions <- round(predictions)
# summarize accuracy
rmse <- mean((battingSF_trainset$SF - predictions)^2)
print(rmse)   

battingSF_testset_pred <- predict(mars_fit,battingSF_testset)
local_v1 <- battingSF_testset_pred <0
battingSF_testset_pred[local_v1]  <- 0
rmse <- mean((battingSF_testset$SF - battingSF_testset_pred)^2)
print(rmse)   # is 1.158477
hist(battingSF_testset_pred)
# done   mse  = 1.194475


#decide to use the MARS model, deal with the predictions
pred_SF <- predict(fit,battingSF_pred_set)
pred_SF <-round(pred_SF)
batting$SF[SF_NA_vector] <- pred_SF

b<-batting$AB > 0

#model IBB missing data for wOBA calcualtions----
#check data
md.pattern(batting)

IBB_NA_vector <- is.na(batting$IBB)
IBB_nonNA_vector <- !IBB_NA_vector
battingIBB_pred_set <- filter(batting,IBB_NA_vector)
battingIBB_exist_set <- filter(batting,IBB_nonNA_vector)
#divide train and test set
battingIBB_Train_Size <- floor(0.70 * nrow(battingIBB_exist_set))
set.seed(2)
battingIBB_train_index <- sample(seq_len(nrow(battingIBB_exist_set)), size = battingIBB_Train_Size)
battingIBB_test_index <- (1:59549)[-battingIBB_train_index]
battingIBB_trainset <- battingIBB_exist_set[battingIBB_train_index,]
battingIBB_testset <- battingIBB_exist_set[battingIBB_test_index,]

#do IBB with mars
fit <- earth(IBB~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+HBP+SH+SF, data = battingIBB_trainset)
IBB_mars_fit <-fit
# summarize the fit
summary(IBB_mars_fit)
# summarize the importance of input variables
evimp(IBB_mars_fit)
# make predictions
predictions <- predict(IBB_mars_fit, battingIBB_trainset)
predictions <- round(predictions)
# summarize accuracy
rmse <- mean((battingIBB_trainset$IBB - predictions)^2)
print(rmse)   
battingIBB_testset_pred <- predict(IBB_mars_fit,battingIBB_testset)
battingIBB_testset_pred <- round(battingIBB_testset_pred)
local_v3 <- battingIBB_testset_pred < 1

battingIBB_testset_pred[local_v3]  <- 0
rmse <- mean((battingIBB_testset$IBB - battingIBB_testset_pred)^2)
print(rmse)   
hist(battingIBB_exist_set$IBB,breaks = 200)
hist(battingIBB_testset_pred,breaks = 200)

# do IBB predictions, use mars
pred_IBB <- predict(IBB_mars_fit,battingIBB_pred_set)
pred_IBB <-round(pred_IBB)
local_v4 <- pred_IBB <0 
pred_IBB[local_v4] <- 0
batting$IBB[IBB_NA_vector] <- pred_IBB
#check the data again
md.pattern(batting)

#remove players prior to 1911 & had few at-bats
batting <-subset(batting,AB>=50 & yearID>1910)
b<-batting$AB > 0


#wOBA calculation----
attach(batting)
#calculate on-base-percentage (OBP)
batting$OBP<-(H+BB+HBP)/(AB+BB+HBP+SF)
#calculate batting average
batting$AVG<-H/AB
#total bases
batting$TB<- H + X2B + (2*X3B) + (3*HR)
#slugging percentage
batting$SLG<- batting$TB/AB

#start wOBA calculation
# X1B = number of singles
batting$X1B<-batting$H-batting$X2B-batting$X3B-batting$HR
# bring in wOBA annual correction data from Fan Graphs and join to bat
woba<-read.csv("wOBA.csv", header=T)
names(woba)[1] <- "yearID"
bat_woba<- inner_join(batting,woba)
# calculate wOBA as new column in "bat" object
bat_woba$wOBA <- ((bat_woba$BB * bat_woba$wBB) + (bat_woba$HBP * bat_woba$wHBP) + 
                    (bat_woba$X1B * bat_woba$w1B) + (bat_woba$X2B * bat_woba$w2B) +
                    (bat_woba$X3B * bat_woba$w3B) + (bat_woba$HR * bat_woba$wHR)) / 
  (bat_woba$AB + bat_woba$BB - bat_woba$IBB + bat_woba$SF + bat_woba$HBP)
#give the value to batting dataset
batting$wOBA <- bat_woba$wOBA

#import teams data---- 
teams<- read.csv("Teams.csv",header = T)
batting<-merge(batting,teams,by.x=c("teamID","yearID"),by.y=c("teamID","yearID"))

#import MVP award data---- 
award<-read.csv("AwardsPlayers.csv",header = T)
award <- award[,1:4]
award<-subset(award,awardID=="Most Valuable Player")
batting <- merge(batting,award,by.x=c("playerID","yearID","lgID.x"),by.y=c("playerID","yearID","lgID"), all = T)
md.pattern(batting)
local_v1 <- !is.na(batting$awardID)
batting$awardID <- local_v1
local_v2 <- !is.na(batting$G.x)
batting <- batting[local_v2,]
md.pattern(batting)

#import WAR stats ----
war<-read.csv("war_daily_bat.csv",header = T)
#create column war$ID to join to bat$ID
war$player_ID<-as.character(war$player_ID)
ofarrel<-subset(batting, playerID=="ofarrbo01" & yearID==1926)
ofarrel$war<-3.55
#fix IDs we couldn't get through automation
war$player_ID[war$name_common == "Jacque Jones"] <- "jonesja04"
war$player_ID[war$name_common == "Jason Jones"] <- "jonesja05"
war$player_ID[war$name_common == "Jorge De La Rosa"] <- "delarjo01"
war$player_ID[war$name_common == "Sam Gray"] <- "graydo02"
war$player_ID[war$name_common == "Rodrigo Lopez"] <- "lopezro01"
war$player_ID[war$name_common == "Ramon Ortiz"] <- "ortizra01"
war$player_ID[war$name_common == "Tike Redman"] <- "redmati01"
war$player_ID[war$name_common == "Jeff Sweeney"] <- "sweened01"
war$player_ID[war$name_common == "U.L. Washington"] <- "washiul01"
war$player_ID[war$name_common == "Buck O'Brien"] <- "obriebu01"
war  <- war [,c(3,4,30)]
colnames(war) <- c("playerID","yearID","war")
batting$X<-1:nrow(batting)
test <- merge(batting,war, by = c("playerID","yearID"), all = F)
# remove duplicates
md.pattern(test)

rem.test<-test[,c(1,2,4)]


batting<-test[!duplicated(rem.test),]
batting<-batting[,-73]
batting<-rbind(batting,ofarrel)

#scale continuous data by league and year----
batting <- filter(batting,lgID.x== 'NL' | lgID.x == 'AL')
colnames(batting)
normal_list <- c(1:4,6:25,73,32,36,37,72)
normal_list <- colnames(batting)[normal_list]
wantnor<- data.frame()
data1 <- batting[,normal_list]
data1$war<-as.numeric(as.character(data1$war))
for (i in 1911:2015){
  a <- filter(data1,yearID== i)
  b <- filter(a,lgID.x == 'NL')
  c <- filter(a,lgID.x == 'AL')
  for( k in 5:25){
    if(length(b[,1]) != 0){
      as.numeric(b[[normal_list[k]]])
      bmax = max(b[[normal_list[k]]])
      bmin = min(b[[normal_list[k]]])
      rangeb = bmax - bmin
      #to avoid dividing by 0
      if(rangeb != 0){
        b[[normal_list[k]]] <- (b[[normal_list[k]]]-bmin)/(rangeb)
      }else {
        b[[normal_list[k]]] <- 0
      }
    }else{}
    if(length(c[,1]) != 0){
      as.numeric(c[,normal_list[k]])
      cmax = max(c[[normal_list[k]]])
      cmin = min(c[[normal_list[k]]])
      rangec = cmax - cmin
      if(rangec != 0){
        c[[normal_list[k]]] <- (c[[normal_list[k]]]-cmin)/(rangec)
      }else {
        c[[normal_list[k]]] <- 0
      }
    }
  }
  wantnor <- rbind(wantnor,b,c)
}

wantnor$LgWin[wantnor$yearID==1994]<-"N"
wantnor$WSWin[wantnor$yearID==1994]<-"N"
names(wantnor)<-c("playerID","yearID","leagueID","teamID","G","AB","R","H","X2B","X3B","HR","RBI","SB","BB","IBB",
                  "HBP","SH","SF","OBP","AVG","TB","SLG","X1B","wOBA","war","W","LgWin","WSWin","awardID")
batting<-wantnor[c(1:14,19:29)]



  
#Clean up and partition data----
#remove pitchers
local_v1 <- batting$awardID == T & batting$AB <0.5
local_v1 <- !local_v1
batting <- batting[local_v1,]
bathold<-batting[,1:4]

for (m in 1:length(seeds)){
#divide train/test set
Train_Size <- floor(0.70 * length(unique(batting$yearID)))
set.seed(seeds[m])
train_index <- sample(unique(batting$yearID), size = Train_Size, replace= F)
test_index <- unique(batting$yearID)[!(unique(batting$yearID) %in% train_index)]
trainset <- batting[batting$yearID %in% train_index,]
testset <- batting[batting$yearID %in% test_index,]
features <- 5:24
b<-batting$AB>0
#Neural network, pretty good----
# ran neural network with many different configs, this was best. 
# fit model # iter1680 converge
fit_ann <- nnet(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin, data=trainset, size=15, decay=0.0001, maxit=15000)
# summarize the fit
summary(fit_ann)
annfit <- fit_ann
# make predictions
predictions <- predict(fit_ann, trainset[,features], type="raw")
boxplot(predictions)
p1 <- predictions
#local_v1 <- predictions >0.0
#p1[local_v1] <- 1
p1 <- round(p1)
confusionMatrix(p1,as.numeric(trainset$awardID),positive=as.character(1))
#deal with test set
predictions2 <- predict(fit_ann, testset[,features], type="raw")
p3 <- predictions2
boxplot(predictions2)
c <- p3 >0.000
p3[c] <- 1
c2 <- round(p3)
confusionMatrix(c2,as.numeric(testset$awardID ),positive=as.character(1))

#Tree-based models, RF pretty good----

#decision tree not good
fit <- rpart(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=trainset)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, trainset[,features])
confusionMatrix(as.numeric(as.logical(predictions)),as.numeric(trainset$awardID),positive=as.character(1))
p1 <- round(predictions)
confusionMatrix(p1,as.numeric(trainset$awardID),positive=as.character(1))

# try bagging decision trees, not good
# fit model
trainset2 <- trainset
trainset2$awardID <- as.factor(trainset2$awardID)
fit <- bagging(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=trainset2)
# make predictions
predictions <- predict(fit, trainset2[,features], type="class")
# summarize accuracy
confusionMatrix(as.numeric(as.logical(predictions)), as.numeric(trainset2$awardID)-1,positive=as.character(1))

#deal with test set 
testset2 <- testset
testset2$awardID <- as.factor(testset2$awardID)
predictions2 <- predict(fit, testset2[,features], type="class")
confusionMatrix(as.numeric(as.logical(predictions2)),as.numeric(testset2$awardID )-1,positive=as.character(1))  
#


#random forest, pretty good
fit_rf <- randomForest(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=trainset)
# make predictions
predictions <- predict(fit_rf, trainset[,features])
# summarize accuracy
p <- predictions
p <- round(p)
table(p, trainset$awardID)
test <- as.numeric(trainset$awardID)
confusionMatrix(as.numeric(as.logical(p)),test,positive=as.character(1))

predictions2 <- predict(fit_rf, testset[,features])
p2 <- predictions2
p2[predictions2>0.02] <-1
p2 <- round(p2)
table(p2,testset$awardID)  
test2 <- as.numeric(testset$awardID)
confusionMatrix(p2,test2,positive=as.character(1))

# KNN, not good -----
trainset2 <- trainset
trainset2$awardID <- as.factor(trainset2$awardID)
fit_knn <- knn3(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=trainset2,k=5)
predictions <- predict(fit_knn, trainset[,features], type="class")
# summarize accuracy
confusionMatrix(as.numeric(as.logical(predictions)), as.numeric(as.logical(trainset2$awardID)),positive=as.character(1))
#testset pred
predictions1 <- predict(fit_knn, testset[,features], type="class")
# summarize accuracy
confusionMatrix(as.numeric(as.logical(predictions1)), as.numeric(as.logical(testset$awardID)),positive=as.character(1))

# SVM, ok ----
fit_svm <- ksvm(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=trainset)
# make predictions
predictions <- predict(fit_svm, trainset[,features], type="response")
boxplot(predictions)
p1 <- predictions
p1 <- round(p1)
confusionMatrix(p1, as.numeric(trainset$awardID),positive=as.character(1))

p2 <- predictions
b <- p2 >0.1
p2[b] <- 1
b2 <- round(p2)
confusionMatrix(b2,as.numeric(trainset$awardID),positive=as.character(1))

#deal with test set
predictions2 <- predict(fit_svm, testset[,features], type="response")
p3 <- predictions2
boxplot(predictions2)
c <- p3 >0.1
p3[c] <- 1
c2 <- round(p3)
confusionMatrix(c2,as.numeric(testset$awardID),positive=as.character(1))

#Logistic regression, not good----
fit_lr <- vglm(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,family = multinomial ,data=trainset)
#
predictions <- predict(fit_lr, trainset[,features], type="response")
boxplot(predictions)
p1 <- predictions
p1 <- round(p1)
colnames(p1) <- c('a1','mvp')
confusionMatrix(p1[,2],as.numeric(trainset$awardID),positive=as.character(1))

p2 <- predictions
local_v1 <- p2[,2] >0.2
p2[,2][local_v1] <- 1
p2 <- round(p2)
confusionMatrix(p2[,2],as.numeric(trainset$awardID),positive=as.character(1))

#naive bayes, not good - too many false positives ----
fit_nb <- naiveBayes(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=trainset2)
predictions <- predict(fit_nb, trainset2[,features])
# summarize accuracy
confusionMatrix(as.numeric(as.logical(predictions)), as.numeric(trainset2$awardID)-1,positive=as.character(1))

predictions2 <- predict(fit_nb, testset[,features])
confusionMatrix(as.numeric(as.logical(predictions2)),as.numeric(testset$awardID),positive=as.character(1))  

#Ensemble Models----
Test_Size2 <- floor(0.50* nrow(trainset))
traindata_index <- sample(seq_len(nrow(trainset)),size = Test_Size2)
traindata <- trainset[traindata_index,]
blenderdata_index <- seq_len(nrow(trainset))[-traindata_index]
blenderdata <- trainset[blenderdata_index,]

#enlarge the traindata MVP 
MVP_rows1 <- traindata[traindata$awardID == T,]
t <- data.frame()
for (i in 1:200){
  t <- rbind(t,MVP_rows1)
}
traindata <- rbind(traindata,t)
set.seed(2)
traindata <- traindata[sample(nrow(traindata)),]
#enlarge the blenderdata MVP
MVP_rows2 <- blenderdata[blenderdata$awardID == T,]
t <- data.frame()
for (i in 1:200){
  t <- rbind(t,MVP_rows1)
}
blenderdata <- rbind(blenderdata,t)
set.seed(3)
blenderdata <- blenderdata[sample(nrow(blenderdata)),]


# ksvm 
#fit_svm <- ksvm(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=traindata)
# make predictions
predictions_svm <- predict(fit_svm, traindata, type="response")
boxplot(predictions_svm)
p_svm <- predictions_svm
p_svm <- round(p_svm)
confusionMatrix(p_svm, as.numeric(traindata$awardID),positive=as.character(1))

# ann
# fit model 
#fit_ann <- nnet(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin, data=traindata, size=15, decay=0.0001, maxit=15000)
# make predictions
predictions_ann <- predict(fit_ann, traindata, type="raw")
boxplot(predictions_ann)
p_ann <- predictions_ann
p_ann <- round(p_ann)
confusionMatrix(p_ann,as.numeric(traindata$awardID),positive=as.character(1))
# random forest 
#fit_rf <- randomForest(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=traindata)
predictions_rf <- predict(fit_rf, traindata)
p_rf <- predictions_rf
p_rf <- round(p_rf)
confusionMatrix(p_rf, as.numeric(traindata$awardID),positive=as.character(1))
# KNN 
traindata2 <- traindata
traindata2$awardID <- as.factor(traindata2$awardID)
#fit_knn <- knn3(awardID~G+AB+R+H+X2B+X3B+HR+RBI+SB+BB+OBP+AVG+TB+SLG+X1B+wOBA+war+LgWin+W+WSWin,data=traindata2,k=2)
predictions_knn <- predict(fit_knn, traindata, type="class")
# summarize accuracy
#confusionMatrix(predictions_knn, as.numeric(traindata2$awardID),positive=as.character(1))

# ann 2 
new_feature <- data.frame()
f1 <- predict(fit_svm,blenderdata,type="response")
f2 <- predict(fit_ann,blenderdata,type="raw")
f3 <- predict(fit_rf,blenderdata)
#blenderdata2 <- blenderdata
#blenderdata2$awardID <- as.factor(blenderdata2$awardID)
f4 <- predict(fit_knn,blenderdata,type = "class")
confusionMatrix(round(f1),as.numeric(blenderdata$awardID),positive=as.character(1))
confusionMatrix(round(f2),as.numeric(blenderdata$awardID),positive=as.character(1))
confusionMatrix(round(f3),as.numeric(blenderdata$awardID),positive=as.character(1))
confusionMatrix(as.numeric(as.logical(f4)),as.numeric(blenderdata$awardID),positive=as.character(1))

new_feature <- cbind(f1,f2,f3,f4,blenderdata$awardID)
colnames(new_feature) <- c("f1","f2","f3","f4","awardID")
fit_ann2 <- nnet(awardID~f1+f2+f3+f4, data=new_feature, size=15, decay=0.0001, maxit=15000)
ff <- predict(fit_ann2,new_feature,type = "raw")
confusionMatrix(round(ff),as.numeric(blenderdata$awardID),positive=as.character(1))

fit_rf2 <- randomForest(awardID~f1+f2+f3+f4,data=new_feature)
ff_rf <- predict(fit_rf2,new_feature)
confusionMatrix(round(ff_rf),as.numeric(blenderdata$awardID),positive=as.character(1))
# test without combine 
t_f1 <- predict(fit_svm,testset,type="response")
t_f2 <- predict(fit_ann,testset,type="raw")
t_f3 <- predict(fit_rf,testset[,features])
t_f4 <- predict(fit_knn,testset,type = "class")

confusionMatrix(round(t_f1),as.numeric(testset$awardID),positive=as.character(1))
confusionMatrix(round(t_f2),as.numeric(testset$awardID),positive=as.character(1))
confusionMatrix(round(t_f3),as.numeric(testset$awardID),positive=as.character(1))
confusionMatrix(as.numeric(as.logical(t_f4)),as.numeric(testset$awardID),positive=as.character(1))

new_feature2 <- cbind(t_f1,t_f2,t_f3,t_f4)
colnames(new_feature2) <-  c("f1","f2","f3","f4")

t_ff <- predict(fit_ann2,new_feature2,type = "raw")
confusionMatrix(round(t_ff),as.numeric(testset$awardID),positive=as.character(1))

####Random Forest gives best ensemble resultstset$awardID))

t_ff_rf <- predict(fit_rf2,new_feature2)
mvp<-as.data.frame(cbind(testset,t_ff_rf))
mvp<-subset(mvp,awardID==TRUE)
#plot(mvp[,1],col=mvp[,2]+1)
test_pred<-cbind(testset,t_ff_rf)
test_pred$lgyr<-NA
for (i in 1:nrow(test_pred)){
  test_pred$lgyr[i]<-paste(test_pred$leagueID[i],test_pred$yearID[i],sep="_")
}
real_mvp <- test_pred[test_pred$awardID==TRUE,]
test_pred1<-test_pred[test_pred$lgyr %in% unique(real_mvp$lgyr),]

sum(t_ff_rf >0.0498)
testk <- test_pred1$t_ff_rf
testk[test_pred1$t_ff_rf >0.05738565] <- 1
confusionMatrix(round(testk),as.numeric(test_pred1$awardID),positive=as.character(1))


#find out how deep we need to go into our predictions for each league and year to find the mvp
samplen<-vector()
for (i in unique(test_pred1$lgyr)){
  tpyr<-subset(test_pred1,test_pred1$lgyr==i)
  yr_preds<-data.frame()
  yr_preds<-arrange(tpyr,desc(t_ff_rf))
  samplen<-c(samplen,match(TRUE,yr_preds$awardID))
}
ncorrect<-as.data.frame(table(samplen))
names(ncorrect)<-c("n","correct")
ncorrect$n<-as.numeric(as.character(ncorrect$n))
ncorrect$correct<-cumsum(ncorrect$correct)
nmvp<-nrow(test_pred1[test_pred1$awardID==T,])-1
ncorrect$new<-c(0,ncorrect$correct[2:nrow(ncorrect)]-ncorrect$correct[1:(nrow(ncorrect)-1)])
ncorrect$sensitivity<-ncorrect$correct/nmvp
ncorrect$specificity<-((ncorrect$n*nmvp)-(ncorrect$correct))/(nrow(test_pred1)-nmvp)
ncorrect$specificity[ncorrect$specificity>1]<-1
ncorrect<-rbind(c(0,0,0,0),ncorrect,c(1,1,1,1))

#plot ROC curve and calculate the area under the curve
plot(ncorrect$sensitivity~ncorrect$specificity,ylim=c(0,1),xlim=c(0,1),type="l",col="red")
abline(a=0,b=1)
area_roc$area[m]<-auc(ncorrect$specificity,ncorrect$sensitivity)[1]
area_roc$seed[m]<-seeds[m]
tpred<-test_pred1[,c(1,2,4,26)]
names(tpred)[4]<-paste("x",seeds[m],sep="_")
bathold<-left_join(bathold,tpred)
print(m)
  }
options(warn=oldw)


#take all of the model outputs and average them for each player
bathold_m<-rowMeans(bathold[5:104],na.rm=T)
bathold_means<-cbind(bathold[,1:4],bathold_sums)
bathold_all<-cbind(bathold_means,batting$awardID)
bathold_all$lgyr<-NA
write.csv(bathold_all,"model_outputs_100.csv")
names(bathold_all)[c(5,6)]<-c("means","awardID")
#remove records for years when the MVP was removed because he was a pitcher
bathold_all<-bathold_all[!is.nan(bathold_all$means),]

#create a column to separate records by league and year - we have many different prediciton spaces - one for each MVP
for (i in 1:nrow(bathold_all)){
  bathold_all$lgyr[i] <- paste(bathold_all$leagueID[i],bathold_all$yearID[i],sep="_")
}

#figure out how deep we need to go in each prediction space to get the mvp
samplen<-vector()
for (i in unique(bathold_all$lgyr)){
      tpyr<-subset(bathold_all,bathold_all$lgyr==i)
      yr_preds<-data.frame()
      yr_preds<-arrange(tpyr,desc(means))
      samplen<-c(samplen,match(TRUE,yr_preds$awardID))
  }
ncorrect<-as.data.frame(table(samplen))
names(ncorrect)<-c("n","correct")
ncorrect$n<-as.numeric(as.character(ncorrect$n))
ncorrect$correct<-cumsum(ncorrect$correct)
nmvp<-nrow(bathold_all[bathold_all$awardID==T,])-1
ncorrect$new<-c(0,ncorrect$correct[2:18]-ncorrect$correct[1:17])
ncorrect$sensitivity<-ncorrect$correct/nmvp
ncorrect$specificity<-((ncorrect$n*nmvp)-(ncorrect$correct))/(nrow(bathold_all)-nmvp)
ncorrect$specificity[ncorrect$specificity>1]<-1
ncorrect<-rbind(c(0,0,0,0),ncorrect,c(1,1,1,1))

#plot ROC and get area under the curve
plot(ncorrect$sensitivity~ncorrect$specificity,ylim=c(0,1),xlim=c(0,1),type="l",col="red")
abline(a=0,b=1)
auc(ncorrect$specificity,ncorrect$sensitivity)

#see how a player's average prediction value changes as we introduce more iterations from Monte Carlo process
timeseries<-data.frame(sample=samplen,lgyr=unique(bathold_all$lgyr))
timeseries$lgyr<-as.character(timeseries$lgyr)
lgyr_splt<-unlist(strsplit(timeseries$lgyr,"_"))
lg<-lgyr_splt[seq(1,length(lgyr_splt),by=2)]
year<-lgyr_splt[seq(2,length(lgyr_splt),by=2)]
timeseries$year<-as.numeric(year)
timeseries$lg<-lg
nl<-subset(timeseries,lg=="NL")
al<-subset(timeseries,lg=="AL")
plot(al$sample~al$year,type="l")
plot(nl$sample~nl$year,type="l")
