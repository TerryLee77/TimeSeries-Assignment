data <- read.csv('./Data/datafull.csv')

### Exploration Data Analysis

### Pre-Processing

library(astsa)


library(mice)

## rf 填补

## Test

### 尝试手动剔除变量，看看填补效果,选400个作为缺失值（缺失了788项)

data$WD <- as.factor(data$WD)

data_omit <- na.omit(data[,-1])
data_temp <- na.omit(data[,-1])

data_temp[sample(1:dim(data_temp)[1],400),] <- NA

miceMod_temp <- mice(data_temp,m=5,method = 'rf')
miceOutput_temp <-complete(miceMod_temp) 

# 查看精度
library(DMwR)
## remotes::install_github("cran/DMwR")

temp_pm <- miceOutput_temp[is.na(data_temp$PM2.5),'NO2']

real_pm <- data_omit[is.na(data_temp$PM2.5),'NO2']

regr.eval(temp_pm,real_pm)

# RMSE

## Factor

temp_wd <- miceOutput_temp[is.na(data_temp$WD),'WD']

real_wd <- data_omit[is.na(data_temp$WD),'WD']

sum(temp_wd!=real_wd)/400

## MEAN

data_temp_num <- data_temp[,-10]

miceMod_temp_mean <- mice(data_temp_num,m=5,method = 'mean')
miceOutput_temp_mean <-complete(miceMod_temp_mean)

temp_pm_mean <- miceOutput_temp_mean[is.na(data_temp$PM2.5),'NO2']
regr.eval(temp_pm_mean,real_pm)

# pmm

miceMod_temp_pmm <- mice(data_temp_num,m=5,method = 'pmm')
miceOutput_temp_pmm <-complete(miceMod_temp_pmm)

temp_pm_pmm <- miceOutput_temp_pmm[is.na(data_temp$PM2.5),'NO2']
regr.eval(temp_pm_pmm,real_pm)

#lasso.norm

miceMod_temp_lasso.norm <- mice(data_temp_num,m=5,method = 'lasso.norm')
miceOutput_temp_lasso.norm <-complete(miceMod_temp_lasso.norm)

temp_pm_lasso.norm<- miceOutput_temp_lasso.norm[is.na(data_temp$PM2.5),'NO2']
regr.eval(temp_pm_lasso.norm,real_pm)

# midastouch

miceMod_temp_midastouch <- mice(data_temp_num,m=5,method = 'midastouch')
miceOutput_temp_midastouch <-complete(miceMod_temp_midastouch)


temp_pm_midastouch<- miceOutput_temp_midastouch[is.na(data_temp$PM2.5),'NO2']
regr.eval(temp_pm_midastouch,real_pm)



## Random Forests for importance variable

library(randomForest)
library(doParallel)
library(foreach)

data_train <- data[,-1]

data_train <- data_train[,-12]
data_train <- data_train[,-12]

data_train <- data_train[,-11]

library(ggplot2) 

# PM2.5

cl <- makeCluster(16)
registerDoParallel(cl)

quick_RF <- foreach(ntree=rep(125, 16), 
                    .combine=combine,
                    .packages='randomForest') %dopar%
  randomForest(x=data_train[ ,!colnames(data_train) %in% colnames(data_train)[1:6]], y=data_train$PM2.5, ntree=ntree,importance=TRUE)
stopCluster(cl) 

imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]



ggplot(imp_DF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")+ggtitle('PM2.5')+theme(plot.title = element_text(hjust = 0.5)) 

#PM10


cl <- makeCluster(16)
registerDoParallel(cl)

quick_RF <- foreach(ntree=rep(125, 16), 
                    .combine=combine,
                    .packages='randomForest') %dopar%
  randomForest(x=data_train[ ,!colnames(data_train) %in% colnames(data_train)[1:6]], y=data_train$PM10, ntree=ntree,importance=TRUE)
stopCluster(cl) 

imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")+ggtitle('PM10')+theme(plot.title = element_text(hjust = 0.5)) 


#SO2

cl <- makeCluster(16)
registerDoParallel(cl)

quick_RF <- foreach(ntree=rep(125, 16), 
                    .combine=combine,
                    .packages='randomForest') %dopar%
  randomForest(x=data_train[ ,!colnames(data_train) %in% colnames(data_train)[1:6]], y=data_train$SO2, ntree=ntree,importance=TRUE)
stopCluster(cl) 

imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")+ggtitle('SO2')+theme(plot.title = element_text(hjust = 0.5)) 


#NO2

cl <- makeCluster(16)
registerDoParallel(cl)

quick_RF <- foreach(ntree=rep(125, 16), 
                    .combine=combine,
                    .packages='randomForest') %dopar%
  randomForest(x=data_train[ ,!colnames(data_train) %in% colnames(data_train)[1:6]], y=data_train$NO2, ntree=ntree,importance=TRUE)
stopCluster(cl) 

imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")+ggtitle('NO2')+theme(plot.title = element_text(hjust = 0.5)) 




# O3

cl <- makeCluster(16)
registerDoParallel(cl)

quick_RF <- foreach(ntree=rep(125, 16), 
                    .combine=combine,
                    .packages='randomForest') %dopar%
  randomForest(x=data_train[ ,!colnames(data_train) %in% colnames(data_train)[1:6]], y=data_train$O3, ntree=ntree,importance=TRUE)
stopCluster(cl) 

imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")+ggtitle('O3')+theme(plot.title = element_text(hjust = 0.5)) 

## CO

cl <- makeCluster(16)
registerDoParallel(cl)

quick_RF <- foreach(ntree=rep(125, 16), 
                    .combine=combine,
                    .packages='randomForest') %dopar%
  randomForest(x=data_train[ ,!colnames(data_train) %in% colnames(data_train)[1:6]], y=data_train$CO, ntree=ntree,importance=TRUE)
stopCluster(cl) 

imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")+ggtitle('CO')+theme(plot.title = element_text(hjust = 0.5)) 



