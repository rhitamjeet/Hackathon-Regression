train = read.csv("D:/datasets/SocGen problem1/train.csv", stringsAsFactors = F)
test = read.csv("D:/datasets/SocGen problem1/test.csv", stringsAsFactors = F)

library(dplyr)

full = bind_rows(train,test)
date_columns = c("creation_date","sell_date","start_date")

for (i in 1:3)
{
  year = substr(full[,date_columns[i]],1,4)
  month = substr(full[,date_columns[i]],5,6)
  date = substr(full[,date_columns[i]],7,8)
  
  full[,date_columns[i]] = paste(paste(year, month, sep = '-'),date, sep = "-")
  full[,date_columns[i]] = as.Date(full[,date_columns[i]], format = "%Y-%m-%d")
}

colSums(is.na(full))
#EDA
eda1 = full %>% group_by(start_date,pf_category) %>% summarise(returns = mean(return,na.rm = T)) 
View(eda1)
eda2 =eda1[complete.cases(eda1),]
ggplot(eda2,aes(start_date,return,col = pf_category)) + geom_line()
# NA treatment in libor rate
unique(full[is.na(full$libor_rate),]$creation_date)

library(ggplot2)

ggplot(full) + geom_point(aes(x = start_date, y = euribor_rate), col = 'red')+ geom_point(aes(x = start_date, y = libor_rate), col = 'blue')

# we see that the euribor rate is quite constant compared to the libor rate. we will first impute the mean. later will try to get more exact values and see the accuraccy. methods - Missforest , EDA

a1 = full[complete.cases(full$libor_rate,full$euribor_rate),]

cor(a1$libor_rate,a1$euribor_rate)
# it is higly correlated . we will explore this further to see if we can impute exact values. the date columns need to be used

#for now spline method
library(zoo)
plot(na.spline(full$libor_rate), type = 'l')

full$libor_rate = na.spline(full$libor_rate)
full$bought = na.spline(full$bought)
full$sold = na.spline(full$sold)
hist(train$return)
boxplot(train$return)
library(moments)
boxplot(train$return + 1)
box_plot$stats
hist(train$return + 1)
# outliers treatment

full[full$return>0.2 & !is.na(full$return),]$return = 0.1452
train[train$return>0.2,]$return = 0.1452

skewness(train$return)
hist(train$return)
#sqrt root transformation
#will do later

# Categorical Variables and Numerical Variables
full1 = full[-c(5,12,14)] # removing date columns
feature_classes = sapply(names(full1),function(x){class(full1[[x]])})
numeric_feats = names(feature_classes[feature_classes != "character"])
numeric_feats = numeric_feats[-c(5,6,7,8)]
categorical_feats = names(feature_classes[feature_classes == "character"])
categorical_feats = categorical_feats[-c(1,2,3)]
library(caret)

dummies <- dummyVars(~. ,full1[categorical_feats],levelsOnly = T)
categorical_1_hot <- predict(dummies,full1[categorical_feats])
categorical_1_hot[is.na(categorical_1_hot)] <- 0  #for any level that was NA, set to zero

#Combining the data 
full2 = cbind(full1[numeric_feats], categorical_1_hot)

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)
# test out Ridge regression model

lambdas <- seq(1,0,-0.001)

# train model

model_ridge <- train(x=full2[1:nrow(train),],y=train$return,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))

coef <- data.frame(coef.name = dimnames(coef(model_ridge$finalModel,s=model_ridge$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_ridge$finalModel,s=model_ridge$bestTune$lambda)))

mean(model_ridge$resample$Rsquared)

picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))

coef <- arrange(coef,-coef.value)
imp_coef <- rbind(head(coef,10),
                  tail(coef,10))

ggplot(imp_coef) +
  geom_bar(aes(x=reorder(coef.name,coef.value),y=coef.value),
           stat="identity") +
  
  coord_flip() +
  ggtitle("Coefficents in the Ridge Model") +
  theme(axis.title=element_blank())

#prediction 
ypred = predict(model_ridge,newdata = full2[9367:14167,])
final = data.frame(test$portfolio_id,ypred)
colnames(final) = c("portfolio_id","return")
write.csv(final , "D:/datasets/SocGen problem1/ridge2.csv", row.names = F)

#lasso
model_lasso <- train(x=full2[1:nrow(train),],y=train$return,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1, #Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),0.00075,0.0005,0.0001)))

coef <- data.frame(coef.name = dimnames(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda)))

mean(model_lasso$resample$Rsquared)

picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))

coef <- arrange(coef,-coef.value)
imp_coef <- rbind(head(coef,10),
                  tail(coef,10))

ggplot(imp_coef) +
  geom_bar(aes(x=reorder(coef.name,coef.value),y=coef.value),
           stat="identity") +
  
  coord_flip() +
  ggtitle("Coefficents in the Lasso Model") +
  theme(axis.title=element_blank())

#prediction 
ypred = predict(model_lasso,newdata = full2[9367:14167,])
final = data.frame(test$portfolio_id,ypred)
colnames(final) = c("portfolio_id","return")
write.csv(final , "D:/datasets/SocGen problem1/lasso.csv", row.names = F)



#Anomoly Detection -??~~
#multi collinearity -?
#time series?? how to use
#desk id feature engineering is lowering accuracy
# columns not used so far - indicator, hedge , status - error while modelling

