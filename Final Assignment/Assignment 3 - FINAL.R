library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(readxl)
library(ggplot2)
library(randomForest)

library(lubridate)
library(Rmisc)  
library(stringr)
library(lattice)
options(scipen=5)

library(dbscan)
library(fpc)
library(factoextra)
library(NbClust)
library(vegan)


##DATA ANALYSIS##

#Load the Data Set
bike=read.csv('hour.csv')

#What does the data set look like?
str(bike)
head(bike)

#Are there any blank records?
sum(is.na(bike))

#Lets look at an overview of the data set
summary(bike)


##DATA PREPARATION##

#Convert the format of the DTEDAY column
bike$dteday=as.Date(bike$dteday)

#Convert the Season to Factors and assign the appropriate labels
bike$season=factor(bike$season, labels = c("Spring", "Summer", "Fall", "Winter"))
table(bike$season)

#Convert the Weathersit to Factors and assign the appropriate labels
bike$weathersit=factor(bike$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
table(bike$weathersit)

#Lets check the structure of the Dataset again to confirm if all the changes were applied and re-examine it
str(bike)
head(bike)


##DATA EXPLORATION##

#What is the relationship between the Seasons and the number of bikes rented?
par(mfrow=c(1,1))
plot(bike$season,bike$cnt,main="Season Vs. Total Bike Rentals",xlab="Season",ylab="Total Bike Rentals") 
#The box plot indicates that the number of bike rentals are the lowest in Spring and start increasing as the improves in the Summer. They peak in the Fall and start declining again as Winter approaches. 

#What is the relationship between the Months and the number of bikes rented?
plot(bike$mnth,bike$cnt,main="Month Vs. Total Bike Rentals",xlab="Month",ylab="Total Bike Rental")    
#This plot is consistent with the previous one and provides a more granular view of the dataset. Bikes rented are lowest in January and February, which are usually the coldest part of the winter. As the weather improves, the number of bikes rented rises March to the summer and peaks in the months of September and October. After that, it starts declining again as the cold weather comes back. 

#Are there any days that are better or worse for bike rentals?
plot(bike$weekday,bike$cnt,main="Weekday Vs. Total Bike Rentals",xlab="Weekday",ylab="Total Bike Rentals") 
#Fewer bike rental occur on days 0 and 6, which represent Sunday and Saturday, respectively. The number of rentals during the weekdays seem to be quite evenly spread. 

#Are there specific hours of the day that are better or worse for bike rentals?
p=ggplot(data=bike, aes(x=hr, y=cnt,fill = hr)) +
  geom_bar(stat="identity", fill="steelblue")+ 
  theme_minimal() + ggtitle("Bike Rentals by the Hour") + xlab("Hour") + ylab("Total Bike Rentals")
p
#Bike Rentals seem to be lowest from midnight to 6 am. They are highest in the morning at 8 am and then again at 5 pm and 6 pm. Rentals between 8 am and 5 pm seem to be quite evenly distributed. 

#How does weather impact bike rentals?
p3=ggplot(data=bike, aes(x=weathersit, y=cnt,fill = weathersit)) +
  geom_bar(stat="identity", fill="forestgreen")+
  theme_minimal()+ ggtitle("Weather Vs. Total Bike Rentals") + xlab("Weather") + ylab("Total Bike Rentals")
p3
#Generally, it is quite evident that Good weather conditions are the best for riding bikes. Normal weather is moderately good while there is very low demand in Bad weather and no demand at all when the weather is Very Bad. 
#ploting weather realted variables including temperture, feeling temperture, humudity and windspeed

#How does Temperature, Feels like Temperature, Humidity, and Windspeed impact bike rentals?
par(mfrow=c(2,2))
plot(bike$temp,bike$cnt, col="lightskyblue4",xlab="Temperture(Celsius)",ylab="Total Bike Rentals",main = "Temperture Vs. Total Bike Rentals")
abline(lm(bike$cnt~bike$temp))
plot(bike$atemp,bike$cnt, col="lightskyblue4",xlab="Feeling Temperture(Celsius)",ylab="Total Bike Rentals",main = "Feels Like Temperature Vs. Total Bike Rentals")
abline(lm(bike$cnt~bike$atemp))
plot(bike$hum,bike$cnt, col="lightskyblue4",xlab="Humidity",ylab="Total Bike Rentals",main = "Humidity Vs. Total Bike Rentals")
abline(lm(bike$cnt~bike$hum))
plot(bike$windspeed,bike$cnt,col="lightskyblue4",xlab="Windspeed",ylab="Total Bike Rentals",main = "Windspeed Vs. Total Bike Rentals")
abline(lm(bike$cnt~bike$windspeed))
#The plots above assume a linear relationship and are somewhat mixed. The regression line for Temperature provides a fairly good approximate of the data, while Feels like Temperature does not seem to represent the data too accurately. Similarly, Humidity provides a fairly good representation of the data but Windspeed provides a rather inaccurate representation. 

#What is the share of Registered Vs. Casual Renters?
par(mfrow=c(1,1))
slices = c(sum(bike$registered), sum(bike$casual))      
lbls = c("Registered","Causual")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices, labels = lbls, main="Registered Vs. Casual Renters")
#As expected, 81% of renters are Registered users while 19% are Casual users.

##MODELING##

#UNSUPERVISED MODELS

#We first remove the Date and Instant variables since they do not contain any valuable information.
#kmeans clustering on all variables
data <- read.csv("hour.csv")
data$dteday <- NULL
data$instant<- NULL

#KMEANS with all variables
model <- cascadeKM(data, 1, 10, iter = 10)
model$results[2,]
which.max(model$results[2,])
#recomends 10 clusters most based on registrations
kfullresult <- kmeans(data,10)
kfullresult$size
plot(data$cnt, col=kfullresult$cluster, xlab="Clusters",ylab="Total Bike Rentals",main = "Clusters with All Variables")


#KMEANS without registered and casual users
data$dteday <- NULL
data$instant<- NULL
data$casual <- NULL
data$registered <- NULL
data$cnt <- NULL
model <- cascadeKM(data, 1, 10, iter = 10)
model$results[2,]
which.max(model$results[2,])
#recomends 2 groups
kresult <- kmeans(data,2)
kresult$size
#cnt registered and casual are the biggest factors in the groupings probably due to their variance


#Clustering without registered and casual users
data$dteday <- NULL
data$instant<- NULL
data$casual <- NULL
data$registered <- NULL
data$cnt <- NULL
model <- cascadeKM(data, 1, 10, iter = 10)
model$results[2,]
which.max(model$results[2,])
#recomends 2 groups
kresult <- kmeans(data,2)
kresult$size
#cnt registered and casual are the biggest factors in the groupings probably due to their variance

#Clustering by time 
dataTime<-data
dataTime$dteday<-NULL
dataTime$weathersit <- NULL
dataTime$atemp <- NULL
dataTime$temp <- NULL
dataTime$hum <- NULL
dataTime$windspeed <- NULL
dataTime$casual <- NULL
dataTime$registered <- NULL
dataTime$cnt <- NULL
dataTime$instant<-NULL
#clustering by time gives 2 clusters
model <- cascadeKM(dataTime, 1, 10, iter = 10)
model$results[2,]
which.max(model$results[2,])
#recomends 2 clusters
kTimeresult <- kmeans(dataTime,2)
kTimeresult$size
plot(data$yr,data$cnt,col=kTimeresult$cluster, xlab="Time", ylab="Total Bike Rentals", main = "Clusters based on Time")
plot(data$season,data$cnt, col=kTimeresult$cluster)
plot(data$yr,data$cnt,col=kTimeresult$cluster)
plot(data$mnth,data$cnt,col=kTimeresult$cluster)
plot(data$hr,data$cnt,col=kTimeresult$cluster)
plot(data$holiday,data$cnt,col=kTimeresult$cluster)
plot(data$weekday,data$cnt,col=kTimeresult$cluster)
plot(data$workingday,data$cnt,col=kTimeresult$cluster)

#cluster by weather conditions
dataWeather<-data
dataWeather$dteday <- NULL
dataWeather$yr <- NULL
dataWeather$mnth <- NULL
dataWeather$hr <- NULL
dataWeather$holiday <- NULL
dataWeather$weekday <- NULL
dataWeather$workingday <- NULL
dataWeather$instant<-NULL
dataWeather$casual <- NULL
dataWeather$registered <- NULL
dataWeather$cnt <- NULL
model <- cascadeKM(dataWeather, 1, 10, iter = 10)
model$results[2,]
which.max(model$results[2,])
#recomends 8 clusters
kweatherresult <- kmeans(dataWeather,8)
kweatherresult$size

plot(data$cnt, data$weathersit, col=kweatherresult$cluster, xlab="Weather", ylab="Total Bike Rentals", main = "Clusters based on Weather")
plot(data$weathersit,data$cnt,col=kweatherresult$cluster)
plot(data$temp,data$cnt,col=kweatherresult$cluster)
plot(data$atemp,data$cnt,col=kweatherresult$cluster)
plot(data$hum,data$cnt,col=kweatherresult$cluster)
plot(data$windspeed,data$cnt,col=kweatherresult$cluster)

#PCA Analysis
data <- read.csv("hour.csv")
data$instant <- NULL
data$dteday <- NULL
data$casual <- NULL
data$registered <- NULL
data$cnt <- NULL
pca <- prcomp(data)
cor(data)
#The correlation matrix shows that the variables are not highly correlated.
print(pca)
summary(pca)
abs(pca$rotation)
plot(pca, type="lines", main="PCA")
screeplot(pca, main="PCA")
#First 3 components are the most important
biplot(pca)
#its a lot of noise but its shows how hr season mnth  are important on the component graph
plot(pca$x[,1], pca$x[,2])
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
varimax(pca$rotation[,1:5])
pca$center

#SUPERVISED MODELS

#Random Forest
data <- read.csv("hour.csv")
set.seed(123)
tree.rf=randomForest(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, data=data,importance=TRUE,ntree=500)
plot(tree.rf)
varImpPlot(tree.rf)
predict.rf=predict(tree.rf,data)
RMSE.forest=sqrt(mean((predict.rf-data$cnt)^2))
MSE.forest=mean((predict.rf-data$cnt)^2)
MAE.forest=mean(abs(predict.rf- data$cnt))
saveRDS(tree.rf, "model.rds")

#Liner Regression Model
data <- read.csv("hour.csv")
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
lModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, data, method="glm", trControl=train_control,importance=T)
lmPredictions<- predict(lModel,data)
RMSE.lm=sqrt(mean((lmPredictions-data$cnt)^2))
MSE.lm=mean((lmPredictions-data$cnt)^2)
MAE.lm=mean(abs(lmPredictions- data$cnt))
summary(lModel)
plot(varImp(lModel))

#Regression Tree
data <- read.csv("hour.csv")
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
rpartModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, data, method="rpart", trControl=train_control,importance=T)
rpartPredictions<- predict(rpartModel,data)
RMSE.rpart=sqrt(mean((rpartPredictions-data$cnt)^2))
MSE.rpart=mean((rpartPredictions-data$cnt)^2)
MAE.rpart=mean(abs(rpartPredictions- data$cnt))
plot(varImp(rpartModel, scale=FALSE))

#svm (Linear)
data <- read.csv("hour.csv")
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
svmModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, data, method="svmLinear", trControl=train_control,importance=T)
svmPredictions<- predict(svmModel,data)
RMSE.svm=sqrt(mean((svmPredictions-data$cnt)^2))
MSE.svm=mean((svmPredictions-data$cnt)^2)
MAE.svm=mean(abs(svmPredictions- data$cnt))
plot(varImp(svmModel))

#Neurel Network
data <- read.csv("hour.csv")
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
nnetModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, data, method="nnet", trControl=train_control,importance=T)
nnetPredictions<- predict(nnetModel,data)
RMSE.nnet=sqrt(mean((nnetPredictions-data$cnt)^2))
MSE.nnet=mean((nnetPredictions-data$cnt)^2)
MAE.nnet=mean(abs(nnetPredictions- data$cnt))
plot(varImp(nnetModel))

#Random Forest this time with Kfold
data <- read.csv("hour.csv")
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
rfModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed, data, method="rf", trControl=train_control,importance=T)
rfPredictions<- predict(rfModel,data)
RMSE.rfkf=sqrt(mean((rfPredictions-data$cnt)^2))
MSE.rfkf=mean((rfPredictions-data$cnt)^2)
MAE.rfkf=mean(abs(rfPredictions- data$cnt))
plot(varImp(rfModel, scale=FALSE))

##EVALUATION
accuracy=data.frame(Method=c("random forest model","lm","regression tree","svm","nnet","random Forest this time with kfold"),
                    RMSE=c(RMSE.forest,RMSE.lm,RMSE.rpart,RMSE.svm,RMSE.nnet,RMSE.rfkf),
                    MAE=c(MAE.forest,MAE.lm,MAE.rpart,MAE.svm,MAE.nnet,MAE.rfkf),
                    MSE=c(MSE.forest,MSE.lm,MSE.rpart,MSE.svm,MSE.nnet,MSE.rfkf))

accuracy$RMSE = round(accuracy$RMSE,2)
accuracy$MAE = round(accuracy$MAE,2)
accuracy$MSE = round(accuracy$MSE,2)

all.predictions = data.frame(Actual = data$cnt,
                             RandForest = predict.rf, 
                             LinModel = lmPredictions,
                             RPart = rpartPredictions,
                             svm = svmPredictions,
                             nnet = nnetPredictions,
                             RandForestKFold = rfPredictions)
all.predictions

RandForest.c=cor(all.predictions$Actual,all.predictions$RandForest) 
LinModel.c=cor(all.predictions$Actual,all.predictions$LinModel) 
RPart=cor(all.predictions$Actual,all.predictions$RPart) 
svm.c=cor(all.predictions$Actual,all.predictions$svm) 
nnet.c=cor(all.predictions$Actual,all.predictions$nnet)
RandForestKFold.c=cor(all.predictions$Actual,all.predictions$RandForestKFold)

predict.vs.actual.correlation=data.frame(RandForest.c,
                                         LinModel.c,
                                         RPart,
                                         svm.c,
                                         nnet.c,
                                         RandForestKFold.c)
predict.vs.actual.correlation

#RandForest.c LinModel.c     RPart     svm.c nnet.c RandForestKFold.c
#1    0.9778838  0.5814736 0.6393053 0.5767638     NA         0.9838384


#Integrating clusters
#lets add them to the models with the higher accuracy
#RandForest, LinMOdel, RPart, RandForestKfold
data$WeatherCluster <- kweatherresult$cluster
data$TimeCluster <- kTimeresult$cluster

#Random forest model 2.0
set.seed(123)
tree.rf=randomForest(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed+WeatherCluster+TimeCluster, data=data,importance=TRUE,ntree=500)
plot(tree.rf)
varImpPlot(tree.rf)
predict.rf=predict(tree.rf,data)
RMSE.forest=sqrt(mean((predict.rf-data$cnt)^2))
MSE.forest=mean((predict.rf-data$cnt)^2)
MAE.forest=mean(abs(predict.rf- data$cnt))

#Linear model 2.0
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
lModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed+WeatherCluster+TimeCluster, data, method="glm", trControl=train_control,importance=T)
lmPredictions<- predict(lModel,data)
RMSE.lm=sqrt(mean((lmPredictions-data$cnt)^2))
MSE.lm=mean((lmPredictions-data$cnt)^2)
MAE.lm=mean(abs(lmPredictions- data$cnt))
summary(lModel)
plot(varImp(lModel))

#Regression tree 2.0
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
rpartModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed+WeatherCluster+TimeCluster, data, method="rpart", trControl=train_control,importance=T)
rpartPredictions<- predict(rpartModel,data)
RMSE.rpart=sqrt(mean((rpartPredictions-data$cnt)^2))
MSE.rpart=mean((rpartPredictions-data$cnt)^2)
MAE.rpart=mean(abs(rpartPredictions- data$cnt))
plot(varImp(rpartModel))

#Random Forest this time with kfold 2.0
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
rfModel<- train(cnt~+season+mnth+hr+holiday+workingday+weathersit+temp+hum+windspeed+WeatherCluster+TimeCluster, data, method="rf", trControl=train_control,importance=T)
rfPredictions<- predict(rfModel,data)
RMSE.rfkf=sqrt(mean((rfPredictions-data$cnt)^2))
MSE.rfkf=mean((rfPredictions-data$cnt)^2)
MAE.rfkf=mean(abs(rfPredictions- data$cnt))
plot(varImp(rfModel))

#Append it to the accuracy dataset
accuracy <- rbind(accuracy,data.frame(Method="RandomForest2.0",RMSE=RMSE.forest,MAE=MAE.forest,MSE=MSE.forest))
accuracy <- rbind(accuracy,data.frame(Method="LinearModel2.0",RMSE=RMSE.lm,MAE=MAE.lm,MSE=MSE.lm))
accuracy <- rbind(accuracy,data.frame(Method="RegressionTree2.0",RMSE=RMSE.rpart,MAE=MAE.rpart,MSE=MSE.rpart))
accuracy <- rbind(accuracy,data.frame(Method="RandomForestWithKfold2.0",RMSE=RMSE.rfkf,MAE=MAE.rfkf,MSE=MSE.rfkf))

accuracy$RMSE = round(accuracy$RMSE,2)
accuracy$MAE = round(accuracy$MAE,2)
accuracy$MSE = round(accuracy$MSE,2)

all.predictions$RandomForest2.0 <- predict.rf
all.predictions$LinearModel2.0 <- lmPredictions
all.predictions$RegressionTree2.0 <- rpartPredictions
all.predictions$RandomForestWithKfold2.0 <- rfPredictions

RandForest.c2=cor(all.predictions$Actual,all.predictions$RandomForest2.0) 
LinModel.c2=cor(all.predictions$Actual,all.predictions$LinearModel2.0) 
RPart.c2=cor(all.predictions$Actual,all.predictions$RegressionTree2.0) 
RandForestKFold.c2=cor(all.predictions$Actual,all.predictions$RandomForestWithKfold2.0)

predict.vs.actual.correlation <- cbind(predict.vs.actual.correlation,RandForest.c2)
predict.vs.actual.correlation <- cbind(predict.vs.actual.correlation,LinModel.c2)
predict.vs.actual.correlation <- cbind(predict.vs.actual.correlation,RPart.c2)
predict.vs.actual.correlation <- cbind(predict.vs.actual.correlation,RandForestKFold.c2)

predict.vs.actual.correlation
#RandForest.c LinModel.c     RPart     svm.c nnet.c RandForestKFold.c RandForest.c2 LinModel.c2  RPart.c2 RandForestKFold.c2
#1    0.9778838  0.5814736 0.6393053 0.5767638     NA         0.9838735     0.9715803   0.5845314 0.6393053          0.9836316

#What if we tried to train with only the clusters?

#Random Forest this time with kfold 2.5
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
rfModel2.5<- train(cnt~+WeatherCluster+TimeCluster, data, method="rf", trControl=train_control)
rfPredictions2.5<- predict(rfModel2.5,data)
RMSE.rfkf2.5=sqrt(mean((rfPredictions2.5-data$cnt)^2))
MSE.rfkf2.5=mean((rfPredictions2.5-data$cnt)^2)
MAE.rfkf2.5=mean(abs(rfPredictions2.5- data$cnt))

accuracy <- rbind(accuracy,data.frame(Method="RandomForestWithKfold2.5",RMSE=RMSE.rfkf2.5,MAE=MAE.rfkf2.5,MSE=MSE.rfkf2.5))
accuracy$RMSE = round(accuracy$RMSE,2)
accuracy$MAE = round(accuracy$MAE,2)
accuracy$MSE = round(accuracy$MSE,2)
all.predictions$RandomForestWithKfold2.5 <- rfPredictions2.5
RandForestKFold.c2.5=cor(all.predictions$Actual,all.predictions$RandomForestWithKfold2.5)
predict.vs.actual.correlation <- cbind(predict.vs.actual.correlation,RandForestKFold.c2.5)
# only 0.5056317 accurate but it shows we can reduce the number of features by clustering. Therefore reducing time needed to train the data

#Determine how many clusters we should us for shiny app
model <- cascadeKM(data$cnt, 1, 10, iter = 10)
model$results[2,]
which.max(model$results[2,])
#recommends 10 groups
#plot a graph
plot(kmeans(data$cnt, 10)$cluster,data$cnt)

##MODEL DEPLOYMENT
#Export the most efficient model for the app
saveRDS(rfModel, "model2.0.rds")