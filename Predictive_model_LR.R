###Install Libraries
install.packages("stringr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("MASS")
install.packages("car")

### Load the Libraries
library(stringr)
library(tidyr)
library(dplyr)
library(MASS)
library(car)

###Reading csv file
cars<- read.csv("CarPrice_Assignment.csv")
View(cars)

### Data Dictionary
# Car_ID			        Unique id of each observation (Interger)		
# Symboling 			    Its assigned insurance risk rating
# carCompany			    Name of car company (Categorical)		
# fueltype			      Car fuel type i.e gas or diesel (Categorical)		
# aspiration			    Aspiration used in a car (Categorical)		
# doornumber			    Number of doors in a car (Categorical)		
# carbody			        body of car (Categorical)		
# drivewheel			    type of drive wheel (Categorical)		
# enginelocation	    Location of car engine (Categorical)		
# wheelbase			      Weelbase of car (Numeric)		
# carlength			      Length of car (Numeric)		
# carwidth			      Width of car (Numeric)		
# carheight			      height of car (Numeric)		
# curbweight			    The weight of a car without occupants or baggage. (Numeric)		
# enginetype			    Type of engine. (Categorical)		
# cylindernumber	    cylinder placed in the car (Categorical)		
# enginesize			    Size of car (Numeric)		
# fuelsystem			    Fuel system of car (Categorical)		
# boreratio			      Boreratio of car (Numeric)		
# stroke			        Stroke or volume inside the engine (Numeric)		
# compressionratio		compression ratio of car (Numeric)		
# horsepower			    Horsepower (Numeric)		
# peakrpm			        car peak rpm (Numeric)		
# citympg			        Mileage in city (Numeric)		
# highwaympg			    Mileage on highway (Numeric)		
# price		            Price of car (Numeric)

###Structure of cars dataframe
str(cars)
cars$symboling<-as.factor(cars$symboling)


###checking duplicate records
sum(duplicated(cars))

###checking missing values
sum(is.na(cars))

###Removing outliers
quantile(cars$enginesize,seq(0,1,0.01))
boxplot(cars$enginesize)
cars$enginesize[cars$enginesize>201.20]<-201.20

quantile(cars$citympg,seq(0,1,0.01))
boxplot(cars$citympg)
cars$citympg[cars$citympg>44.72]<-44.72


###removing carID - as it is not helpful in predicting the price
cars<-cars[,-1]
### removing enginelocation - as it has two levels "front" & "rear" : and 99% of the values are "front" 
### Because of which the variable enginelocation is not a significant variable for building the model
cars<-cars[,-8]


###splitting CarName
cars$CarName<- str_replace_all(cars$CarName," ","-")
cars<-separate(cars,CarName,into = c("CarName","model"),sep = "-")
cars<-cars[,-3]
cars$CarName<-as.factor(cars$CarName)


###Cleaning the car names
cars$CarName[cars$CarName=="vw"]<-"volkswagen"
cars$CarName[cars$CarName=="vokswagen"]<-"volkswagen"
cars$CarName[cars$CarName=="maxda"]<-"mazda"
cars$CarName[cars$CarName=="Nissan"]<-"nissan"
cars$CarName[cars$CarName=="porcshce"]<-"porsche"
cars$CarName[cars$CarName=="toyouta"]<-"toyota"


###Levels in each variable
for(i in 1:ncol(cars)){
  print(paste(colnames(cars[i]),"-",length(levels(cars[,i]))))
}

# [1] "symboling - 6"
# [1] "CarName - 22"
# [1] "fueltype - 2"
# [1] "aspiration - 2"
# [1] "doornumber - 2"
# [1] "carbody - 5"
# [1] "drivewheel - 3"
# [1] "enginelocation - 2"
# [1] "wheelbase - 0"
# [1] "carlength - 0"
# [1] "carwidth - 0"
# [1] "carheight - 0"
# [1] "curbweight - 0"
# [1] "enginetype - 7"
# [1] "cylindernumber - 7"
# [1] "enginesize - 0"
# [1] "fuelsystem - 8"
# [1] "boreratio - 0"
# [1] "stroke - 0"
# [1] "compressionratio - 0"
# [1] "horsepower - 0"
# [1] "peakrpm - 0"
# [1] "citympg - 0"
# [1] "highwaympg - 0"
# [1] "price - 0"

###Creating dummy variables for variables with 2 levels
#fueltype- gas=0 ;diesel=1
#aspiration- std=1;turbo=0
#doornumber- two=0;four=1

#for all the 2 level variables
for(i in 1:ncol(cars)){
  if(length(levels(cars[,i]))==2){
    levels(cars[,i])<-c(1,0)
    cars[,i] <- as.numeric(levels(cars[,i]))[cars[,i]]
  }
}

###Creating dummy variables for variables with levels > 2
#creating dummy variables for CarName
dummy_1 <- data.frame(model.matrix( ~CarName, data = cars))
dummy_1 <- dummy_1[,-1]
cars <- cbind(cars[,-2], dummy_1)


#creating dummy variables for carbody
dummy_2 <- data.frame(model.matrix( ~carbody, data = cars))
dummy_2 <- dummy_2[,-1]
cars <- cbind(cars[,-5], dummy_2)

#creating dummy variables for drivewheel
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = cars))
dummy_3 <- dummy_3[,-1]
cars <- cbind(cars[,-5], dummy_3)

#creating dummy variables for enginetype
dummy_4 <- data.frame(model.matrix( ~enginetype, data = cars))
dummy_4 <- dummy_4[,-1]
cars <- cbind(cars[,-10], dummy_4)

#creating dummy variables for cylindernumber
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = cars))
dummy_5 <- dummy_5[,-1]
cars <- cbind(cars[,-10], dummy_5)

#creating dummy variables for fuelsystem
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = cars))
dummy_6 <- dummy_6[,-1]
cars <- cbind(cars[,-11], dummy_6)

#creating dummy variables for symboling
dummy_7 <- data.frame(model.matrix( ~symboling, data = cars))
dummy_7 <- dummy_7[,-1]
cars <- cbind(cars[,-1], dummy_7)


### separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(cars), 0.7*nrow(cars))
train = cars[trainindices,]
test = cars[-trainindices,]


# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)# R^2-0.9635

#using stepAIC to remove insignificant variables
step <- stepAIC(model_1, direction="both")
step

#building model_2 after removing relatively insignificant variables
model_2<-lm(formula = price ~ aspiration + carlength + carwidth + carheight + 
              curbweight + boreratio + stroke + horsepower + CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_2) # R^2-0.9687
#checking for correlated variables
vif(model_2)

#removing horsepower
model_3<-lm(formula = price ~ aspiration + carlength + carwidth + carheight + 
              curbweight + boreratio + stroke +  CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_3) # R^2-0.9681
#checking for correlated variables
vif(model_3)

#removing carlength
model_4<-lm(formula = price ~ aspiration + carwidth + carheight + 
              curbweight + boreratio + stroke +  CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_4) #R^2-0.9675
#checking for correlated variables
vif(model_4)

#removing carbodysedan
model_5<-lm(formula = price ~ aspiration + carwidth + carheight + 
              curbweight + boreratio + stroke +  CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback +  carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_5)# R^2-0.9667 
#checking for correlated variables
vif(model_5)

#removing drivewheelrwd
model_6<-lm(formula = price ~ aspiration + carwidth + carheight + 
              curbweight + boreratio + stroke +  CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback +  carbodywagon + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_6)# R^2-0.9667
#checking for correlated variables
vif(model_6)

#removing carheight
model_7<-lm(formula = price ~ aspiration + carwidth +  
              curbweight + boreratio + stroke +  CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback +  carbodywagon + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_7)#R^2- 0.9665
#checking for correlated variables
vif(model_7)

#removing CarNamesaab
model_8<-lm(formula = price ~ aspiration + carwidth +  
              curbweight + boreratio + stroke +  CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault +  CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback +  carbodywagon + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_8)#R^2- 0.9659
#checking for correlated variables
vif(model_8)

#removing boreratio
model_9<-lm(formula = price ~ aspiration + carwidth +  
              curbweight + stroke +  CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault +  CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback +  carbodywagon + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_9)#R^2- 0.9653
#checking for correlated variables
vif(model_9)

#removing stroke
model_10<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
               carbodyhatchback +  carbodywagon + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_10)#R^2- 0.9652
#checking for correlated variables
vif(model_10)

#removing CarNamevolvo
model_11<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + carbodyhardtop + 
               carbodyhatchback +  carbodywagon + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_11)#R^2- 0.9653
#checking for correlated variables
vif(model_11)

#removing carbodyhardtop - as it is the least significant
model_13<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               carbodyhatchback +  carbodywagon + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_13)#R^2- 0.9656
#checking for correlated variables
vif(model_13)


#removing carbodyhatchback- as it is the least significant
model_14<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               carbodywagon + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + symboling1, data = train)
summary(model_14)#R^2- 0.9658
#checking for correlated variables
vif(model_14)


# removing symboling1  - as it is the least significant
model_15<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               carbodywagon + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_15)# R^2-0.9658
#checking for correlated variables
vif(model_15)


#removing CarNamemercury - as it is the least significant
model_16<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemazda +
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               carbodywagon + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_16)#R^2- 0.9656
#checking for correlated variables
vif(model_16)


#removing CarNameisuzu - as it is the least significant
model_17<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNamejaguar + CarNamemazda +
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               carbodywagon + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_17)# R^2-0.9649
#checking for correlated variables
vif(model_17)

#removing carbodywagon - as it is the least significant
model_18<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
               CarNamejaguar + CarNamemazda +
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_18)#R^2- 0.9642
#checking for correlated variables
vif(model_18)

#removing CarNamehonda - as it is the least significant
model_19<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + 
               CarNamejaguar + CarNamemazda +
               CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_19)# R^2-0.962
#checking for correlated variables
vif(model_19)

#removing CarNamenissan - as it is the least significant
model_20<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + CarNamedodge + 
               CarNamejaguar + CarNamemazda +
               CarNamemitsubishi +  CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_20)#R^2- 0.9609
#checking for correlated variables
vif(model_20)

#removing CarNamedodge - as it is the least significant
model_21<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNamemazda +
               CarNamemitsubishi +  CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_21)#R^2- 0.9592
#checking for correlated variables
vif(model_21)

#removing CarNameplymouth - as it is the least significant
model_22<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNamemazda +
               CarNamemitsubishi +  CarNamepeugeot +
               CarNameporsche + CarNamerenault +  CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_22)#R^2- 0.9579
#checking for correlated variables
vif(model_22)

#removing CarNamevolkswagen - as it is the least significant
model_23<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNamemazda +
               CarNamemitsubishi +  CarNamepeugeot +
               CarNameporsche + CarNamerenault +  CarNamesubaru +  CarNametoyota + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_23)#R^2- 0.9566
#checking for correlated variables
vif(model_23)

#removing CarNamemazda- as it is the least significant
model_24<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet +  CarNamejaguar +
               CarNamemitsubishi +  CarNamepeugeot +
               CarNameporsche + CarNamerenault +  CarNamesubaru +  CarNametoyota + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_24)#R^2- 0.9558
#checking for correlated variables
vif(model_24)

#removing CarNamesubaru - as it is the least significant
model_25<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet +  CarNamejaguar +
               CarNamemitsubishi +  CarNamepeugeot +
               CarNameporsche + CarNamerenault +  CarNametoyota + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_25)#R^2- 0.95548
#checking for correlated variables
vif(model_25)

#removing CarNamerenault - as it is the least significant
model_26<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet +  CarNamejaguar +
               CarNamemitsubishi +  CarNamepeugeot +
               CarNameporsche +   CarNametoyota + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_26)#R^2- 0.9536
#checking for correlated variables
vif(model_26)

#removing CarNametoyota - as it is the least significant
model_27<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet +  CarNamejaguar +
               CarNamemitsubishi +  CarNamepeugeot + CarNameporsche + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_27)#R^2- 0.9522
#checking for correlated variables
vif(model_27)

#removing CarNamemitsubishi- as it is the least significant
model_28<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamebuick + CarNamechevrolet +  CarNamejaguar +
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_28)# R^2-0.9506
#checking for correlated variables
vif(model_28)

#removing CarNamebuick - as it is the least significant
model_29<-lm(formula = price ~ aspiration + carwidth +  
               curbweight +  CarNamebmw + 
               CarNamechevrolet +  CarNamejaguar +
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_29)# R^2-0.9484
#checking for correlated variables
vif(model_29)

#removing carbodysedan- as it is the least significant
model_30<-lm(formula = price ~ carwidth + curbweight +  CarNamebmw + 
               CarNamechevrolet +  CarNamejaguar +
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix , data = train)
summary(model_30)# R^2-0.9443
#checking for correlated variables
vif(model_30)


#removing cylindernumberfour 
model_31<-lm(formula = price ~ carwidth + curbweight +  CarNamebmw + 
               CarNamechevrolet +  CarNamejaguar +
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv + enginetyperotor + cylindernumberfive + 
               cylindernumbersix , data = train)
summary(model_31)#R^2- 0.8954
#checking for correlated variables
vif(model_31)


#remove enginetyperotor - as it is the least significant
model_32<-lm(formula = price ~ carwidth + curbweight +  CarNamebmw + 
               CarNamechevrolet +  CarNamejaguar +
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv + cylindernumberfive + 
               cylindernumbersix , data = train)
summary(model_32)# R^2-0.896 
#checking for correlated variables
vif(model_32)

#removing cylindernumberfive - as it is the least significant
model_33<-lm(formula = price ~ carwidth + curbweight +  CarNamebmw + 
               CarNamechevrolet +  CarNamejaguar +
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv + cylindernumbersix , data = train)
summary(model_33)# R^2-0.8963
#checking for correlated variables
vif(model_33)

#removing cylindernumbersix - as it is the least significant
model_34<-lm(formula = price ~ carwidth + curbweight +  CarNamebmw + 
               CarNamechevrolet +  CarNamejaguar +
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv  , data = train)
summary(model_34)#R^2- 0.8955
#checking for correlated variables
vif(model_34)

#removing CarNamejaguar - as it is the least significant
model_35<-lm(formula = price ~ carwidth + curbweight +  CarNamebmw + 
               CarNamechevrolet + CarNamepeugeot + CarNameporsche + 
               enginetypedohcv  , data = train)
summary(model_35)#R^2- 0.8933
#checking for correlated variables
vif(model_35)

#removing CarNamechevrolet - as it is the least significant
model_36<-lm(formula = price ~ carwidth + curbweight +  CarNamebmw + 
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv  , data = train)
summary(model_36)#R^2- 0.8861
#checking for correlated variables
vif(model_36)

### As all the variables are significant, therefore removing the variable with high multicolinearity
### Creating two different models by removing one variable with high multicolinearity

## Creating model by removing carwidth variable
model_37<-lm(formula = price ~ curbweight +  CarNamebmw + 
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv  , data = train)
summary(model_37)#R^2- 0.8647
#checking for correlated variables
vif(model_37)

#removing enginetypedohcv 
model_38<-lm(formula = price ~ curbweight +  CarNamebmw + 
               CarNamepeugeot + CarNameporsche , data = train)
summary(model_38)#R^2- 0.8561 
#checking for correlated variables
vif(model_38)


# Predict the car prices in the testing dataset
Predict_1 <- predict(model_38,test[,-18])
test$test_price <- Predict_1
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2
# check R-squared
rsquared #0.8045673



## Creating model by removing curbweight variable
model_39<-lm(formula = price ~ carwidth +  CarNamebmw + 
               CarNamepeugeot + CarNameporsche + 
               enginetypedohcv  , data = train)
summary(model_39)#R^2-0.8211
#checking for correlated variables
vif(model_39)

# Predict the car prices in the testing dataset
Predict_2 <- predict(model_39,test[,-18])
test$test_price_2 <- Predict_2
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price_2)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price_2)^2
# check R-squared
rsquared #0.6493395


### After removing the curbweight and building a model. The R^2 is reduced by 6 points and there is a high difference between
### predicted model and built model.
### Therefore, selecting the final model (model_38) as the difference in R^2 of the predicted model and built model which is less (5 points difference).


####################################################################################################################################
############################################## Regression Model Summary ############################################################

#The R^2 of the final model (model_38) : 85.61% 
#The R^2 after prediction using test data : 80.46%

### Key Performance Metrics :
    #1. Curbweight
    #2. CarNamebmw
    #3. CarNamepeugeot
    #4. CarNameporsche

###Curbweight:
# Curbweight can be a major factor for pricing a car in the US market, as the level of curbweight decides the efficiency of the car.
# Higher curbweights reduces the effeciency of the car and more price should be paid inorder to increase the efficiency of the car.
# Therefore, standardising the curbwieght of the cars can play a major role in pricing the car.

###Car Companies:
# As we all know companies brand's are highly realted to pricing of a car. As the chinese automobile company aspires to enter US market,
# Positioning and branding of a car as per the market segment and understanding the brand marketing plays an important role in pricing of the car.

####################################################################################################################################
####################################################################################################################################
