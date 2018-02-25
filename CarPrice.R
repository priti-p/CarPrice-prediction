install.packages("MASS")
install.packages("car")

# Load essential libraries
library(ggplot2)
library(lubridate)
library(MASS)
library(car)


# load the media company data
car_info <- read.csv("C:/Users/Priti_Parate/Documents/upgrad/car_price/CarPrice_Assignment.csv")#, stringsAsFactors = F)
str(car_info)

##check any duplicates
#----------------------
unique(car_info) # no dupicates

## check any missing values
#----------------------
sum(is.na(car_info)) #no missing values

##check for outliers
#----------------------
## removing unwanted columns
boxplot(car_info[,-(c(1:9, 14:16, 18, 23, 26))])

# horsepower
quantile(car_info$horsepower, seq(0,1,0.01))
#there is sudden jump from 207 hence removeing
car_info$horsepower[which(car_info$horsepower>207)] <- 207

#enginesize
quantile(car_info$enginesize, seq(0,1,0.01))
#there is sudden jump from 207 hence removeing
car_info$enginesize[which(car_info$enginesize>209)] <- 209

boxplot(car_info[,-(c(1:9, 14:16, 17:18, 22:23, 26))])

#compressionratio
quantile(car_info$compressionratio, seq(0,1,0.01))
#there is sudden jump from 10.9400 hence removeing
car_info$compressionratio[which(car_info$compressionratio>10.9400)] <- 10.9400

#highwaympg
boxplot(car_info$highwaympg)
quantile(car_info$highwaympg, seq(0,1,0.01))
#there is sudden jump from 10.9400 hence removeing
car_info$highwaympg[which(car_info$highwaympg>49.88)] <- 49.88

#create dummy variables

# create_dummy <- function(df, col_name, col_no)
# {
#   dummy <- model.matrix(~col_name - 1, data=df)
#   dummy <- dummy[,-1]
#   df <- cbind(df[,-col_no], dummy)
#   
#   return (df)
# }
# 
# car_info_1 <- car_info

#1 fuelsystem
dummy_fuelsystem <- model.matrix(~car_info$fuelsystem - 1, data=car_info)
dummy_fuelsystem <- dummy_fuelsystem[,-1]
car_info_1 <- cbind(car_info[,-18], dummy_fuelsystem)

#2 cylindernumber
dummy_cylindernumber <- model.matrix(~car_info_1$cylindernumber - 1, data=car_info_1)
dummy_cylindernumber <- dummy_cylindernumber[,-1]
car_info_1 <- cbind(car_info_1[,-16], dummy_cylindernumber)


#3 enginetype
dummy_enginetype <- model.matrix(~car_info_1$enginetype - 1, data=car_info_1)
dummy_enginetype <- dummy_enginetype[,-1]
car_info_1 <- cbind(car_info_1[,-15], dummy_enginetype)


#4 enginelocation
dummy_enginelocation <- model.matrix(~car_info_1$enginelocation - 1, data=car_info_1)
dummy_enginelocation <- dummy_enginelocation[,-1]
car_info_1 <- cbind(car_info_1[,-9], dummy_enginelocation)

#5 drivewheel
dummy_drivewheel <- model.matrix(~car_info_1$drivewheel - 1, data=car_info_1)
dummy_drivewheel <- dummy_drivewheel[,-1]
car_info_1 <- cbind(car_info_1[,-8], dummy_drivewheel)

#6 carbody
dummy_carbody <- model.matrix(~car_info_1$carbody - 1, data=car_info_1)
dummy_carbody <- dummy_carbody[,-1]
car_info_1 <- cbind(car_info_1[,-7], dummy_carbody)

#7 doornumber
dummy_doornumber <- model.matrix(~car_info_1$doornumber - 1, data=car_info_1)
dummy_doornumber <- dummy_doornumber[,-1]
car_info_1 <- cbind(car_info_1[,-6], dummy_doornumber)

#8 aspiration
dummy_aspiration <- model.matrix(~car_info_1$aspiration - 1, data=car_info_1)
dummy_aspiration <- dummy_aspiration[,-1]
car_info_1 <- cbind(car_info_1[,-5], dummy_aspiration)

#9 fueltype
dummy_fueltype <- model.matrix(~car_info_1$fueltype - 1, data=car_info_1)
dummy_fueltype <- dummy_fueltype[,-1]
car_info_1 <- cbind(car_info_1[,-4], dummy_fueltype)

#symboling
car_info_1$symboling <- as.factor(car_info_1$symboling)
levels(car_info_1$symboling)[levels(car_info_1$symboling) == -3] <- "highly_safe"
levels(car_info_1$symboling)[levels(car_info_1$symboling) == -2] <- "medium_safe"
levels(car_info_1$symboling)[levels(car_info_1$symboling) == -1] <- "low_safe"
levels(car_info_1$symboling)[levels(car_info_1$symboling) == 0] <- "neutral"
levels(car_info_1$symboling)[levels(car_info_1$symboling) == 1] <- "low_risky"
levels(car_info_1$symboling)[levels(car_info_1$symboling) == 2] <- "medium_risky"
levels(car_info_1$symboling)[levels(car_info_1$symboling) == 3] <- "highly_risky"
dummy_symboling <- model.matrix(~car_info_1$symboling - 1, data=car_info_1)
dummy_symboling <- dummy_symboling[,-1]
car_info_1 <- cbind(car_info_1[,-2], dummy_symboling)


###as need to consider only company name as the independent variable for the model building. 
CarCompany <- sapply(strsplit(as.character(car_info$CarName),' '), "[", 1)
car_info_1 <- cbind(car_info_1[,-3], CarCompany)
rm(temp)
car_info_1$CarName<-NULL

#clearing carcompany values properly
levels(car_info_1$CarCompany)[levels(car_info_1$CarCompany)=="maxda"]    <- "mazda"
levels(car_info_1$CarCompany)[levels(car_info_1$CarCompany)=="Nissan"]   <- "nissan"
levels(car_info_1$CarCompany)[levels(car_info_1$CarCompany)=="porcshce"] <- "porsche"
levels(car_info_1$CarCompany)[levels(car_info_1$CarCompany)=="toyouta"]  <- "toyota"
levels(car_info_1$CarCompany)[levels(car_info_1$CarCompany)=="vokswagen"]<- "volkswagen"
levels(car_info_1$CarCompany)[levels(car_info_1$CarCompany)=="vw"]    <- "volkswagen"
dummy_CarCompany <- model.matrix(~car_info_1$CarCompany - 1, data=car_info_1)
dummy_CarCompany <- dummy_CarCompany[,-1]
car_info_1 <- cbind(car_info_1[,-49], dummy_CarCompany)

write.csv(car_info_1, "carInfo.csv")
#Building the model
#divide the data into training and test data set
set.seed(100)

train_ind<-sample(1:nrow(car_info_1),0.7*nrow(car_info_1))

train_data<-car_info_1[train_ind,]
View(train_data)
test_data <-car_info_1[-train_ind,]

## not dividing now as data is very low
model_1<-lm(price~.,data = train_data)
summary(model_1)
#Multiple R-squared:  0.9797,	Adjusted R-squared:  0.9652

stepAIC(model_1,direction = "both")

model_2 <- lm(formula = price ~ car_ID + carwidth + carheight + curbweight + 
                enginesize + boreratio + stroke + compressionratio + peakrpm + 
                citympg + `car_info_1$cylindernumberfive` + `car_info_1$cylindernumberfour` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_2)
vif(model_2)
#Multiple R-squared:  0.9784,	Adjusted R-squared:   0.97

#VIF of car_ID is very high removing it...
model_3 <- lm(formula = price ~ carwidth + carheight + curbweight + 
                enginesize + boreratio + stroke + compressionratio + peakrpm + 
                citympg + `car_info_1$cylindernumberfive` + `car_info_1$cylindernumberfour` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)

summary(model_3)
vif(model_3)
#Multiple R-squared:  0.9743,	Adjusted R-squared:  0.9646 

#vif for curbweight is high
### plot for curbweight vs carlength and carwidth
ggplot(car_info_1, aes(curbweight, carlength)) + geom_line(aes(colour = "blue" )) + 
  geom_line(aes(x=curbweight, y=carwidth, colour="red"))

## model 4 - after removing curbweight
model_4 <- lm(formula = price ~ carwidth + carheight + 
                enginesize + boreratio + stroke + compressionratio + peakrpm + 
                citympg + `car_info_1$cylindernumberfive` + `car_info_1$cylindernumberfour` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_4)
vif(model_4)
#Multiple R-squared:  0.9698,	Adjusted R-squared:  0.9588 

## vif for enginesize is high
model_5 <- lm(formula = price ~ carwidth + carheight + 
                boreratio + stroke + compressionratio + peakrpm + 
                citympg + `car_info_1$cylindernumberfive` + `car_info_1$cylindernumberfour` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_5)
vif(model_5)
#Multiple R-squared:  0.9659,	Adjusted R-squared:  0.9539

#pvalue for cytympg is high
model_6 <- lm(formula = price ~ carwidth + carheight + 
                boreratio + stroke + compressionratio + peakrpm + 
                `car_info_1$cylindernumberfive` + `car_info_1$cylindernumberfour` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_6)
vif(model_6)
#Multiple R-squared:  0.9658,	Adjusted R-squared:  0.9542

#vif for carwidth is high
model_7 <- lm(formula = price ~ carheight + 
                boreratio + stroke + compressionratio + peakrpm + 
                `car_info_1$cylindernumberfive` + `car_info_1$cylindernumberfour` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_7)
vif(model_7)
#Multiple R-squared:  0.9495,	Adjusted R-squared:  0.9329 

#p-value for peakrpm is high
model_8 <- lm(formula = price ~ carheight + 
                boreratio + stroke + compressionratio + 
                `car_info_1$cylindernumberfive` + `car_info_1$cylindernumberfour` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_8)
vif(model_8)
#Multiple R-squared:  0.9495,	Adjusted R-squared:  0.9335

#vif for cylindernumberfour is >12
model_9 <- lm(formula = price ~ carheight + 
                boreratio + stroke + compressionratio + 
                `car_info_1$cylindernumberfive` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$symbolinghighly_risky` + `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_9)
vif(model_9)
#Multiple R-squared:  0.9391,	Adjusted R-squared:  0.9206

#symbolinghighly_risky` p-value 0.971042
model_10 <- lm(formula = price ~ carheight + 
                boreratio + stroke + compressionratio + 
                `car_info_1$cylindernumberfive` + 
                `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                `car_info_1$symbolinglow_risky` + `car_info_1$symbolingmedium_risky` + 
                `car_info_1$CarCompanyaudi` + 
                `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
              data = train_data)
summary(model_10)
vif(model_10)
#Multiple R-squared:  0.9391,	Adjusted R-squared:  0.9213

#symbolinglow_risky`  pvalue= 0.89536
model_11 <- lm(formula = price ~ carheight + 
                 boreratio + stroke + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                 `car_info_1$carbodywagon` + dummy_aspiration + `car_info_1$symbolingneutral` + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_11)
vif(model_11)
#Multiple R-squared:  0.9391,	Adjusted R-squared:  0.922

#carbodywagon pvalue = 0.806862
model_12 <- lm(formula = price ~ carheight + 
                 boreratio + stroke + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + `car_info_1$drivewheelrwd` + 
                 dummy_aspiration + `car_info_1$symbolingneutral` + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_12)
vif(model_12)
#Multiple R-squared:  0.939,	Adjusted R-squared:  0.9227

#vif for drivewheelrwd is high
model_13 <- lm(formula = price ~ carheight + 
                 boreratio + stroke + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + 
                 dummy_aspiration + `car_info_1$symbolingneutral` + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_13)
vif(model_13)
#Multiple R-squared:  0.9389,	Adjusted R-squared:  0.9232

#pvalue for stroke is high
model_14 <- lm(formula = price ~ carheight + 
                 boreratio + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + `car_info_1$cylindernumberthree` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + 
                 dummy_aspiration + `car_info_1$symbolingneutral` + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_14)
vif(model_14)
#Multiple R-squared:  0.9387,	Adjusted R-squared:  0.9236

#pvalue for cylindernumberthree is high
model_15 <- lm(formula = price ~ carheight + 
                 boreratio + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + 
                 dummy_aspiration + `car_info_1$symbolingneutral` + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanysaab` + `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_15)
vif(model_15)
#Multiple R-squared:  0.9385,	Adjusted R-squared:  0.924

#vif and p-value for CarCompanysaab is high

model_16 <- lm(formula = price ~ carheight + 
                 boreratio + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + 
                 dummy_aspiration + `car_info_1$symbolingneutral` + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_16)
vif(model_16)
#Multiple R-squared:  0.9383,	Adjusted R-squared:  0.9244

#carheight pvalue is high
model_17 <- lm(formula = price ~ 
                 boreratio + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + 
                 dummy_aspiration + `car_info_1$symbolingneutral` + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_17)
vif(model_17)
#Multiple R-squared:  0.9376,	Adjusted R-squared:  0.9243

#symbolingneutral pvalue is high
model_18 <- lm(formula = price ~ 
                 boreratio + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + 
                 dummy_aspiration + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_18)
vif(model_18)
#Multiple R-squared:  0.9366,	Adjusted R-squared:  0.9237

#pvalue for aspiration is high
model_19 <- lm(formula = price ~ 
                 boreratio + compressionratio + 
                 `car_info_1$cylindernumberfive` + 
                 `car_info_1$cylindernumbersix` + 
                 `car_info_1$enginetypedohcv` + `car_info_1$enginetypel` + 
                 `car_info_1$enginetypeohcf` + dummy_enginelocation + 
                 `car_info_1$symbolingmedium_risky` + 
                 `car_info_1$CarCompanyaudi` + 
                 `car_info_1$CarCompanybuick` + `car_info_1$CarCompanydodge` + 
                 `car_info_1$CarCompanyhonda` + `car_info_1$CarCompanyisuzu` + 
                 `car_info_1$CarCompanymazda` + `car_info_1$CarCompanymercury` + 
                 `car_info_1$CarCompanymitsubishi` + `car_info_1$CarCompanynissan` + 
                 `car_info_1$CarCompanyplymouth` + `car_info_1$CarCompanyrenault` + 
                 `car_info_1$CarCompanytoyota` + 
                 `car_info_1$CarCompanyvolkswagen` + `car_info_1$CarCompanyvolvo`, 
               data = train_data)
summary(model_19)
vif(model_19)
#Multiple R-squared:  0.9352,	Adjusted R-squared:  0.9227

Predict_price<-predict(model_19,test_data)
test_data$test_price<-Predict_price

cor_out <- cor(test)
