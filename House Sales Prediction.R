housedata = read.table("C:/Users/prathyusha/OneDrive/Desktop/kc_house_data.csv", header=TRUE, sep=",")
head(housedata)
str(housedata)

#checking if any missing values - no missing values found so data preprocessing not done
colSums(is.na(housedata))
sum(is.na(housedata))

#loading required packages
library(corrplot)
library(caret)
library(ggplot2)
library(lubridate)
library(minqa)
library(car)
library(Metrics)

#converting date to numerical variable
housedata$date<-(substr(housedata$date, 1, 8))
housedata$date<- ymd(housedata$date)
housedata$date<-as.numeric(as.Date(housedata$date))

head(housedata$date)
mydata = housedata[,-1]
head(mydata)

#finding correlation between the variables
par(mfrow=c(1,1))
corhouse = cor(housedata)
corrplot(corhouse, type="full", method = "circle", main="Correlation")
corrplot(corhouse, method = "color", outline = T, cl.pos = 'n', rect.col = "blue",  tl.col = "blue", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green","white","purple"))(100))
print(round(corhouse, digits=2))

print(subset(housedata, housedata$bedrooms > 10))
bed3 <- subset(housedata,housedata$bedrooms == 3)
print(tapply(bed3$sqft_living, bed3$bedrooms, mean))
housedata[15871,4] = 3

# converting to factors as below variables are categorical variables
housedata$zipcode <- as.factor(housedata$zipcode)
housedata$grade <- as.factor(housedata$grade)
housedata$waterfront <- as.factor(housedata$waterfront)
housedata$floors <- as.factor(housedata$floors)
housedata$bedrooms <- as.factor(housedata$bedrooms)
housedata$view = as.factor(housedata$view)
housedata$condition = as.factor(housedata$condition)

#divide data into training and testing
housedata=housedata[sample(nrow(housedata)),]
select.data= sample (1:nrow(housedata), 0.8*nrow(housedata))
train= housedata[select.data,]
test= housedata[-select.data,]

#Model 1: Elimination by p value and vif
#VIF is calculated for each model after building the model and variables with value > 4 show multicollinearity and must be removed from model
full=lm(log(price)~date+bedrooms+bathrooms+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+long+sqft_living15+sqft_lot15, data=train)
summary(full)
#predictions of full model
y1=predict.glm(full,test)
y1=exp(y1)
y=test$price
rmse_1 = sqrt((y-y1)%*%(y-y1))/nrow(test)
rmse_1 # rmse is 3099.507

vif(full)
par(mfrow = c(2, 2))

plot(full) # residual analysis Normal QQ plot shows that most of the points lie on the line so residuals follw normal distribution

#to compare the predictions by full model with actual values
head(y1)
head(y)

#Model2 - Forward elimination
base=lm(log(price)~date,data=train)
model2=step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F)
model2 = lm(formula = log(price) ~ date + grade +   view + 
              waterfront + condition + yr_renovated + floors + sqft_above + 
              bedrooms + bathrooms + sqft_lot + sqft_living15 + long + 
              sqft_lot15 + yr_built, data = train)

summary(model2)
par(mfrow = c(2, 2))
plot(model2)

vif(model2)

y2=predict.glm(model2,test)
y=test$price
y2=exp(y2)
rmse_2 = sqrt((y-y2)%*%(y-y2))/nrow(test)
rmse_2 # same rmse as full model

head(y2)
head(y) 

#Model 3
model3=lm(price~date+bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15, data=train)
summary(model3)

# Model 4 - 
library(MASS)
modelAIC<-stepAIC(model3, direction="both") #get the model summary and use log price to build next model
summary(modelAIC)
par(mfrow = c(2, 2))
plot(modelAIC)
#using the same variables as modelAIC and taking log price
modelAIClog = lm(formula = log(price) ~ date + bedrooms + bathrooms + sqft_living + 
                   sqft_lot + floors + waterfront + view + condition + grade + 
                   sqft_above + yr_built + yr_renovated + zipcode + lat + long + 
                   sqft_living15 + sqft_lot15, data = train)
summary(modelAIClog) #adjR2 88.4%
par(mfrow = c(2, 2))
plot(modelAIClog) # residual analysis

yaic=predict.glm(modelAIClog,test) # predictions
y=test$price
yaic=exp(yaic)
rmse_aic = sqrt((y-yaic)%*%(y-yaic))/nrow(test)
rmse_aic
head(yaic)
head(y)
