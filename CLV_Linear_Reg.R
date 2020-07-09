#Library to run the model
library(boot) 
library(car)
library (QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)


#Path of csv file
setwd("C:\\Users\\DELL\\Desktop\\UMA\\R")


#To read csv file
data<- read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv")


#to get structure of data
str(data)


#To get summary of given data
summary(data)


#To find the outliers
boxplot(data$Customer.Lifetime.Value)
quantile(data$Customer.Lifetime.Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

data2 <- data[data$Customer.Lifetime.Value<15000, ]
boxplot(data2$Customer.Lifetime.Value)

nrow(data)-nrow(data2)
data <- data2



boxplot(data$Monthly.Premium.Auto)
quantile(data$Monthly.Premium.Auto, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

data2 <- data[data$Monthly.Premium.Auto<160, ]
boxplot(data2$Monthly.Premium.Auto)

nrow(data)-nrow(data2)
data <- data2


boxplot(data$Total.Claim.Amount)
quantile(data$Total.Claim.Amount, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

data2 <- data[data$Total.Claim.Amount<930, ]
boxplot(data2$Total.Claim.Amount)

nrow(data)-nrow(data2)
data <- data2


# Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))

nrow(data)
names(data)


#To run a model
fit <- lm(Customer.Lifetime.Value ~ State + Response + Coverage +
            Education +Effective.To.Date + 
            EmploymentStatus + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            Policy + Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ State + Response + Coverage +
            Education +Effective.To.Date + 
            EmploymentStatus + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ Response + Coverage +
            Education +Effective.To.Date + 
            EmploymentStatus + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ Coverage +
            Education +Effective.To.Date + 
            EmploymentStatus + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ Education +Effective.To.Date + 
            EmploymentStatus + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            Effective.To.Date + 
            EmploymentStatus + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            EmploymentStatus + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") + 
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") + 
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + Marital.Status + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") +
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies + Policy.Type +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") + 
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Last.Claim + Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") +
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Sales.Channel + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") + 
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type + Total.Claim.Amount +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") + 
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type +
            Vehicle.Class + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + 
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") + 
            I(EmploymentStatus == "Unemployed") + Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type +
            I(Vehicle.Class == "Sports Car") + Vehicle.Size,data=data)
summary(fit)


fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") + I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") +Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type +
            I(Vehicle.Class == "Sports Car"),data=data)
summary(fit)


#Final model 
fit <- lm(Customer.Lifetime.Value ~ I(Education == "College") +
            I(Education == "High School or Below") + 
            I(Effective.To.Date == "1/14/2011") + I(Effective.To.Date == "1/18/2011") +
            I(Effective.To.Date == "1/29/2011") + I(Effective.To.Date == "1/30/2011") +
            I(Effective.To.Date == "2/17/2011") + I(Effective.To.Date == "2/25/2011") +
            I(EmploymentStatus == "Medical Leave") +Gender +
            Income + I(Marital.Status == "Single") + Monthly.Premium.Auto +
            Months.Since.Policy.Inception + 
            Number.of.Open.Complaints + Number.of.Policies +
            I(Policy == "Corporate L2") + I(Policy == "Corporate L3") + 
            Renew.Offer.Type +
            I(Vehicle.Class == "Sports Car"),data=data)
summary(fit)


#Check Vif
# Checking multicollinearity
vif(fit)


# Get the predicted or fitted values
fitted(fit)


# MAPE
data$pred <- fitted(fit)
head(data$pred)


#Calculating MAPE
attach(data)
(sum((abs(Customer.Lifetime.Value-pred))/Customer.Lifetime.Value))/nrow(data)


Checking of Assumption :

dwt(fit)

# Breusch-Pagan test 
bptest(fit) 


# Normality testing Null hypothesis is data is normal
resids <- fit$residuals
hist(resids,col="red")


#To get Anderson-Darling test for normality
ad.test(resids)


#To get the predicted data
write.csv(data, "Interpretation.csv")
