# This is how you call the packages
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)


setwd("C:\\Users\\Lenovo\\Desktop\\Uma")

# Reading data; just change the file path to fetch the data.
data <- read.csv("data1.csv")
head(data)
# Data sanity check
str(data)


################################## IV for numeric data  ##############################################################
iv_num <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                        count(%s) n,
                        sum(%s) good
                 from data 
                 group by rank",target,target))
  tableOutput <- sqldf("select *,
                        (n - good) bad
                         from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- iv_num("utilization","Churn",data,groups=10)
a2<- iv_num("Age","Churn",data,groups=10)
a3<- iv_num("MonthlyIncome","Churn",data,groups=10)
a4<- iv_num("DebtRatio","Churn",data,groups=10)


IV_num<- data.frame(rbind(a1,a2,a3,a4))


################################## IV for categorical data  ##############################################################

iv_char <- function(variable, target, data) {
 	pintu1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")

pintu1<- fn$sqldf("select *, (n-good) bad from pintu1")
pintu1$bad_rate <- pintu1$bad/sum(pintu1$bad)*100

pintu1$good_rate<- pintu1$good/sum(pintu1$good)*100
pintu1$WOE<- (log(pintu1$good_rate/pintu1$bad_rate))*100
pintu1$IV <- (log(pintu1$good_rate/pintu1$bad_rate))*(pintu1$good_rate-pintu1$bad_rate)/100
IV <- sum(pintu1$IV[is.finite(pintu1$IV)])
IV1 <- data.frame(cbind(variable,IV))
return(IV1)
}

A<- iv_char("Num_loans","Churn",data)
B<- iv_char("Num_dependents","Churn",data)
C<- iv_char("Num_Savings_Acccts","Churn",data)

IV_cat<- data.frame(rbind(A,B,C))

Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

############################ IV calculations complete ############################################


# Converting necessary variables into factor
data$Num_loans <- as.factor(data$Num_loans)
data$Num_dependents <- as.factor(data$Num_dependents)
data$Num_Savings_Acccts <- as.factor(data$Num_Savings_Acccts)

## Descriptive analysis
summary(data)


boxplot(data$utilization)
boxplot(data$Age)
quantile(data$Age, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
data <- data[data$Age <= 89 ,]

boxplot(data$MonthlyIncome)
quantile(data$MonthlyIncome, c(0,.005,.01,.02,.03,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
data <- data[data$MonthlyIncome >= 1870,] ## removing lower outliers
data <- data[data$MonthlyIncome <= 12500,] ## removing higher outliers

boxplot(data$DebtRatio)
quantile(data$DebtRatio, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
data <- data[data$DebtRatio <= 0.58,] 

((150000 - nrow(data))/150000)*100


## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))


names(data)



# Logistic Regression on full data

model <- glm(Churn~utilization+	Age+	Num_loans+	Num_dependents+	
MonthlyIncome+	Num_Savings_Acccts+	DebtRatio, data=data, family=binomial())
summary(model)

model <- glm(Churn~utilization+	Age+	Num_loans+	Num_dependents +	
MonthlyIncome+	DebtRatio, data=data, family=binomial())
summary(model)

model <- glm(Churn~utilization+	Age+	Num_loans+	I(Num_dependents==1) +I(Num_dependents==2)+	
I(Num_dependents==3)+ I(Num_dependents==4)+ I(Num_dependents==5)+ I(Num_dependents==6)+
I(Num_dependents==7)+MonthlyIncome+	DebtRatio, data=data, family=binomial())
summary(model)


model <- glm(Churn~utilization+	Age+	I(Num_loans==1)+I(Num_loans==3)+I(Num_loans==4)+
I(Num_loans==4)+I(Num_loans==5)+I(Num_loans==6)+I(Num_loans==7)+I(Num_loans==8)+I(Num_loans==9)+
I(Num_loans==10)+I(Num_loans==12)+I(Num_loans==14)+
I(Num_dependents==1) +I(Num_dependents==2)+	
I(Num_dependents==3)+ I(Num_dependents==4)+ I(Num_dependents==5)+ I(Num_dependents==6)+
I(Num_dependents==7)+MonthlyIncome+	DebtRatio, data=data, family=binomial())
summary(model)

vif(model)





# R square (nagelkarke)
modelChi <- model$null.deviance - model$deviance
#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
R2.hl<-modelChi/model$null.deviance
R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data))
R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data))))))
R.n ## ranges from 0 to 1; closer to 1 better the model


#####################################################################################################################

# Predicted Probabilities
prediction <- predict(model,newdata = data,type="response")
library(pROC)
rocCurve   <- roc(response = data$Churn, predictor = prediction, 
levels = rev(levels(data$Churn)))
data$Churn <- as.factor(data$Churn)
#Metrics - Fit Statistics
predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)


#########################################################################################################################
### KS statistics calculation
data$m1.yhat <- predict(model, data, type = "response")
library(ROCR)
m1.scores <- prediction(data$m1.yhat, data$Churn)
plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")
m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40% - 70%

############################################################################################################
write.csv(data,"result.csv")
