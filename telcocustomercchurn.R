#setting wd
setwd("C:\\Users\\DELASUPREME\\Desktop\\R scripts and data")
getwd()

#load packages
library(stringr)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)

#import data
telco <- read.csv("C:\\Users\\DELASUPREME\\Desktop\\R scripts and data\\WA_Fn-UseC_-Telco-Customer-Churn.csv")

#see and feel data
glimpse(telco)

#viz missing data

summary(telco)#na's are 11

options(repr.plot.width=7, repr.plot.height=5)
missing_data <- telco%>% summarise_all(funs(sum(is.na(.))/n()))
missing_data<- gather(missing_data, key='variables', value = 'percent_missing')
ggplot(missing_data, aes(x= reorder(variables, percent_missing), y=  percent_missing))+
  geom_bar(stat='identity', fill='grey', aes(color = I('blue')), size=0.4)+
  xlab('variables')+
  coord_flip()+
  theme_classic()

#specifies which obs. have no missing values $ gets rid of na's
telco <- telco[complete.cases(telco),]

#change the senior citizen to factor and set it to boolean
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1,  'YES', 'NO'))

###EDA
str(telco)
options(repr.plot.width = 7, repr.plot.height = 5)
telco %>%
  group_by(Churn) %>%
  summarise(Count = n())%>%
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("grey45", "grey75"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")  
  
  
#plot grid for comparison of variables
options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar()+theme(), 
          ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme(),
          ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme(),
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme(),
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme(),
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h") 
  
options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme() +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme() +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")
  
  


plot_grid(ggplot(telco, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+ theme()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+theme()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+theme()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

#Analyzing the 3 cont var w.r.t to churn
#1 tenure
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#2 monthly charges
ggplot(telco, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#3 total charges
ggplot(telco, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#correlation between continous variables
options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(telco[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

#checking for outliers in continous variables
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$tenure)$out
boxplot(telco$MonthlyCharges)$out
boxplot(telco$TotalCharges)$out

#Data preparation
#cleaning the cartegorical features
telco <- data.frame(lapply(telco, function(x) {
  gsub("No internet service", "No", x)}))

telco <- data.frame(lapply(telco, function(x) {
  gsub("No phone service", "No", x)}))

#standardising continous features

num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco[num_columns] <- sapply(telco[num_columns], as.numeric)

telco_int <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

#creating derived features
#max(telco$tenure)
#min(telco$tenure)
telco <- mutate(telco, tenure_bin = tenure)

telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'

telco$tenure_bin <- as.factor(telco$tenure_bin)

options(repr.plot.width =6, repr.plot.height = 3)
ggplot(telco, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ theme()

#creating dummy variables
telco_cat <- telco[,-c(1,6,19,20)]

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))

head(dummy)

#creating the final dataset by combining the dummy data frame

#Combining the data
telco_final <- cbind(telco_int,dummy)
head(telco_final)

#splitting the data into train and validation data
#Splitting the data
set.seed(123)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

####Model building 1
##starting with logistic regressionn

#Build the first model using all variables
model_1 = glm(Churn ~ ., data = train, family = "binomial")
summary(model_1)

#using step AIC for variable selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

vif(model_2)

#Removing DeviceProtection due to high p-value 
model_3 <-glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                Partner + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineSecurity + OnlineBackup + TechSupport + 
                StreamingTV + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                tenure_bin.x5.6.years, family = "binomial", data = train)
summary(model_3)
vif(model_3)

#Removing StreamingTV  as it has high p-value 

model_4 <- glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                 Partner + InternetService.xFiber.optic + InternetService.xNo + 
                 OnlineSecurity + OnlineBackup + TechSupport +  
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                 tenure_bin.x5.6.years, family = "binomial", data = train)

summary(model_4)
vif(model_4)

#model_3 all has significant var. lets  use it for prediction
final_model <- model_3

#model evaluation using the  validation data
pred <- predict(final_model, type = "response", newdata = validation[,-24])
summary(pred)
validation$prob <- pred

# Using probability cutoff of 50%.

pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)

#finding accuracy, sensitivity, specificity
cutoff_churn <- factor(ifelse(pred >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity

#we need to find the optimal prob cutoff which will give max. accuracy, sensitivity
#and specificity

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, actual_churn, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))

#cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#choose a cutoff value of .32 

cutoff_churn <- factor(ifelse(pred >=0.32, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity

##Model Building 2
#Decision tree

#prepare data
set.seed(123)
telco_final$Churn <- as.factor(telco_final$Churn)

indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

#Training decision tree using all var.
options(repr.plot.width = 10, repr.plot.height = 8)
library(rpart)
library(rpart.plot)

#Training
Dtree = rpart(Churn ~., data = train, method = "class")
summary(Dtree)

#Predicting 
DTPred <- predict(Dtree,type = "class", newdata = validation[,-24])

#check confusion matrix

confusionMatrix(validation$Churn, DTPred)

##Model Building 3
#Random forest

library(randomForest)
set.seed(123)
telco_final$Churn <- as.factor(telco_final$Churn)

indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

#Training the RandomForest Model

model.rf <- randomForest(Churn ~ ., data=train, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
model.rf

#Predicting on the validation set and checking the Confusion Matrix.
testPred <- predict(model.rf, newdata=validation[,-24])
table(testPred, validation$Churn)

confusionMatrix(validation$Churn, testPred)

#Checking the variable Importance Plot
varImpPlot(model.rf)

#checking the AUC for all models built

options(repr.plot.width =10, repr.plot.height = 8)

glm.roc <- roc(response = validation$Churn, predictor = as.numeric(pred))
DT.roc <- roc(response = validation$Churn, predictor = as.numeric(DTPred))
rf.roc <- roc(response = validation$Churn, predictor = as.numeric(testPred))

plot(glm.roc,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
plot(DT.roc, col = "blue", add = TRUE, print.auc.y = 0.65, print.auc = TRUE)
plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 0.85, print.auc = TRUE)
legend("bottom", c("Random Forest", "Decision Tree", "Logistic"),
       lty = c(1,1), lwd = c(2, 2), col = c("red", "blue", "black"), cex = 0.75)

###Logistic Regression:

#Accuracy 75.59%,
#Sensitivity 75.75%
#Specificity 75.53%


###DecisionTrees:

#Accuracy 78.1%,
#Sensitivity 82.45%
#Specificity 61.38%

###RandomForest:

#Accuracy 78.86%,
#Sensitivity 82.46%
#Specificity 63.99%

















