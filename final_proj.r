library(ggplot2)
library(dplyr)
library(faraway)
library(tidyr)
library(ResourceSelection)
#CLEANING 
setwd("~/Desktop/Linear Models")
bank <-  read.csv('bank-additional.csv')
## transform into some factors
bank$job <- as.factor(bank$job)
bank$marital <- as.factor(bank$marital)
bank$education <- as.factor(bank$education)
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$contact <- as.factor(bank$contact)
bank$month <- as.factor(bank$month)
bank$poutcome <- as.factor(bank$poutcome)
bank$y <- as.factor(bank$y)
bank$previous <- as.factor(bank$previous)
summary(bank)
## check for missing values and we find no missing values
sum(!complete.cases(bank))

################

bank$y <- ifelse(bank$y=='yes',1,0)
barplot(table(bank$y)) #class distribution

bank$y <- as.factor(bank$y)

cols <- names(bank)[sapply(bank, is.numeric)]
for(col in cols){
  plot(density(bank[,col]),xlab = col)
}

cols2 <- names(bank)[which(!names(bank) %in% cols)]

for(col in cols2){
  barplot(summary(bank[,col]), xlab = col)
}

#variables of interest
plot(y ~ job, bank)


ggplot(bank, aes(x=job, fill = y)) + geom_bar()
ggplot(bank, aes(x=education, fill = y)) + geom_bar()
ggplot(bank, aes(x=day_of_week, fill = y)) + geom_bar() #might have an impact in combination with other variables 
ggplot(bank, aes(x=loan, fill = y)) + geom_bar()
ggplot(bank, aes(x=previous, fill = y)) + geom_bar()
ggplot(bank, aes(x=campaign, fill = y)) + geom_bar()
ggplot(bank, aes(x=contact, fill = y)) + geom_bar()
ggplot(bank, aes(x=day_of_week, fill = y)) + geom_bar()
ggplot(bank, aes(x=housing, fill = y)) + geom_bar()

e <- ggplot(bank, aes(x = y, y = duration))
# Combine with box plot to add median and quartiles
# Change color by groups
e + geom_violin(aes(fill = y), trim = FALSE) + 
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme(legend.position = "none")

e <- ggplot(bank, aes(x = y, y = nr.employed))
# Combine with box plot to add median and quartiles
# Change color by groups
e + geom_violin(aes(fill = y), trim = FALSE) + 
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme(legend.position = "none")

#unknown values 
bank %>% 
  summarise_all(list(~sum(. == "unknown"))) %>% 
  gather(key = "variable", value = "nr_unknown") %>% 
  arrange(-nr_unknown)
##economic factors 

##dropping bad variables 
drops <- c("pdays","poutcome", "contact", "day_of_week", "previous")#Made some improvements. 
bank <- bank[ , !(names(bank) %in% drops)]

##fiting the model 
logistic_fit <- glm(y~.,data=bank,family=binomial)
sumary(logistic_fit)

best_logistic <- step(logistic_fit, trace=0)
sumary(best_logistic)

ypred <- predict(best_logistic,newdata=bank,type="response")

bank_2 <- bank
bank_2$pred <- ypred

testm <- mutate(bank_2, pred=ifelse(pred < 0.085, 0, 1))
xtabs( ~ y + pred, testm)


suppressMessages(library(pROC))
roc_obj <- roc(response=bank_2$y, predictor=bank_2$pred)
AUC <- auc(roc_obj)
roc_logistic <- c(coords(roc_obj, "b",
                         ret=c("threshold","se","sp","accuracy"),
                         best.method="youden"),AUC)
names(roc_logistic) <- c("Threshold","Sensitivity","Specificity",
                         "Accuracy","AUC")
t(roc_logistic)

plot(roc_obj,legacy.axes=FALSE,print.auc=TRUE,print.thres=TRUE,cex.lab=2)

#interpretation of coefficients. 
beta <- coef(best_logistic)
odds <- exp(beta)

#hosmer_lemeshow
hoslem.test(logistic_fit$y,fitted(logistic_fit),g=10)
#chisq
df <- length(best_logistic$coefficients) - 1
1-pchisq(1160,df)
