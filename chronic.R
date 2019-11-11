library(readxl)
chronic_with_na <- read_excel("Downloads/Chronic Kidney Disease Dataset.xls",
                      sheet = "All Data")
View(chronic)
str(chronic)
summary(chronic)
chronic$Age<- ifelse(chronic$Age < 34, 1, ifelse(chronic$Age < 64, 2, 3))
table(chronic$Age)
chronic$Age <- as.factor(chronic$Age)
is.na(chronic$Age)
#selecting columns for transforming datatypes
cols<-c("Female","Racegrp","Educ","Unmarried","Income","CareSource","Insured","Obese","Dyslipidemia","PVD","Activity"
        ,"PoorVision","Smoker","Hypertension","Fam Hypertension","Diabetes","Fam Diabetes","Stroke","CVD"
        ,"Fam CVD","CHF","Anemia","CKD")
chronic[,cols] <-  data.frame(apply(chronic[cols],2, as.factor))
str(chronic)
dim(chronic)
chronic1<-chronic
library(xlsx)
write.xlsx(chronic1, "c:/chronic1.xlsx")
write.table(chronic, file="prepped.csv",sep=",",row.names=F)
cp1<-chronic[1:6000,]
cp2<-chronic[6001:8819,]
test<-chronictest[6001:8819,]
str(chronic)
#na.omit(data, cols = c("x", "z"))
#na.omit(chronic, cols = c("Income"))
#chronic$Income<-na.omit(chronic$Income)
table(chronic$Income)

#chronic2<-na.omit(chronic)
chronic<-cp1
str(chronic)
chronic<-chronic[!is.na(chronic$Income),]
chronic<-chronic[!is.na(chronic$PoorVision),]
chronic<-chronic[!is.na(chronic$Unmarried),]
chronic<-chronic[!is.na(chronic$`Fam CVD`),]
chronic<-chronic[!is.na(chronic$DBP),]
chronic<-chronic[!is.na(chronic$Waist),]
chronic<-chronic[!is.na(chronic$SBP),]
chronic<-chronic[!is.na(chronic$BMI),]
chronic<-chronic[!is.na(chronic$Obese),]
chronic<-chronic[!is.na(chronic$Weight),]
chronic<-chronic[!is.na(chronic$Height),]
chronic<-chronic[!is.na(chronic$Insured),]
chronic<-chronic[!is.na(chronic$Hypertension),]
chronic<-chronic[!is.na(chronic$CHF),]
chronic<-chronic[!is.na(chronic$CVD),]
chronic<-chronic[!is.na(chronic$Educ),]
chronic<-chronic[!is.na(chronic$LDL),]
chronic<-chronic[!is.na(chronic$HDL),]
chronic<-chronic[!is.na(chronic$`Total Chol`),]
chronic<-chronic[!is.na(chronic$Stroke),]
chronic<-chronic[!is.na(chronic$Activity),]
chronic<-chronic[!is.na(chronic$Anemia),]
chronic<-chronic[!is.na(chronic$CareSource),]
chronic<-chronic[!is.na(chronic$Diabetes),]
is.na(chronic)
#plot(density(chronicna$Income), main="Testing paired sample differences", lwd=2, col="lightgray")

#lines(density(second_year), , lwd=2, col="steelblue")
#t.test(chronic$Age, second_year, paired=T)
chronic.aov <- aov(chronic$Weight~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected.

chronic.aov <- aov(chronic$Height~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null accepted.
van.tk<-TukeyHSD(chronic.aov)
round(van.tk$Actual,3)

TukeyHSD(chronic.aov) 
van.tk <- TukeyHSD(chronic.aov)
van.tk

chronic.aov <- aov(chronic$Age~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected.

chronic.aov <- aov(chronic$BMI~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null.

chronic.aov <- aov(chronic$Waist~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected.

chronic.aov <- aov(chronic$SBP~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected

chronic.aov <- aov(chronic$DBP~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected
chronic.aov <- aov(chronic$HDL~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected
chronic.aov <- aov(chronic$LDL~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected
chronic.aov <- aov(chronic$`Total Chol`~chronic$CKD, data=chronic)
chronic.aov
summary(chronic.aov) # Null rejected

# But we don't know what are the differences among training levels 
# We use Tukey pairwise comparisons
van.tk<-TukeyHSD(van.aov)
round(van.tk$Actual,3)

TukeyHSD(van.aov) 
van.tk <- TukeyHSD(van.aov)

library(car) # for detailed correlation plot 
library(corrplot) # for correlation plot
library(Hmisc) # for correlation test of multiple variables 

cor(cars$price,cars$mileage, use="complete.obs", method="pearson") 

l<-glm(chronic$CKD~.,data = chronic,family ="binomial")
summary(l)

table(chronicna$Income)

str(chronicna)
############################

t.test(y=chronic$Weight[chronic$CKD == 1],x=chronic$Weight[chronic$CKD == 0])
na.action(na.omit(cp1$CKD))

chisq.test(chronic$Female,chronic$CKD,correct = FALSE)#sex is NOT significant
chisq.test(chronic$Educ,chronic$CKD,correct = FALSE)#Education is significant
chisq.test(chronic$Unmarried,chronic$CKD,correct = FALSE)#Marital status is significant
chisq.test(chronic$Racegrp,chronic$CKD,correct = FALSE)#Racegroup is significant
chisq.test(chronic$Income,chronic$CKD,correct = FALSE)#Income is significant
chisq.test(chronic$CareSource,chronic$CKD,correct = FALSE)#Care Source is significant
chisq.test(chronic$Insured,chronic$CKD,correct = FALSE)#Insurance is significant
chisq.test(chronic$Obese,chronic$CKD,correct = FALSE)#Obesity is NOT significant
chisq.test(chronic$Dyslipidemia,chronic$CKD,correct = FALSE)#Dyslipidemia is NOT significant
chisq.test(chronic$PVD,chronic$CKD,correct = FALSE)#PVD is significant
chisq.test(chronic$Activity,chronic$CKD,correct = FALSE)#Activity is significant
chisq.test(chronic$PoorVision,chronic$CKD,correct = FALSE)#PoorVision is significant
chisq.test(chronic$Smoker,chronic$CKD,correct = FALSE)#Smoker is significant
chisq.test(chronic$Hypertension,chronic$CKD,correct = FALSE)#Hypertension is significant
chisq.test(chronic$`Fam Hypertension`,chronic$CKD,correct = FALSE)#Fam Hypertension is significant
chisq.test(chronic$Diabetes,chronic$CKD,correct = FALSE)#Diabetes is significant
chisq.test(chronic$`Fam Diabetes`,chronic$CKD,correct = FALSE)#Fam Diabetes is NOT significant
chisq.test(chronic$Stroke,chronic$CKD,correct = FALSE)#Stroke is significant
chisq.test(chronic$CVD,chronic$CKD,correct = FALSE)#CVD is significant
chisq.test(chronic$`Fam CVD`,chronic$CKD,correct = FALSE)#Fam CVD is significant
chisq.test(chronic$CHF,chronic$CKD,correct = FALSE)#CHF is significant
chisq.test(chronic$Anemia,chronic$CKD,correct = FALSE)#Anemia is significant

cnum <- chronic[,c("Age", "Weight", "Height","BMI","Waist","SBP","DBP","HDL","LDL","Total Chol")]
cormat <- cor(cnum) # Select only numeric variables. Otherwise, you'd get 
pairs(cnum)
#scatterplotMatrix(~Age+Weight+Height+BMI+Waist+SBP+DBP+HDL+LDL+`Total Chol`, data=chronic, main="Correlations of Numeric Variables in the Cars Data")
corrplot(cormat, method="circle")
corrplot(cormat, method="circle", addCoef.col="black") # With correlation 

l<-glm(chronic$CKD~chronic$Anemia+chronic$CHF+chronic$`Fam CVD`+chronic$CVD+
         chronic$Stroke+chronic$Diabetes+chronic$`Fam Hypertension`+
         chronic$Hypertension+chronic$Smoker+chronic$PoorVision+
         chronic$Activity+chronic$PVD+chronic$Insured+chronic$CareSource+
         chronic$Income+chronic$Racegrp+chronic$Unmarried+chronic$Educ
       +chronic$Height+chronic$Age+chronic$Waist+chronic$SBP+chronic$DBP+chronic$HDL+
         chronic$LDL+chronic$`Total Chol`,data = chronic,family ="binomial")
summary(l)

l1<-glm(chronic$CKD~chronic$Anemia+chronic$CHF+chronic$`Fam CVD`+chronic$CVD+
         chronic$Stroke+chronic$Diabetes+chronic$`Fam Hypertension`+
         chronic$Hypertension+chronic$Smoker+chronic$PoorVision+
         chronic$Activity+chronic$PVD+chronic$Insured+chronic$CareSource+
         chronic$Income+chronic$Racegrp+chronic$Unmarried+chronic$Educ
       +chronic$Age+chronic$SBP+chronic$DBP+chronic$HDL+
         chronic$LDL,data = chronic,family ="binomial")
summary(l1)


l2<-glm(chronic$CKD~chronic$Anemia+chronic$CVD+chronic$Diabetes+chronic$Hypertension+
         chronic$PVD+chronic$Racegrp+chronic$Unmarried+chronic$Age+chronic$HDL,data = chronic,family ="binomial")
summary(l2)
cpp2<-cp2
cp2<-cpp2
library(caret)
varImp(l2)
exp(coef(l2))
#pred = predict(l2, newdata=cp2)
#accuracy <- table(pred, testing[,"Class"])
#sum(diag(accuracy))/sum(accuracy)
## [1] 0.705
#pred = predict(mod_fit, newdata=testing)
#confusionMatrix(data=pred, testing$Class)
#results_prob <- predict(model,subset(test,select=c(2:9)),type='response')
results_prob<-predict(l2,type='response',chronic)
# If prob > 0.5 then 1, else 0
results <- ifelse(results_prob > 0.5,1,0)
results <-as.factor(results)
predicted<-results
# Actual answers
actual <- chronic$CKD

# Calculate accuracy
#misClasificError <- mean(answers != results)

# Collecting results
#acc[i] <- 1-misClasificError

# Average accuracy of the model
#mean(acc)

#Actual  <- train$Direction
#Actual <- factor(Actual,levels(Actual)[c(2,1)])

confmat.traindata <- table(predicted,actual)
confmat.traindata

#ConfMat <- table(pred=predict(l2,type='response',chronic), true=cp2$CKD)
accuracy1 <- sum(diag(confmat.traindata))/sum(confmat.traindata)
accuracy1 # 0.9366385

##########################################################################
cp2<-cp2[!is.na(cp2$Income),]
cp2<-cp2[!is.na(cp2$PoorVision),]
cp2<-cp2[!is.na(cp2$Unmarried),]
cp2<-cp2[!is.na(cp2$`Fam CVD`),]
cp2<-cp2[!is.na(cp2$DBP),]
cp2<-cp2[!is.na(cp2$Waist),]
cp2<-cp2[!is.na(cp2$SBP),]
cp2<-cp2[!is.na(cp2$BMI),]
cp2<-cp2[!is.na(cp2$Obese),]
cp2<-cp2[!is.na(cp2$Weight),]
cp2<-cp2[!is.na(cp2$Height),]
cp2<-cp2[!is.na(cp2$Insured),]
cp2<-cp2[!is.na(cp2$Hypertension),]
cp2<-cp2[!is.na(cp2$CHF),]
cp2<-cp2[!is.na(cp2$CVD),]
cp2<-cp2[!is.na(cp2$Educ),]
cp2<-cp2[!is.na(cp2$LDL),]
cp2<-cp2[!is.na(cp2$HDL),]
cp2<-cp2[!is.na(cp2$`Total Chol`),]
cp2<-cp2[!is.na(cp2$Stroke),]
cp2<-cp2[!is.na(cp2$Activity),]
cp2<-cp2[!is.na(cp2$Anemia),]
cp2<-cp2[!is.na(cp2$CareSource),]
cp2<-cp2[!is.na(cp2$Diabetes),]
test<-cp2
cp2<-cp2[!is.na(cp2$CKD),]
cpp<-cp2
cp2$CKD<- NULL
results_prob<-predict(l2,type='response',newdata=cp2)
#predict(object = l2, newdata = cp2)
# If prob > 0.5 then 1, else 0
results <- ifelse(results_prob > 0.5,1,0)
results <-as.factor(results)
predicted_test<-results
summary(predicted_test)
actual <- cp2$CKD
confmat.testdata <- table(predicted,actual)
confmat.testdata

#ConfMat <- table(pred=predict(l2,type='response',chronic), true=cp2$CKD)
accuracy1 <- sum(diag(confmat.traindata))/sum(confmat.traindata)
accuracy1 # 0.9366385

#predicted<-predict(l2, type='response', type='class')
chronic<-cp1
l2<-glm(chronic$CKD~chronic$Anemia+chronic$CVD+chronic$Diabetes+chronic$Hypertension+
          chronic$PVD+relevel(chronic$Racegrp,ref="white")+chronic$Unmarried+chronic$Age+chronic$HDL,data = chronic,family ="binomial")
summary(l2)
str(test$CKD)
summary(test$CKD)


################################################################
#Chronic with NA

cols<-c("Female","Racegrp","Educ","Unmarried","Income","CareSource","Insured","Obese","Dyslipidemia","PVD","Activity"
        ,"PoorVision","Smoker","Hypertension","Fam Hypertension","Diabetes","Fam Diabetes","Stroke","CVD"
        ,"Fam CVD","CHF","Anemia","CKD")
chronic_with_na[,cols] <-  data.frame(apply(chronic_with_na[cols],2, as.factor))

chronic_with_na<-chronic_with_na[!is.na(chronic_with_na$Unmarried),]
chronic_with_na<-chronic_with_na[!is.na(chronic_with_na$Hypertension),]
chronic_with_na<-chronic_with_na[!is.na(chronic_with_na$CVD),]
chronic_with_na<-chronic_with_na[!is.na(chronic_with_na$HDL),]
chronic_with_na<-chronic_with_na[!is.na(chronic_with_na$Anemia),]
chronic_with_na<-chronic_with_na[!is.na(chronic_with_na$Diabetes),]
is.na(chronic_with_na)
train<-chronic_with_na[1:6000,]
test<-chronic_with_na[6001:8819,]
val<-train[3501:4135,]
training<-train[1:3500,]
l_na<-glm(CKD~Anemia+CVD+Diabetes+Hypertension+
          PVD+Racegrp+Unmarried+Age+HDL,data = training,family =binomial(link="logit"))
summary(l_na)
results_prob1<-predict(l_na,type='response',chronic_with_na)
results1 <- ifelse(results_prob1 > 0.5,1,0)
results1 <-as.factor(results1)
predicted1<-results1
actual1 <- chronic_with_na$CKD
confmat.traindata1 <- table(predicted1,actual1)
confmat.traindata1

#ConfMat <- table(pred=predict(l2,type='response',chronic), true=cp2$CKD)
accuracy2 <- sum(diag(confmat.traindata1))/sum(confmat.traindata1)
accuracy2 # 0.9256757

predTest<-predict(object=l_na,newdata = test, type="response")
results_test_na<-ifelse(predTest > 0.1,1,0)
results_test_na<-as.factor(results_test_na)
levels(results_test_na)<-c("No CKD","CKD")
summary(results_test_na)
#################################################################
chronic_race<-chronic
#Removing racegroup categories which are not important
chronic_race<- chronic_race[chronic_race$Racegrp=="hispa",]
chronic_race$Racegrp<- droplevels(chronic_race$Racegrp)

chronic_race$Racegrp<-as.character(chronic_race$Racegrp)
l_race<-glm(chronic_race$CKD~chronic_race$Anemia+chronic_race$CVD+chronic_race$Diabetes+chronic_race$Hypertension+
            chronic_race$PVD+chronic_race$Racegrp+chronic_race$Unmarried+chronic_race$Age+chronic_race$HDL,data = chronic_race,family ="binomial")
summary(l_race)


#################################

library(gmodels)
CrossTable(chronic$PoorVision,chronic$Diabetes)
