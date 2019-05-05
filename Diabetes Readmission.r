setwd("C:\\Users\\Vyom\\Desktop\\Stuff\\Vyom\\Minor Project - II")
library(ggplot2)
diabetesData <- read.csv('diabetic_data.csv')
temp <- read.csv('diabetic_data.csv')

usefulData$age <- temp$age[1:97825]

summary(diabetesData)
head(diabetesData)

usefulData <- subset(diabetesData,select = -c(encounter_id,patient_nbr,examide,citoglipton,payer_code,medical_specialty))
usefulData <- subset(usefulData,select = -c(weight))

numericDataFirst <-select_if(usefulData,is.numeric)
corMatrix <- cor(numericDataFirst,use="pairwise.complete.obs")
corrplot(corMatrix)

usefulData$race[is.na((usefulData$race))] <- "Other"

racePlot <- ggplot(usefulData,
                   aes(x = race,stat="count",fill=race))
racePlot + geom_bar(width = 0.2) + labs(x="Race",y="Number of people")

genderPlot <- ggplot(usefulData,
                     aes(x=gender,stat="count",fill=gender))
genderPlot + geom_bar(width = 0.2) + labs(x="Gender",y="Number of people")

agePlot <- ggplot(usefulData,
                  aes(x=age,stat="count",fill=age))
agePlot + geom_bar(width = 0.2) + labs(x="Age group",y="Number of people")


A1C_ResultPlot <- ggplot(usefulData,
                         aes(x=A1Cresult,stat="count",fill=A1Cresult))
A1C_ResultPlot + geom_bar(width = 0.2) + labs(x="A1C Result",y="Number of people")


readmittedPlot <- ggplot(usefulData,
                         aes(x=readmitted,stat="count",fill=readmitted))
readmittedPlot + geom_bar(width = 0.2) + labs(x="Days for readmission",y="Number of people")






usefulData <- usefulData[usefulData$diag_1 != "?",]
usefulData <- usefulData[usefulData$diag_2 != "?",]
usefulData <- usefulData[usefulData$diag_3 != "?",]

usefulData <- usefulData[usefulData$discharge_disposition_id != 11,]
usefulData <- usefulData[usefulData$discharge_disposition_id != 13,]
usefulData <- usefulData[usefulData$discharge_disposition_id != 14,]
usefulData <- usefulData[usefulData$discharge_disposition_id != 19,]
usefulData <- usefulData[usefulData$discharge_disposition_id != 20,]
usefulData <- usefulData[usefulData$discharge_disposition_id != 21,]

str(usefulData)

usefulData$diag_1 <- as.numeric(levels(usefulData$diag_1)[usefulData$diag_1])
usefulData$diag_2 <- as.numeric(levels(usefulData$diag_2)[usefulData$diag_2])
usefulData$diag_3 <- as.numeric(levels(usefulData$diag_3)[usefulData$diag_3])

# Change numbers to actual diagnoses using hospital codes


usefulData$diag_group <- factor( rep("other",nrow(usefulData)),ordered = F, levels = c("circulatory","respiratory","Digestive","Diabetes","Injury","Musculoskeletal","Genitourinary","Neoplasms","other"))
usefulData$diag_group[usefulData$diag_1>=390 & usefulData$diag_1 <= 459 | usefulData$diag_1==785] <- "circulatory"
usefulData$diag_group[usefulData$diag_1>=460 & usefulData$diag_1 <= 519 | usefulData$diag_1==786] <- "respiratory"
usefulData$diag_group[usefulData$diag_1>=520 & usefulData$diag_1 <= 579 | usefulData$diag_1==787] <- "Digestive"
usefulData$diag_group[usefulData$diag_1>=250 & usefulData$diag_1 < 251] <- "Diabetes"
usefulData$diag_group[usefulData$diag_1>800 & usefulData$diag_1 <= 999] <- "Injury"
usefulData$diag_group[usefulData$diag_1>=710 & usefulData$diag_1 <= 739] <- "Musculoskeletal"
usefulData$diag_group[usefulData$diag_1>=580 & usefulData$diag_1 <= 629 | usefulData$diag_1==788] <- "Genitourinary"
usefulData$diag_group[usefulData$diag_1>=140 & usefulData$diag_1 <= 239 | usefulData$diag_1>=790 & usefulData$diag_1 <= 799 | usefulData$diag_1==780 | usefulData$diag_1>=240 & usefulData$diag_1 < 250 | usefulData$diag_1>=251 & usefulData$diag_1 <= 279 | usefulData$diag_1>=680 & usefulData$diag_1 <= 709 | usefulData$diag_1>=001 & usefulData$diag_1 <= 139 | usefulData$diag_1==781 | usefulData$diag_1==782 | usefulData$diag_1==784] <- "Neoplasms"

usefulData$diag_group2 <- factor( rep("other",nrow(usefulData)),ordered = F, levels = c("circulatory","respiratory","Digestive","Diabetes","Injury","Musculoskeletal","Genitourinary","Neoplasms","other"))
usefulData$diag_group2[usefulData$diag_2>=390 & usefulData$diag_2 <= 459 | usefulData$diag_2==785] <- "circulatory"
usefulData$diag_group2[usefulData$diag_2>=460 & usefulData$diag_2 <= 519 | usefulData$diag_2==786] <- "respiratory"
usefulData$diag_group2[usefulData$diag_2>=520 & usefulData$diag_2 <= 579 | usefulData$diag_2==787] <- "Digestive"
usefulData$diag_group2[usefulData$diag_2>=250 & usefulData$diag_2 < 251] <- "Diabetes"
usefulData$diag_group2[usefulData$diag_2>800 & usefulData$diag_2 <= 999] <- "Injury"
usefulData$diag_group2[usefulData$diag_2>=710 & usefulData$diag_2 <= 739] <- "Musculoskeletal"
usefulData$diag_group2[usefulData$diag_2>=580 & usefulData$diag_2 <= 629 | usefulData$diag_2==788] <- "Genitourinary"
usefulData$diag_group2[usefulData$diag_2>=140 & usefulData$diag_2 <= 239 | usefulData$diag_2>=790 & usefulData$diag_2 <= 799 | usefulData$diag_2==780 | usefulData$diag_2>=240 & usefulData$diag_2 < 250 | usefulData$diag_2>=251 & usefulData$diag_2 <= 279 | usefulData$diag_2>=680 & usefulData$diag_2 <= 709 | usefulData$diag_2>=001 & usefulData$diag_2 <= 139 | usefulData$diag_2==781 | usefulData$diag_2==782 | usefulData$diag_2==784] <- "Neoplasms"

usefulData$diag_group3 <- factor( rep("other",nrow(usefulData)),ordered = F, levels = c("circulatory","respiratory","Digestive","Diabetes","Injury","Musculoskeletal","Genitourinary","Neoplasms","other"))
usefulData$diag_group3[usefulData$diag_3>=390 & usefulData$diag_3 <= 459 | usefulData$diag_3==785] <- "circulatory"
usefulData$diag_group3[usefulData$diag_3>=460 & usefulData$diag_3 <= 519 | usefulData$diag_3==786] <- "respiratory"
usefulData$diag_group3[usefulData$diag_3>=520 & usefulData$diag_3 <= 579 | usefulData$diag_3==787] <- "Digestive"
usefulData$diag_group3[usefulData$diag_3>=250 & usefulData$diag_3 < 251] <- "Diabetes"
usefulData$diag_group3[usefulData$diag_3>800 & usefulData$diag_3 <= 999] <- "Injury"
usefulData$diag_group3[usefulData$diag_3>=710 & usefulData$diag_3 <= 739] <- "Musculoskeletal"
usefulData$diag_group3[usefulData$diag_3>=580 & usefulData$diag_3 <= 629 | usefulData$diag_3==788] <- "Genitourinary"
usefulData$diag_group3[usefulData$diag_3>=140 & usefulData$diag_3 <= 239 | usefulData$diag_3>=790 & usefulData$diag_3 <= 799 | usefulData$diag_3==780 | usefulData$diag_3>=240 & usefulData$diag_3 < 250 | usefulData$diag_3>=251 & usefulData$diag_3 <= 279 | usefulData$diag_3>=680 & usefulData$diag_3 <= 709 | usefulData$diag_3>=001 & usefulData$diag_3 <= 139 | usefulData$diag_3==781 | usefulData$diag_3==782 | usefulData$diag_3==784] <- "Neoplasms"


# changing admission source from numbers to actual sources using the Mappings file provided


usefulData$admission_source <- factor(rep("other",nrow(usefulData)),ordered=F,levels=c("clinic_referral","emergency","other"))
usefulData$admission_source[usefulData$admission_source_id==c(1,2,3)] <- "clinic_referral"
usefulData$admission_source[usefulData$admission_source_id==7] <-"emergency"

usefulData$discharged_to <- factor(rep("transferred",nrow(usefulData)),ordered=F,levels=c("home","transferred","left_AMA"))
usefulData$discharged_to[usefulData$discharge_disposition_id==c(1,6,8)] <- "home"
usefulData$discharged_to[usefulData$discharge_disposition_id==7] <- "left_AMA"

usefulData <- select(usefulData, -diag_1,-diag_2,-diag_3)


# Plotting some uni-variate graphs

admissionSourcePlot <- ggplot(usefulData,
                              aes(x=admission_source,stat="count",fill=admission_source))
admissionSourcePlot + geom_bar(width = 0.2) + labs(x="Source of admission",y="Number of people")

diabetesMedPlot <- ggplot(usefulData,
                          aes(x=diabetesMed,stat="count",fill=diabetesMed))
diabetesMedPlot + geom_bar(width = 0.2) + labs(x="Diabetes Medication",y="Number of people")

metforminPlot <- ggplot(usefulData,
                        aes(x=metformin,stat="count",fill=metformin))
metforminPlot + geom_bar(width = 0.2) + labs(x="Metformin Condition",y="Number of people")


# Performing Principal Component Analysis 

numericDataSecond <- select_if(usefulData,is.numeric)
numericDataSecond <- scale(numericDataSecond)
pcaObject <- princomp(numericDataSecond,cor=TRUE,scores=TRUE,covmat=NULL)
summary(pcaObject)
plot(pcaObject,main="Variances")
pcaObject$loadings

str(usefulData$age)


# Plotting some variables against target variable i.e readmitted 

readmittedVersusAgePlot <- ggplot(usefulData,
                                  aes(x=readmitted,stat="count",fill=age))
readmittedVersusAgePlot + geom_bar(width = 0.2) + labs(x="Days for readmission",y="Number of patients")

readmittedVersusGenderPlot <- ggplot(usefulData,
                                     aes(x=readmitted,stat="count",fill=gender))
readmittedVersusGenderPlot + geom_bar(width = 0.2) + labs(x="Days for readmission",y="Number of patients")


readmittedVersusA1CPlot <- ggplot(usefulData,
                                  aes(x=readmitted,stat="count",fill=A1Cresult))
readmittedVersusA1CPlot + geom_bar(width = 0.2) + labs(x="Days for readmission",y="Number of patients")

readmittedVersusDiabetesMed <- ggplot(usefulData,
                                      aes(x=readmitted,stat="count",fill=diabetesMed))
readmittedVersusDiabetesMed + geom_bar(width = 0.2) + labs(x="Days for readmission",y="Number of patients")


head(usefulData)

summary(usefulData$age)
str(usefulData$age)
str(usefulData$readmitted)

usefulData$readmittedBin <- ifelse(usefulData$readmitted == "<30",1,0)

str(diabetesData$diag_1)


# Creating training and testing data. 70% has been taken for training the models and 30% for testing.


set.seed(123)
trainingData <- createDataPartition(y=usefulData$readmitted,p=.7,list=FALSE)

train <- usefulData[trainingData,]
test <- usefulData[-trainingData,]
nrow(train)
nrow(test)

nonBinaryTrain <- train
nonBinaryTest <- test
train$readmitted <- ifelse(train$readmitted == "<30",1,0)
test$readmitted <- ifelse(test$readmitted == "<30",1,0)

rm(logisticRegressionModel)

?glm()

############## Logistic Regression 


logisticRegressionModel <- glm(readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                               + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                               + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                               data=train,family = binomial(link='logit'))


predictorLogisticRegression <- predict(logisticRegressionModel,test,type="response")

predictorLogisticRegression

predictorLogisticRegression <- ifelse(predictorLogisticRegression > 0.5,1,0)

result <- as.data.frame(table(predictorLogisticRegression,test$readmitted))

correctPredictions <- result[1,3] + result[4,3]
sensitivityLogisticBinary <- result[4,3]/(result[2,3] + result[4,3])

specificityLogisticBinary <- result[1,3]/(result[3,3] + result[1,3])

accuracyLogisticRegression <- correctPredictions/nrow(test)
accuracyLogisticRegression




logisticRegressionModelNonBinary <- glm(readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                                        + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                                        + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                                        data=nonBinaryTrain,family = binomial(link='logit'))


predictorLogisticRegressionNonBinary <- predict(logisticRegressionModelNonBinary,test,type="response")

predictorLogisticRegressionNonBinary

logisticRegressionConfusionNonBinaryMatrix <- confusionMatrix(predictorLogisticRegressionNonBinary,nonBinaryTest$readmitted)

decisionTreeConfusionMatrix

predictorLogisticRegressionNonBinary <- ifelse(predictorLogisticRegressionNonBinary > 0.5,1,0)

resultNonBinary <- as.data.frame(table(predictorLogisticRegressionNonBinary,nonBinaryTest$readmitted))

correctPredictionsNonBinary <- resultNonBinary[1,3] + resultNonBinary[4,3]

sensitivityLogistic <- resultNonBinary[4,3]/(resultNonBinary[2,3] + resultNonBinary[4,3])

specificityLogistic <- resultNonBinary[1,3]/(resultNonBinary[1,3] + resultNonBinary[3,3])
accuracyLogisticRegressionNonBinary <- correctPredictionsNonBinary/nrow(nonBinaryTest)
accuracyLogisticRegressionNonBinary

################## Decision Tree ###################


decisionTree <- rpart(formula = readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                      + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                      + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                      data = nonBinaryTrain,method='class')
summary(decisionTree)


predictorTree <- predict(decisionTree,nonBinaryTest,type='class')


table(predict(decisionTree,nonBinaryTest,type='class'),nonBinaryTest$readmitted)

decisionTreeConfusionMatrix <- confusionMatrix(predictorTree,nonBinaryTest$readmitted)
decisionTreeConfusionMatrix
plot(decisionTreeConfusionMatrix$table)
prop.table(table(nonBinaryTest$readmitted,predictorTree),1)

decisionTreeResult <- as.data.frame(table(predict(decisionTree,nonBinaryTest,type='class'),nonBinaryTest$readmitted))



decisionTreeBinary <- rpart(formula =readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                            + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                            + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                            data = train,method='class')
summary(decisionTreeBinary)


predictorTreeBinary <- predict(decisionTreeBinary,test,type='class')


table(predict(decisionTreeBinary,test,type='class'),test$readmitted)

decisionTreeConfusionMatrixBinary <- confusionMatrix(predictorTreeBinary,test$readmitted)
decisionTreeConfusionMatrixBinary
plot(decisionTreeConfusionMatrixBinary$table)
prop.table(table(test$readmitted,predictorTreeBinary),1)

decisionTreeBinaryResult <- as.data.frame(table(predict(decisionTreeBinary,test,type='class'),test$readmitted))



############ Random Forest ##########

diabetesForest <- randomForest(formula = readmitted ~ age+discharged_to+time_in_hospital+
                                 num_lab_procedures+num_procedures+num_medications+number_outpatient+
                                 number_emergency+number_inpatient+number_diagnoses+
                                 insulin+change+diabetesMed+diag_group+diag_group2+diag_group3+A1Cresult,
                               data= nonBinaryTrain)

predictorRandomForest <- predict(diabetesForest,nonBinaryTest,type="response")

table(nonBinaryTest$readmitted,predictorRandomForest)

confusionMatrix(nonBinaryTest$readmitted,predictorRandomForest)


############ Nueral Network ############


diabetesNueralNet <- nnet(formula = readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                          + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                          + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                          data = nonBinaryTrain,size=10,maxit=100)


predictorNueralNet <- predict(diabetesNueralNet,nonBinaryTest,type="class")
table(nonBinaryTest$readmitted,predictorNueralNet)
prop.table(table(nonBinaryTest$readmitted,predictorNueralNet),1)


predictorNueralNet <- factor(predictorNueralNet)
nueralNetworkConfusionMatrix <- confusionMatrix(predictorNueralNet,nonBinaryTest$readmitted)
nueralNetworkConfusionMatrix





diabetesNueralNetBinary <- nnet(formula = readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                                + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                                + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                                data = train,size=10,maxit=100)

predictorNueralNetBinary <- predict(diabetesNueralNetBinary,test,type="raw")
predictorNueralNetBinary <- ifelse(predictorNueralNetBinary > 0.5,1,0)


resultNueralNetBinary <- as.data.frame(table(predictorNueralNetBinary,test$readmitted))
resultNueralNetBinary
correct <- resultNueralNetBinary[1,3]


accuracyNueralNet <- correct/nrow(test)
accuracyNueralNet



############ SVM ############

diabetesSVM <- svm(readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                   + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                   + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                   data = nonBinaryTrain, kernel = "linear")


############ Naive Bayes ############


diabetesNaiveBayes <- naiveBayes(readmitted ~ age + race + time_in_hospital + discharged_to  + num_procedures + num_lab_procedures 
                                 + number_outpatient + num_medications + number_inpatient + number_emergency +  insulin number_diagnoses
                                 + change + A1Cresult + diabetesMed + diag_group + diag_group2 + diag_group3,
                                 data= nonBinaryTrain)


predictorNaiveBayes <- predict(diabetesNaiveBayes,nonBinaryTest,type="class")


prop.table(table(nonBinaryTest$readmitted,predictorNaiveBayes),1)

predictorNaiveBayes <- factor(predictorNaiveBayes)
naiveBayesConfusionMatrix <- confusionMatrix(predictorNaiveBayes,nonBinaryTest$readmitted)
naiveBayesConfusionMatrix





































