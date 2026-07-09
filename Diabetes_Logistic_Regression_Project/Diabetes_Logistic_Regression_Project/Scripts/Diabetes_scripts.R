columns_with_zero <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
for (col in columns_with_zero){
Diabetes[[col]][Diabetes[[col]] == 0] <- NA
}
for (col in columns_with_zero){
Diabetes[[col]][is.na(Diabetes[[col]])] <- mean(Diabetes[[col]], na.rm = TRUE)
}
summary(Diabetes)
Diabetes$Outcome <- as.factor(Diabetes$Outcome)
set.seed(123)
Train_index<- sample(1:nrow(Diabetes), 0.8*nrow(Diabetes))
Train_data<- Diabetes[Train_index,]
Test_data<- Diabetes[-Train_index,]
Model <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = Train_data, family = binomial)
print(Model)
Predictions<- predict(Model, newdata = Test_data, type = "response")
Class<- ifelse(Predictions > 0.4, 1, 0)
Comparism <- data.frame(Actual =Test_data$Outcome, predicted = Class )
print(Comparism)
table(predicted = Class, Actual =Test_data$Outcome) 
Confusion_matrix<- table(predicted = Class, Actual =Test_data$Outcome)
True_positive<- Confusion_matrix[2,2]
True_negative<- Confusion_matrix[1,1]
False_positive<- Confusion_matrix[2,1]
False_negative<- Confusion_matrix[1,2]
Model_accuracy<- (True_positive + True_negative)/sum(Confusion_matrix)
print(Model_accuracy)
Precision<- (True_positive)/ (True_positive + False_positive)
print(Precision)  
dir.create("Model")
saveRDS(Model,"Model/logistic_model.rds")
loaded_model<- readRDS("Model/logistic_model.rds")
print(loaded_model)

