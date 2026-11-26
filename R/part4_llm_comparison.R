source("R/00_setup.R")
source("R/part2_reduced_model.R")
source("R/part3_llm_classifier.R")
#add code below

#part iv
selected_passengers <- chosen
LLM_pred <- responses$llm_prediction
sum(new_clean$PassengerId[selected_passengers]==cases$PassengerId)==30

#actual survival outcomes for the passengers
outcomes <- new_clean$Survived[selected_passengers]
table(outcomes)

#logistic model prediction for the 30 passengers
survival_probability <- predict.glm(object=selected_model, newdata=new_clean[selected_passengers, ], type="response")
logistic_prediction <- 1*(survival_probability>0.5)

#confusion matrix for logistic model
confusion_matrix_logistic <- table(outcomes, logistic_prediction)
confusion_matrix_logistic
correct_died_logistic <- confusion_matrix_logistic[1,1]
wrong_survived_logistic <- confusion_matrix_logistic[1,2]
wrong_died_logistic <- confusion_matrix_logistic[2,1]
correct_survived_logistic <- confusion_matrix_logistic[2,2]
accuracy_logistic <- (correct_survived_logistic+correct_died_logistic)/(correct_survived_logistic+correct_died_logistic+wrong_survived_logistic+wrong_died_logistic)
sensitivity_logistic <- correct_survived_logistic/(correct_survived_logistic+wrong_died_logistic)
specificity_logistic <- correct_died_logistic/(correct_died_logistic+wrong_survived_logistic)

#confusion matrix for LLM
length(LLM_pred)==length(outcomes)
confusion_matrix_LLM <- table(outcomes, LLM_pred)
confusion_matrix_LLM
correct_died_LLM <- confusion_matrix_LLM[1,1]
wrong_survived_LLM <- confusion_matrix_LLM[1,2]
wrong_died_LLM <- confusion_matrix_LLM[2,1]
correct_survived_LLM <- confusion_matrix_LLM[2,2]
accuracy_LLM <- (correct_survived_LLM+correct_died_LLM)/(correct_survived_LLM+correct_died_LLM+wrong_survived_LLM+wrong_died_LLM)
sensitivity_LLM <- correct_survived_LLM/(correct_survived_LLM+wrong_died_LLM)
specificity_LLM <- correct_died_LLM/(correct_died_LLM+wrong_survived_LLM)

#comparing logistic model to LLM
classifiers_comparison <- data.frame(Measure=c("Accuracy", "Sensitivity", "Specificity"),
                         Logistic=round(c(accuracy_logistic, sensitivity_logistic, specificity_logistic), 3),
                         LLM=round(c(accuracy_LLM, sensitivity_LLM, specificity_LLM), 3))
classifiers_comparison

baseline_guess <- (correct_died_logistic+wrong_survived_logistic)/(correct_survived_logistic+correct_died_logistic+wrong_survived_logistic+wrong_died_logistic)
baseline_guess

sum(logistic_prediction==outcomes)
sum(LLM_pred==outcomes)

#frequency for the LLM and logistic model matching
table(logistic_prediction, LLM_pred)
sum(logistic_prediction==LLM_pred)/length(outcomes)

#prediction table
prediction_table <- data.frame(Pclass=new_clean$Pclass[selected_passengers],
                               Sex=new_clean$Sex[selected_passengers],
                               Age=new_clean$Age[selected_passengers],
                               SibSp=new_clean$SibSp[selected_passengers],
                               Fare=new_clean$Fare[selected_passengers],
                               Survived=outcomes,
                               survival_probability=round(survival_probability, 4),
                               logistic_prediction=logistic_prediction,
                               LLM_prediction=LLM_pred)
prediction_table
prediction_table[-which(logistic_prediction==LLM_pred),]

#accuracy of logistic model on the full test set
sum(1*(P_selected>0.5)==new_clean$Survived[test])/length(test)
