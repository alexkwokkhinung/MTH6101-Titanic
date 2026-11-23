source("R/00_setup.R")
source("R/part1_cleaning_eda_full_model.R")
#add code below
# PART (ii)

M2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare,
          data = new_clean[training, ],
          family = "binomial")

summary(M2)

P_train_full <- predict.glm(M1,
                            newdata = new_clean[training, ],
                            type = "response")

P_train_reduced <- predict.glm(M2,
                               newdata = new_clean[training, ],
                               type = "response")

roc_full_train <- roc(response = new_clean$Survived[training],
                      predictor = P_train_full)

roc_reduced_train <- roc(response = new_clean$Survived[training],
                         predictor = P_train_reduced)

auc(roc_full_train)
auc(roc_reduced_train)

plot(roc_full_train,
     col = "blue",
     main = "Training ROC: Full vs Reduced")

plot(roc_reduced_train,
     col = "red",
     add = TRUE)

legend("bottomright",
       legend = c("Full model", "Reduced model"),
       col = c("blue", "red"),
       lwd = 2)
P_selected <- predict.glm(selected_model,
                          newdata = new_clean[test, ],
                          type = "response")

roc_selected <- roc(response = new_clean$Survived[test],
                    predictor = P_selected)

plot(roc_selected,
     main = "ROC Curve: Selected Logistic Model")

auc(roc_selected)
summary(selected_model)

exp(selected_model$coefficients)
