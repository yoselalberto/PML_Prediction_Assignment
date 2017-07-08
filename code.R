

# configuración -----------------------------------------------------------

library(magrittr)
library(tidyverse)
library(janitor)
library(caret)

# cargado -----------------------------------------------------------------

training_inicio <- read_csv("Datos/pml-training.csv")[, -1]
testing_inicio  <- read_csv("Datos/pml-testing.csv")[, -1]


# preprocesado ------------------------------------------------------------

# elimino columnas vacias
testing <- testing_inicio %>% remove_empty_cols() %>% select(-new_window)
# conservo variables de testing
variables <- intersect(names(training_inicio), names(testing))
training <- training_inicio[, c(variables, "classe")] %>% 
    filter(new_window == "no") %>% select(-new_window) %>% 
    na.omit()


# spliting ----------------------------------------------------------------

set.seed(1642)
in_train <- createDataPartition(training$classe, p = 0.75, list = FALSE)    
training_build      <- training[in_train, ]
training_validation <- training[-in_train, ]


# training ----------------------------------------------------------------

# random forest
set.seed(142)
model_rf    <- train(classe ~ ., data = training_build, method = "rf",
                  preProcess = c("center", "scale"), 
                  trControl = trainControl(method = "cv", number = 3))
# rpart
set.seed(9879)
model_rpart <- train(classe ~ ., data = training_build, method = "rpart",
                         preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 3))
# gradient boosting machine
set.seed(124)
model_gbm   <- train(classe ~ ., data = training_build, method = "gbm", 
                   preProcess = c("center", "scale"), verbose = FALSE,
                   trControl = trainControl(method = "cv", number = 3))
# salvado de modelos
write_rds(model_rf, "Models/model_rf.rds")
write_rds(model_gbm, "Models/model_gmb.rds")
write_rds(model_rpart, "Models/model_rpart.rds")

# predict -----------------------------------------------------------------

set.seed(425)
pred_rf <- predict(model_rf, training_validation) %>% as.character()
pred_gbm <- predict(model_gbm, training_validation) %>% as.character()
pred_rpart <- predict(model_rpart, training_validation) %>% as.character()


# evaluation --------------------------------------------------------------

# separados
sum(training_validation$classe == pred_rf) / length(pred_rf)
sum(training_validation$classe == pred_gbm) / length(pred_gbm)
sum(training_validation$classe == pred_rpart) / length(pred_rpart)
# mayority vote



# prediccion en testing ---------------------------------------------------

pred_rf_testing <- predict(model_rf, select(testing, -problem_id)) %>% as.character()
# tibble
tibble(id= testing$problem_id, pred = pred_rf_testing)


