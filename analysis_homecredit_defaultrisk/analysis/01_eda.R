
library(here)
library(tidyverse)
library(magrittr)
library(caret)
library(xgboost)
#library(doParallel)

set.seed(12345)
# setup parallel cluster
#cl <- makeCluster(detectCores() - 1)
#registerDoParallel(cl)


df <- read_csv(here::here('data/application_train.csv'))

df_bureau <- read_csv(here::here('data/bureau_balance.csv'))
df_bureau <- read_csv(here::here('data/bureau.csv'))
df_bureau <- read_csv(here::here('data/credit_card_balance.csv'))
df_bureau <- read_csv(here::here('data/installments_payments.csv'))
df_bureau <- read_csv(here::here('data/POS_CASH_balance.csv'))
df_bureau <- read_csv(here::here('data/previous_application.csv'))

read_csv(here::here('data/HomeCredit_columns_description.csv'))


# Pre-processing ---------------------------------------------------------------

prep_data <- function(df_train, df_test) {
  
  df_train %<>% select(-SK_ID_CURR)
  df_test %<>% select(-SK_ID_CURR)
  
  # create dummy variables
  fit_dummies <- dummyVars(TARGET ~ ., data = df_train)
  x_train <- predict(fit_dummies, newdata = df_train)
  x_test <- predict(fit_dummies, newdata = df_test)
  
  # impute missing values
  fit_impute <- preProcess(x_train, method = 'medianImpute')
  x_train <- predict(fit_impute, newdata = x_train)
  x_test <- predict(fit_impute, newdata = x_test)
  
  return(list('train' = x_train, 'test' = x_test))
}

X <- prep_data(df, df)
y <- df$TARGET %>%
  as.character %>%
  as.factor %>%
  fct_recode(good = '0', bad = '1')


# Modelling --------------------------------------------------------------------

# candidate hyperparameters
xgb_grid <- expand.grid(
  nrounds = c(100,300),
  max_depth = 8,#c(4, 8, 12),
  eta = c(0.01, 0.001),#, 0.0001),
  gamma = 1,
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 0.5)
# cross-validation   settings
xgb_control <- trainControl(
  summaryFunction = twoClassSummary,
  method = 'repeatedcv',
  number = 5,
  classProbs = TRUE,
  allowParallel = TRUE)
# fit model
xgb_model <- train(
  x = X$train,
  y = y,
  trControl = xgb_control,
  #tuneGrid = xgb_grid,
  metric = 'ROC',
  method = 'xgbTree')

#Stop parallel cluster
stopCluster(cl)





# Nice ROC curves
library(caret)
library(mlbench)
data(Sonar)
ctrl <- trainControl(method="cv", 
                     summaryFunction=twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)
rfFit <- train(Class ~ ., data=Sonar, 
               method="rf", preProc=c("center", "scale"), 
               trControl=ctrl)
library(pROC)
# Select a parameter setting
selectedIndices <- rfFit$pred$mtry == 2
# Plot:
plot.roc(rfFit$pred$obs[selectedIndices],
         rfFit$pred$M[selectedIndices])
