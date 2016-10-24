## 0: SETUP
## ========

# Set seed
set.seed(42)

# Dependencies
library(data.table)
library(Matrix)
library(caret)
library(xgboost)
library(matrixStats)

# Define file paths
date_csv <- "train_date.csv"
numeric_csv <- "train_numeric.csv"
test_date_csv <- "test_date.csv"
test_numeric_csv <- "test_numeric.csv"

# Parameters for Faron computation
number_of_chunks <- 20
n_train <- 1183747
n_test <- 1183748

# Features
numeric_features <- c('Id',
                      'L1_S24_F1604',
                      'L1_S24_F1695', 
                      'L1_S24_F1846', 
                      'L3_S29_F3407', 
                      'L3_S32_F3850',
                      'L3_S33_F3855', 
                      'L3_S33_F3865')
date_features <- c('Id',
                   'L3_S30_D3496', 
                   'L3_S30_D3501',
                   'L3_S30_D3506', 
                   'L3_S32_D3852', 
                   'L3_S33_D3856')


## 1: EXTRACT MIN DATE
## ===================

# TRAIN
print("Extract min date: TRAIN")

dt_final <- NA
dt_names <- names(fread(date_csv,
                        nrows = 1))
dt <- fread(date_csv,
            select = c("Id"))

# Loop through chunks
chunk_size <- floor(n_train / number_of_chunks)
for (chunk_number in 0:number_of_chunks) {
  
  # read in a chunk
  dt_chunk <- fread(date_csv,
                    skip = (chunk_size * chunk_number + 1),
                    nrows = chunk_size)
  setnames(dt_chunk, dt_names)
  
  # get stats
  # Edit: fixed minDate including Id column; thanks to r4sn4
  dt_dateorder <- data.table(Id = dt_chunk$Id, 
                             minDate = rowMins(as.matrix(dt_chunk[, -"Id", with=F]), na.rm = T))
  
  # concat
  if(is.na(dt_final)){
    dt_final <- dt_dateorder
  } else {
    dt_final <- rbind(dt_final, dt_dateorder)
  }
  
}


# TEST
print("Extract min date: TEST")

dt_final_test <- NA
dt_names <- names(fread(test_date_csv,
                        nrows = 1))
dt <- fread(test_date_csv,
            select = c("Id"))

# Loop through chunks
chunk_size <- floor(n_test / number_of_chunks)
for (chunk_number in 0:number_of_chunks){
  
  # read in a chunk
  dt_chunk <- fread(test_date_csv,
                    skip = (chunk_size * chunk_number + 1),
                    nrows = chunk_size)
  setnames(dt_chunk, dt_names)
  
  # get stats
  dt_dateorder <- data.table(Id = dt_chunk$Id, 
                             minDate = rowMins(as.matrix(dt_chunk[, -"Id", with=F]), na.rm = T))
  
  # concat
  if(is.na(dt_final_test)){
    dt_final_test <- dt_dateorder
  } else {
    dt_final_test <- rbind(dt_final_test, dt_dateorder)
  }
  
}


## 2: FARON
## ========

print("Compute Faron features")

dt <- rbind(dt_final, dt_final_test)
rm(dt_final)
rm(dt_final_test)

ord_start_id <- order(dt$minDate, dt$Id)
dt$diffFromStartId <- rep(NA, length(ord_start_id))
dt$diffFromStartId[ord_start_id] <- c(NA, diff(dt$Id[ord_start_id]))
dt$diffUntilStartId <- rep(NA, length(ord_start_id))
dt$diffUntilStartId[ord_start_id] <- c(diff(-dt$Id[ord_start_id]), NA)

dt_faron_train <- dt[1:n_train,]
dt_faron_test <- dt[(n_train+1):nrow(dt),]
rm(dt)


## 3: MATTHEWS
## ===========

print("Load methods for computing MCC")

# load matthews coefficient methods
mcc <- function(y_true, y_prob) {
  DT <- data.table(y_true = y_true, y_prob = y_prob, key="y_prob")
  
  nump <- sum(y_true)
  numn <- length(y_true)- nump
  
  DT[, tn_v:= cumsum(as.numeric(y_true == 0))]
  DT[, fp_v:= cumsum(as.numeric(y_true == 1))]
  DT[, fn_v:= numn - tn_v]
  DT[, tp_v:= nump - fp_v]
  DT[, tp_v:= nump - fp_v]
  DT[, mcc_v:= (tp_v * tn_v - fp_v * fn_v) / sqrt((tp_v + fp_v) * (tp_v + fn_v) * (tn_v + fp_v) * (tn_v + fn_v))]
  DT[, mcc_v:= ifelse(!is.finite(mcc_v), 0, mcc_v)]
  
  return(max(DT[['mcc_v']]))
}
mcc_eval <- function(y_prob, dtrain) {
  y_true <- getinfo(dtrain, "label")
  best_mcc <- mcc(y_true, y_prob)
  return(list(metric="MCC", value=best_mcc))
}

## 4: MODEL
## ========

# TRAIN
print("Train model")

dt_date <- fread(date_csv, select = c(date_features))
dt <- fread(numeric_csv, select = c(numeric_features, "Response"))
Id <- dt$Id
Y <- dt$Response
dt[ , Id := NULL]
dt[ , Response := NULL]
dt_date[ , Id := NULL]

# clean numerics
for(col in names(dt)) {
  col_mean <- mean(dt[[col]], na.rm = TRUE)
  set(dt, which(is.na(dt[[col]])), col, col_mean)
}

# clean date
for(col in names(dt_date)) {
  col_mean <- mean(dt_date[[col]], na.rm = TRUE)
  set(dt_date, which(is.na(dt_date[[col]])), col, col_mean)
}

# combine
dt <- cbind(dt, dt_date, dt_faron_train)
rm(dt_date)
rm(dt_faron_train)

# sparsify
X <- Matrix(as.matrix(dt), sparse = T)
rm(dt)

# train cross-validation models
n_folds <- 5
folds <- createFolds(as.factor(Y), k = n_folds)
valid <- folds[[1]]
model <- c(1:length(Y))[-valid]

param <- list(objective = "binary:logistic", 
              eta = 0.01, 
              max_depth = 4,
              min_child_weight = 3,
              subsample = 0.8,
              colsample_bytree = 0.8,
              base_score = 0.005,
              eval_metric = mcc_eval,
              maximize = TRUE)

dmodel <- xgb.DMatrix(X[model,], label = Y[model])
dvalid <- xgb.DMatrix(X[valid,], label = Y[valid])

# generate model for fold    
xgb_model <- xgb.train(data = dmodel, param, nrounds = 20,
                       watchlist = list(mod = dmodel, val = dvalid))

# get threshold for best mcc
pred <- predict(xgb_model, dvalid)
matt <- data.table(thresh = seq(0.99, 0.9999, by = 0.0001))
matt$scores <- sapply(matt$thresh, FUN = function(x) mcc(Y[valid], (pred > quantile(pred, x)) * 1))
threshold <- matt$thresh[which(matt$scores == max(matt$scores))]

rm(X)

# TEST
print("Run model on test set")

dt_date <- fread(test_date_csv, select = c(date_features))
dt <- fread(test_numeric_csv, select = c(numeric_features))
Id <- dt$Id
dt[ , Id := NULL]
dt_date[ , Id := NULL]

# clean numerics
for(col in names(dt)) {
  col_mean <- mean(dt[[col]], na.rm = TRUE)
  set(dt, which(is.na(dt[[col]])), col, col_mean)
}

# clean date
for(col in names(dt_date)) {
  col_mean <- mean(dt_date[[col]], na.rm = TRUE)
  set(dt_date, which(is.na(dt_date[[col]])), col, col_mean)
}

# combine
dt <- cbind(dt, dt_date, dt_faron_test)
rm(dt_date)
rm(dt_faron_test)

# sparsify
X <- Matrix(as.matrix(dt), sparse = T)
rm(dt)


## 5: PREDICT
## ==========

pred <- predict(xgb_model, X)
predicted_response <- (pred > quantile(pred, threshold)) * 1
dt <- data.table(Id = Id, Response = predicted_response)


## 6: STORE
## ========
print("Store prediction")

write.csv(dt, "submission.csv", row.names = F)