library(h2o)
library(caret)
load("~/Desktop/MIT/Recerca/NitUP/Project/v2/General_DB_GapFilled_17feb23.Rdata")
colnames(General_DB)

db <- General_DB
var <- c(42:46)

#### 0. Intro ####

# Real life nice story
# What is upscaling? What do we use it for?
# What is a machine learning model
# Idea of general pipeline to follow

#### 1. Subset creation ####
parts = createDataPartition(db$Net_Nuptake_Combi, p= .9, list=F) #it changes  every time. (set.seed)
train_test=db[parts,]
validation = db[-parts,]

ratio <- nrow(db)/5 #20% aprox
start <- db[,var]

selectId <- maxDissim(start, start, obj = minDiss, n = ratio)

dissimilar <- db[selectId, ] #train
non_dissimilar <- db[-selectId, ] #test

parts <- sample(1:dim(non_dissimilar)[1], ratio)

train = non_dissimilar[-parts, ]
train <- rbind(dissimilar,train)
test = db[parts, ]

#### 2. Select the best ML model ####
library(h2o)
names <- colnames(db)[c(42:46)]

h2o.init()

train_conv_h2o <- as.h2o(train) # 70% train
test_conv_h2o  <- as.h2o(test) # 20%
valid_conv_h2o <- as.h2o(validation)

# train_conv_h2o <- h2o.assign(train, "train" ) # 70%
# test_conv_h2o  <- h2o.assign(test, "test" ) # 20%
# valid_conv_h2o <- h2o.assign(validation, "valid" ) # 10%
hist(db$Net_Nuptake_Combi)
selection <- h2o.automl(names , "Net_Nuptake_Combi", training_frame = train_conv_h2o,
                        leaderboard_frame = valid_conv_h2o, distribution = "gamma")

save(selection, file="~/Desktop/MIT/Classes/selection.RData")
load("~/Desktop/MIT/Classes/ML and upscaling/selection.RData")
model <- h2o.getModel("XGBoost_grid_1_AutoML_1_20230509_95359_model_1287")

rasterFromXYZ()
#### 3. XGBoost ####
library(xgboost)
colnames(train)
train_x = data.matrix(train[, var])
train_y = train[,41]
test_x = data.matrix(test[, var])
test_y = test[, 41]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 5, verbose=0,
                  watchlist=watchlist, nrounds = 150, objective="reg:gamma",
                  min_child_weight=1, eta=0.3)

min <- min(model$evaluation_log$test_gamma_nloglik)
min_iter <- model$evaluation_log[which(model$evaluation_log$test_gamma_nloglik==min),]
number <- min_iter$iter[[1]]

model_xgboost = xgboost(data = xgb_train, max.depth = 5, nrounds = number, verbose = 0, missing=NA,
                        objective="reg:gamma", min_child_weight=1, eta=0.3)

# set hyper parameters automatically

# https://www.drdataking.com/post/hyperparameters-tuning-for-xgboost-using-bayesian-optimization/
# https://www.kaggle.com/code/prashant111/a-guide-on-xgboost-hyperparameters-tuning

# model evaluation
pred_y = predict(model_xgboost, xgb_test)

y_test_mean = mean(test_y)
tss =  sum((test_y - y_test_mean)^2 )
rss =  sum((pred_y - test_y)^2)

performance <- c(mean((test_y - pred_y)^2),caret::RMSE(test_y, pred_y), 1 - (rss/tss))
names(performance) <- c("mse", "rmse", "r2")

importance_matrix = xgb.importance(colnames(xgb_train), model = model_xgboost)
xgb.plot.importance(importance_matrix[1:5,]) # What if  I  want to include  interactions?

#### 4. Random Forest ####
library(randomForest)

rf <- randomForest(N ~ .,data=train, importance=TRUE, ntree=100)
plot(rf)
importance(rf)
varImpPlot(rf, conditional = TRUE)
getTree(rf)

#### 5. Function example ####

func_boost <- function(db,iterations=5, variables, cont_var,
                       max.depth=4, min_child_weight=0,eta=0.3){
  performance <- as.data.frame(matrix(ncol=3, nrow=iterations))
  colnames(performance) <- c("mse", "rmse", "r2")
  imp_matrix <- data.frame()
  var_behavior <- data.frame()
  models_list <- vector("list", iterations)
  training_data <- vector("list", iterations)
  validation_data <- vector("list", iterations)
  for ( i in 1:iterations) {
    parts = createDataPartition(db$Net_Nuptake_Combi, p= .9, list=F)
    data=db[parts,]
    validation = db[-parts,]
    
    validation_x = data.matrix(validation[, variables])
    validation_y = validation[,41]
    
    partió <- nrow(db)/5
    start <- db[,cont_var]
    selectId <- maxDissim(start, start, obj = minDiss, n = partió)
    
    dissimilar <- db[selectId, ]
    non_dissimilar <- db[-selectId, ]
    
    parts <- sample(1:dim(non_dissimilar)[1], partió)
    
    train = non_dissimilar[-parts, ]
    train <- rbind(dissimilar,train)
    test = db[parts, ]
    
    # parts = createDataPartition(db$Net_Nuptake_Combi, p = .8, list = F, groups=5)
    # train = db[parts, ]
    # test = db[-parts, ]
    
    train_x = data.matrix(train[, variables])
    train_y = train[,41]
    test_x = data.matrix(test[, variables])
    test_y = test[, 41]
    
    xgb_train = xgb.DMatrix(data = train_x, label = train_y)
    xgb_test = xgb.DMatrix(data = test_x, label = test_y)
    
    watchlist = list(train=xgb_train, test=xgb_test)
    
    model = xgb.train(data = xgb_train, max.depth = max.depth, verbose=0,
                      watchlist=watchlist, nrounds = 150,objective="reg:gamma",
                      min_child_weight=min_child_weight, eta=eta)
    min <- min(model$evaluation_log$test_gamma_nloglik)
    min_iter <- model$evaluation_log[which(model$evaluation_log$test_gamma_nloglik==min),]
    number <- min_iter$iter[[1]]
    model_xgboost = xgboost(data = xgb_train, max.depth = max.depth, nrounds = number, verbose = 0, missing=NA,
                            objective="reg:gamma", min_child_weight=min_child_weight, eta=eta)
    pred_y = predict(model_xgboost, xgb_test)
    y_test_mean = mean(test_y)
    tss =  sum((test_y - y_test_mean)^2 )
    rss =  sum((pred_y - test_y)^2)
    
    performance[i,] <- c(mean((test_y - pred_y)^2),caret::RMSE(test_y, pred_y), 1 - (rss/tss))
    importance_matrix = xgb.importance(colnames(xgb_train), model = model_xgboost)
    imp_matrix <- rbind(imp_matrix, importance_matrix)
    models_list[[i]] <- model_xgboost
    training_data[[i]] <- train_x
    validation_data[[i]] <- cbind(validation$Latitude, validation$Longitude, validation_y, validation_x)
  }
  total_performance <- colMeans(performance)
  names(total_performance) <- c("mse", "rmse", "r2")
  importance_matrix <- aggregate(imp_matrix, list(imp_matrix$Feature), FUN=mean)
  importance_matrix <- importance_matrix[,-2]
  colnames(importance_matrix)[1] <- "Feature"
  importance_matrix <- setDT(importance_matrix)
  return(list(total_performance, performance, importance_matrix,
              models_list, training_data, validation_data))
}
library(xgboost)
library(data.table)
interaccions_model <- func_boost(db=General_DB, iterations = 10,
                                 variables=c(42:46),
                                 max.depth=4, min_child_weight=1, eta=0.30,
                                 cont_var=c(42:46))
interaccions_model[[5]]
xgb.plot.importance(interaccions_model[[3]]) # What if  I  want to include  interactions?

#### 6. Upscaling ####

pred_y = predict(model_xgboost, xgb_test) # xgb_test is a df = pred_y is df
# replace xgb_test with a stack or brick with same variable names

################
load("~/Desktop/ML_pred_df.Rdata")
load("~/Desktop/CNr_map.Rdata")

CNr_df <- as.data.frame(CNr, xy=T)
CNr_df$layer <- ML_pred_df

ML_pred_map <- rasterFromXYZ(CNr_df)
Final_map <- mask(ML_pred_map, CNr)
plot(Final_map)
