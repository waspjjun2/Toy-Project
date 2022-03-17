

als <- read.table("http://web.stanford.edu/~hastie/CASI_files/DATA/ALS.txt",header=TRUE)

str(als)
table(als$testset)


# split train, test set
train.ind <- as.logical(1 - als$testset)
test.ind <- als$testset 
train <- als[train.ind,]
test <- als[test.ind,]

# remove train, test index
train <- train[,-1]
test <- test[,-1]

test.x <- test[,-1]
test.y <- test[,1] ## y : dFRS

str(train) # 1197 obs. of  370 variables
str(test.x) # 625 obs. of  369 variables





## randomforest : ntree = 300
# install.packages('randomForest')
# install.packages('ranger')
library(ranger)
library(randomForest)
?ranger
#####################
num_features <- dim(train.x)[2]
rf.fit <- ranger(dFRS ~ ., data=train,
                 num.trees=500,
                 mtry = round((num_features/3),0),
                 min.node.size = 4)

rf.pred <- predict(rf.fit, test.x)$predictions
MSPE <- sum((test.y - rf.pred)^2) / dim(test)[1]
MSPE
##################

?ranger

num_features <- dim(train)[2]
library(ranger)
library(randomForest)

rf.grid <- expand.grid(
  num.trees = c(200,300,400,500),
  mtry = floor(num_features * c(.25, .333, .4)),
  min.node.size = c(2, 3, 5, 10),
  mspe = NA,
  se.of.mspe = NA)

rf.grid

B = 5
MSPE.box <- matrix(NA, nrow = B, ncol = nrow(rf.grid))

for (j in 1:B){
  
  set.seed(j) # for stable result
  
  for(i in seq_len(nrow(rf.grid))) {
    
    rf.fit <- ranger(
      formula         = dFRS ~ ., 
      data            = train, 
      num.trees       = rf.grid$num.trees[i],
      mtry            = rf.grid$mtry[i],
      min.node.size   = rf.grid$min.node.size[i],
      verbose         = FALSE
    )
    
    rf.pred <- predict(rf.fit, test.x)$predictions
    MSPE.box[j,i] <- sum((test.y - rf.pred)^2) / dim(test)[1]
    
    print(paste(j, 'rep ', i, 'th completed!'))
    
  }
  
}

# warnings()
MSPE.box
rf.MSPE <- apply(MSPE.box, 2, mean)
rf.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)

# extract statistics
rf.grid$mspe <- round(rf.MSPE,4)
rf.grid$se.of.mspe <- round(rf.se.of.mspe, 6)

# assess top 10 models
library(dplyr)
table1 <- rf.grid %>%arrange(mspe) %>% head(20)
table1
# install.packages('xtable')
library(xtable)

# ?xtable
table1 <- xtable(table1, digits = c(0,0,0,0,4,6),
       caption = 'Random Forest hyperparmeter tuning')

print(table1, scalebox=0.7, caption.placement = "top")

# importance plot

rf.fit <- randomForest(dFRS ~ ., train,
                       ntree = 500, mtry = 148, nodesize=3)

# make dataframe from importance() output
library(dplyr)
feat_imp_df <- importance(rf.fit) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

?arrange
feat_imp_df2 <- feat_imp_df %>% arrange(desc(IncNodePurity)) %>% head(20)

# plot dataframe
library(ggplot2)
ggplot(feat_imp_df2, aes(x = reorder(feature, IncNodePurity), 
                        y = IncNodePurity)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Random Forest>"
  )








## boosting : depth d = 4 and e = 0.02
## HP : n.trees, interaction.depth, n.minobsinnode,
## shrinkage(0.001~0.1) -> shirinkage 줄면 ntree up 경향
# bag.fraction = 0.5 (default!), distribution : "gaussian"으로 설정

library(gbm)
?gbm
#### choose shrinkage of gbm

gbm.lr.grid <- expand.grid(
  shrinkage = c(0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1),
  mspe = NA,
  se.of.mspe = NA
)


B = 5
MSPE.box <- matrix(NA, nrow = B, ncol = nrow(gbm.lr.grid))

for (j in 1:B){
  
  set.seed(j) # for stable result
  
  for(i in seq_len(nrow(gbm.lr.grid))) {
    
    gbm.fit <- gbm(
      formula         = dFRS ~ ., 
      data            = train,
      distribution = "gaussian",
      n.trees = 500, 
      shrinkage = gbm.lr.grid$shrinkage[i], 
      interaction.depth = 4
    )
    
    gbm.pred <- predict(gbm.fit, test.x)
    MSPE.box[j,i] <- sum((test.y - gbm.pred)^2) / dim(test)[1]
    
    print(paste(j, 'rep ', i, 'th completed!'))
    
  }
  
}

MSPE.box
gbm.MSPE <- apply(MSPE.box, 2, mean)
gbm.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)


# extract statistics
gbm.lr.grid$mspe <- round(gbm.MSPE,4)
gbm.lr.grid$se.of.mspe <- round(gbm.se.of.mspe, 6)

# assess top 10 models
library(dplyr)
gbm.lr.grid %>%
  arrange(mspe) %>% 
  head(10)


#### tuning other parameters

gbm.grid <- expand.grid(
  n.trees = c(200, 300, 400, 500),
  interaction.depth = c(1,4,7),
  n.minobsinnode = c(5, 10, 15), 
  mspe = NA,
  se.of.mspe = NA
)

B = 5
MSPE.box <- matrix(NA, nrow = B, ncol = nrow(gbm.grid))

for (j in 1:B){
  
  set.seed(j) # for stable result
  
  for(i in seq_len(nrow(gbm.grid))) {
    
    gbm.fit <- gbm(
      formula         = dFRS ~ ., 
      data            = train,
      distribution = "gaussian",
      n.trees = gbm.grid$n.trees[i], 
      shrinkage = 0.01, ## fixed
      interaction.depth = gbm.grid$interaction.depth[i],
      n.minobsinnode = gbm.grid$n.minobsinnode[i]
    )
    
    gbm.pred <- predict(gbm.fit, test.x)
    MSPE.box[j,i] <- sum((test.y - gbm.pred)^2) / dim(test)[1]
    
    print(paste(j, 'rep ', i, 'th completed!'))
    
  }
  
}

MSPE.box
gbm.MSPE <- apply(MSPE.box, 2, mean)
gbm.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)

# extract statistics
gbm.grid$mspe <- round(gbm.MSPE,4)
gbm.grid$se.of.mspe <- round(gbm.se.of.mspe, 6)

# assess top 10 models
library(dplyr)
table2 <- gbm.grid %>% arrange(mspe) %>%  head(20)

table2 <- xtable(table2, digits = c(0,0,0,0,4,6),
                 caption = 'GBM hyperparmeter tuning')

print(table2, scalebox=0.7, caption.placement = "top")

?gbm

# importance plot

gbm.fit <- gbm(
  formula         = dFRS ~ ., 
  data            = train,
  distribution = "gaussian",
  n.trees = 500, 
  shrinkage = 0.01, ## fixed
  interaction.depth = 7,
  n.minobsinnode = 15
)
head(summary.gbm(gbm.fit, plotit=FALSE))

# make dataframe from importance() output
library(dplyr)
feat_imp_df <- summary.gbm(gbm.fit, plotit=FALSE) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

head(feat_imp_df)
?arrange
feat_imp_df2 <- feat_imp_df %>% arrange(desc(rel.inf)) %>% head(20)

# plot dataframe
library(ggplot2)
ggplot(feat_imp_df2, aes(x = reorder(feature, rel.inf), 
                         y = rel.inf)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: < GBM >"
  )





### my own model : xgboost

als <- read.table("http://web.stanford.edu/~hastie/CASI_files/DATA/ALS.txt",header=TRUE)

str(als)
table(als$testset)

# split train, test set
train.ind <- as.logical(1 - als$testset)
test.ind <- als$testset 
train <- als[train.ind,]
test <- als[test.ind,]

# remove train, test index
train <- train[,-1]
test <- test[,-1]

test.x <- test[,-1]
test.y <- test[,1] ## y : dFRS

str(train) # 1197 obs. of  370 variables
str(test.x) # 625 obs. of  369 variables

# library(h2o)      # for a java-based implementation of GBM variants
library(xgboost)  # for fitting extreme gradient boosting
?xgboost

library(recipes)
xgb_prep <- recipe(dFRS ~ ., data = train) %>%
  step_integer(all_nominal()) %>%
  prep(training = train, retain = TRUE) %>%
  juice()
xgb_prep
X <- as.matrix(xgb_prep[setdiff(names(xgb_prep), "dFRS")])
Y <- xgb_prep$dFRS

# ##################
set.seed(2021) ##
als_xgb <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 5000,
  objective = "reg:squarederror",
  early_stopping_rounds = 100,
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)

?xgb.train
xgb.train
xgb.fit()
# minimum test CV RMSE : 0.248806
min_MSPE <- min(als_xgb$evaluation_log$test_rmse_mean)^2
min_MSPE

####################
str(train)
xg.train = xgb.DMatrix(as.matrix(sapply(train, as.numeric)), label=train[,1])

xgb.fit <- xgb.train(
  xg.train,
  nrounds = 5000,
  objective = "reg:squarederror",
  early_stopping_rounds = 100, 
  verbose = 0,
  
  )

xgb.pred <- predict(xgb.fit, as.matrix(sapply(test.x, as.numeric)))
sum((test.y - xgb.pred)^2) / dim(test)[1]

test.y
xgb.pred

####################################

### tuning lr

?xgb.train

xgb.train = xgb.DMatrix(as.matrix(sapply(train, as.numeric)), label=train[,1])

hyper_grid <- expand.grid(
  eta = c(0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1),
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, ## fixed
  colsample_bytree = 0.5, ## fixed
  mspe = NA,
  se.of.mspe = NA,
  trees = NA  # a place to dump required number of trees
)

B = 5
MSPE.box <- matrix(NA, nrow = B, ncol = nrow(hyper_grid))

for (j in 1:B){
  
  set.seed(j) # for stable result
  
  for(i in seq_len(nrow(hyper_grid))) {
    
    xgb.fit <- xgboost(
      xgb.train,
      nrounds = 5000, ##
      objective = "reg:squarederror",
      early_stopping_rounds = 100,
      verbose = 0,
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth,
      min_child_weight = hyper_grid$min_child_weight,
      subsample = hyper_grid$subsample,
      colsample_bytree = hyper_grid$colsample_bytree
      
    )
    
    xgb.pred <- predict(xgb.fit, as.matrix(sapply(test, as.numeric)))
    
    # record result
    hyper_grid$trees[i] <- xgb.fit$best_iteration
    MSPE.box[j,i] <- sum((test.y - xgb.pred)^2) / dim(test)[1]
    
    print(paste(j, 'rep ', i, 'th completed!'))
    
  }

}

MSPE.box
xgb.MSPE <- apply(MSPE.box, 2, mean)
xgb.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)

# extract statistics
hyper_grid$mspe <- round(xgb.MSPE,4)
hyper_grid$se.of.mspe <- round(xgb.se.of.mspe, 6)

# assess top 10 models
library(dplyr)
hyper_grid %>%
  arrange(mspe)


### tuning tree-specific hyperparameter


hyper_grid <- expand.grid(
  eta = 0.02, ## fixed
  max_depth = seq(3,9,2), 
  min_child_weight = seq(1,5,2),
  subsample = 0.5, ## fixed
  colsample_bytree = 0.5, ## fixed
  mspe = NA,
  se.of.mspe = NA,
  trees = NA  # a place to dump required number of trees
)

B = 5
MSPE.box <- matrix(NA, nrow = B, ncol = nrow(hyper_grid))

for (j in 1:B){
  
  set.seed(j) # for stable result
  
  for(i in seq_len(nrow(hyper_grid))) {
    
    xgb.fit <- xgboost(
      xgb.train,
      nrounds = 5000, ##
      objective = "reg:squarederror",
      early_stopping_rounds = 100, 
      verbose = 0,
      eta = hyper_grid$eta, 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample, 
      colsample_bytree = hyper_grid$colsample_bytree
      
    )
    
    xgb.pred <- predict(xgb.fit, as.matrix(sapply(test, as.numeric)))
    
    # record result
    hyper_grid$trees[i] <- xgb.fit$best_iteration
    MSPE.box[j,i] <- sum((test.y - xgb.pred)^2) / dim(test)[1]
    
    print(paste(j, 'rep ', i, 'th completed!'))
    
  }
  
}

MSPE.box
xgb.MSPE <- apply(MSPE.box, 2, mean)
xgb.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)

# extract statistics
hyper_grid$mspe <- round(xgb.MSPE,4)
hyper_grid$se.of.mspe <- round(xgb.se.of.mspe, 6)

# assess top 10 models
library(dplyr)
hyper_grid %>%
  arrange(mspe) %>% 
  head(10)


### tuning regularization hyperparameters

hyper_grid <- expand.grid(
  eta = 0.01, ## fixed
  max_depth = 3, ## fixed
  min_child_weight = 5, ## fixed
  subsample = 0.5, ## fixed
  colsample_bytree = 0.5, ## fixed
  gamma = c(0, 1, 10),
  lambda = c(0, 1e-2, 0.1, 1, 100),
  alpha = c(0, 1e-2, 0.1, 1, 100),
  mspe = NA,
  se.of.mspe = NA,
  # trees = NA  # a place to dump required number of trees
)

B = 5
# MSPE.box <- matrix(NA, nrow = B, ncol = nrow(hyper_grid))
MSPE.box <- c()

# parallel computation
library(foreach)
n.cores <- parallel::detectCores()-1
n.cores
# 클러스터 초기화
party.cluster <- parallel::makeCluster(n.cores)
doParallel::registerDoParallel(party.cluster)
# 클러스터에 전역 변수 추가
parallel::clusterExport(party.cluster,
                        varlist = c('xgb.train', 'test', 'test.y', 'hyper_grid', 'MSPE.box'))

                   
foreach.MSPE.box <- foreach::foreach(j = 1:5, .combine = rbind,
                                .packages = c("xgboost"), .inorder = TRUE) %dopar% {
                                  
                                  set.seed(j) # for stable result
                                  
                                  for(i in seq_len(nrow(hyper_grid))) {
                                    
                                    xgb.fit <- xgboost(
                                      xgb.train,
                                      nrounds = 5000, ##
                                      objective = "reg:squarederror",
                                      early_stopping_rounds = 100,
                                      verbose = 0,
                                      eta = hyper_grid$eta,
                                      max_depth = hyper_grid$max_depth,
                                      min_child_weight = hyper_grid$min_child_weight,
                                      subsample = hyper_grid$subsample,
                                      colsample_bytree = hyper_grid$colsample_bytree,
                                      gamma = hyper_grid$gamma[i],
                                      lambda = hyper_grid$lambda[i],
                                      alpha = hyper_grid$alpha[i]
                                      
                                    )
                                    
                                    xgb.pred <- predict(xgb.fit, as.matrix(sapply(test, as.numeric)))
                                    
                                    # record result
                                    # hyper_grid$trees[i] <- xgb.fit$best_iteration
                                    MSPE.box[i] <- sum((test.y - xgb.pred)^2) / dim(test)[1] ##
                                    
                                    print(paste(j, 'rep ', i, 'th completed!'))
                                    
                                  }
                                  
                                  return(MSPE.box) ##
                                  
                                  
                                }


registerDoSEQ() # if, error!

# 클러스터 해제
parallel::stopCluster(party.cluster)

foreach.MSPE.box ## check!
MSPE.box <- foreach.MSPE.box
xgb.MSPE <- apply(MSPE.box, 2, mean)
xgb.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)

# extract statistics
hyper_grid$mspe <- round(xgb.MSPE,5)
hyper_grid$se.of.mspe <- round(xgb.se.of.mspe, 6)

# assess top 10 models
library(dplyr)
hyper_grid %>%
  arrange(mspe) %>% 
  head(10)



## SCAD


# install.packages('ncvreg')
library(ncvreg)

#############################
X <- train[,-1]
y <- train[, 1]

set.seed(1)
# cve The error for each value of lambda, averaged across the cross-validation folds.
# cvse The estimated standard error associated with each value of for cve.
scad.cv.fit <- cv.ncvreg(X, y)
scad.cv.fit$lambda.min
scad.cv.fit$lambda
lambda.min.ind <- ( scad.cv.fit$lambda == scad.cv.fit$lambda.min ) 
scad.cv.fit$cve
scad.cv.fit$cvse

# plot(scad.cv.fit)
scad.cv.pred <- predict(scad.cv.fit, as.matrix(test.x))
sum((test.y - scad.cv.pred)^2) / dim(test)[1]

#############################

X <- train[,-1]
y <- train[, 1]


scad.fit <- ncvreg(X, y, penalty="SCAD",
                   gamma = 3.7, # good performance default
                   alpha = 1) # alpha=1 is equivalent to MCP/SCAD penalty
scad.fit$lambda

scad.pred <- predict(scad.fit, as.matrix(test.x), lambda = scad.fit$lambda)
plot(scad.fit)

X <- train[,-1]
y <- train[, 1]

scad.fit <- ncvreg(X, y, family="gaussian", penalty="SCAD",
                   gamma = 3.7, # good performance default
                   alpha = 1, # alpha=1 is equivalent to MCP/SCAD penalty
                   lambda.min = hyper_grid$lambda.min[1],
                   nlambda = hyper_grid$nlambda[1],
                   eps = hyper_grid$eps[1])
scad.fit$lambda
# plot(scad.fit)
scad.pred <- predict(scad.fit, as.matrix(test.x), lambda = scad.fit$lambda)
dim(scad.pred)
length(test.y)
sum((test.y - scad.pred)^2) / dim(test)[1]

#############################



?ncvreg
library(ncvreg)

als <- read.table("http://web.stanford.edu/~hastie/CASI_files/DATA/ALS.txt",header=TRUE)

str(als)

# split train, test set
train.ind <- as.logical(1 - als$testset)
test.ind <- als$testset 
train <- als[train.ind,]
test <- als[test.ind,]

# remove train, test index
train <- train[,-1]
test <- test[,-1]

train.x <- train[,-1]
train.y <- train[,1]

test.x <- test[,-1]
test.y <- test[,1] ## y : dFRS


# Adaptive Lasso Function with 10-fold CV (seed 1)
scad.cv.fit <- cv.ncvreg(train.x, train.y, seed = 1)
MSPE.box <- c()
MSPE.se.box <- c()

scad.cv.fit$lambda.min ##
lambda.box <- scad.cv.fit$lambda ##
MSPE.box <- scad.cv.fit$cve
MSPE.se.box <- scad.cv.fit$cvse

scad.dat <- data.frame(t(rbind(lambda.box, MSPE.box, MSPE.se.box)))

library(dplyr)
scad.best.dat <- scad.dat %>% arrange(MSPE.box) %>% head(10)

?ncvreg


####


set.seed(1)
scad.fit <- ncvreg(as.matrix(train.x), as.matrix(train.y), penalty="SCAD")

scad.pred <- predict(scad.fit, as.matrix(test.x), lambda = scad.fit$lambda)


# scad.lambda.grid <- scad.best.dat$lambda.box

B = 5
range(scad.fit$lambda)
MSPE.box <- matrix(NA, nrow = B, ncol = length(scad.fit$lambda))

# You may wish to look at the ncvfit() function, which is intended for non-path
# (i.e., single-lambda) optimization and allows the user to supply initial values

for (j in 1:B){
  
  set.seed(j)
  
  scad.fit <- ncvreg(as.matrix(train.x), as.matrix(train.y), penalty="SCAD")
  scad.pred <- predict(scad.fit, as.matrix(test.x), lambda = scad.fit$lambda)
  
  MSPE.seed <- c()
  
  for(i in 1:length(scad.fit$lambda)){
    MSPE.seed[i] <- sum((test.y - scad.pred[,i])^2) / dim(test.x)[1]
  }
  
  MSPE.box[j,] <- MSPE.seed
  
  print(paste(j, 'rep completed!'))
  
}

MSPE.box
scad.fit$lambda
scad.MSPE <- apply(MSPE.box, 2, mean)
scad.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)


# extract statistics
scad.fit.dat <- data.frame(cbind(round(scad.fit$lambda,5), round(scad.MSPE,4), round(scad.se.of.mspe, 6)))
names(scad.fit.dat) <- c('lambda','mspe','se.of.mspe')
scad.fit.dat

# assess top 15 models
library(dplyr)
table5 <- scad.fit.dat %>% arrange(mspe) %>% head(15)

table5 <- xtable(table5[,-3], digits = c(0,6,4),
                 caption = 'SCAD lambda hyperparmeter tuning')

print(table5, scalebox=0.7, caption.placement = "top")

# scad.fit <- ncvreg(train.x, train.y, family="gaussian", penalty="SCAD",
#                    gamma = 3.7, # good performance default
#                    alpha = 1, # alpha=1 is equivalent to MCP/SCAD penalty
#                    lambda.min = 0.04577396)

beta <- coef(scad.fit)[,55]

## 변수 개수
sum(beta != 0.000000e+00)

## plotting -> train process

plot(scad.fit)

















## ALASSO

library(glmnet)

als <- read.table("http://web.stanford.edu/~hastie/CASI_files/DATA/ALS.txt",header=TRUE)

str(als)

# split train, test set
train.ind <- as.logical(1 - als$testset)
test.ind <- als$testset 
train <- als[train.ind,]
test <- als[test.ind,]

# remove train, test index
train <- train[,-1]
test <- test[,-1]

train.x <- train[,-1]
train.y <- train[,1]

test.x <- test[,-1]
test.y <- test[,1] ## y : dFRS

# als <- als[,-1]
# X <- als[,-1]
# y <- als[, 1]

# Adaptive Lasso Function with 10-fold CV (seed 1)
adaLasso <- function(full.data, data, labels, parallel = TRUE, standardize = TRUE, weight,
                     gamma = 1, formula = NULL, ols.data = NULL, lambda = NULL, seed = 1){
  require(glmnet)
  
  if(weight == "ridge"){
    set.seed(seed)
    cv.ridge <- cv.glmnet(x = data, y = labels, alpha = 0, standardize = standardize, lambda = lambda)
    weight <- 1/abs(matrix(coef(cv.ridge, s = cv.ridge$lambda.min)[-1, ]))^gamma
  }
  
  weight[,1][weight[, 1] == Inf] <- 999999999 # prevent Inf weight
  set.seed(seed)
  cv.lasso <- cv.glmnet(x = data, y = labels, alpha = 1, standardize = standardize, lambda = lambda, penalty.factor = weight)
  # lasso <- glmnet(x = data, y = labels, alpha = 1, standardize = standardize, lambda = cv.lasso$lambda.min, penalty.factor = weight)
  return(cv.lasso)
}

# # ols case -> inf!
# adalasso <- adaLasso(full.data = als, data = as.matrix(X), labels = y,
#                      weight = "ols", gamma = 1, seed = 1)

# ridge case
adalasso <- adaLasso(data = as.matrix(train.x), labels = train.y , weight = "ridge", gamma = 1, seed = 1)

# adalasso$cvm
# adalasso$cvsd
# adalasso$lambda

adalasso.dat <- data.frame(t(rbind(adalasso$lambda, adalasso$cvm, adalasso$cvsd)))
colnames(adalasso.dat) <- c('lambda', 'MSPE', 'se.of.MSPE')
adalasso.dat

?cv.glmnet
# top 10 lambda
library(dplyr)
cv.best.dat <- adalasso.dat %>% arrange(MSPE, decreasing = TRUE) %>% head(15)
length(adalasso.dat$lambda)
range(adalasso.dat$lambda)

table3 <- adalasso.dat %>% arrange(MSPE, decreasing = TRUE) %>% head(15)
table3 <- xtable(table3, digits = c(0,6,4,6),
                 caption = 'ALASSO 10 fold cross-validation hyperparmeter tuning')

print(table3, scalebox=0.7, caption.placement = "top")

## get test MSPE

ada.lambda.grid <- cv.best.dat$lambda

B = 5
MSPE.box <- matrix(NA, nrow = B, ncol = length(ada.lambda.grid))

for (j in 1:B){
  
  set.seed(j)
  
  for(i in 1:length(ada.lambda.grid)){
    
    cv.ridge <- cv.glmnet(x = as.matrix(train.x), y = train.y, alpha = 0, standardize = TRUE)
    weight <- 1/abs(matrix(coef(cv.ridge, s = cv.ridge$lambda.min)[-1, ]))
    weight[,1][weight[, 1] == Inf] <- 999999999 # prevent Inf weight
    
    
    ada.fit <- glmnet(x = as.matrix(train.x), y = train.y, alpha = 1, standardize = TRUE,
                    lambda = ada.lambda.grid[i], penalty.factor = weight)
    ada.pred <- predict(ada.fit, as.matrix(test.x))
    
    MSPE.box[j,i] <- sum((test.y - ada.pred)^2) / dim(test.x)[1]
    
    print(paste(j, 'rep ', i, 'th completed!'))
    
  }
  
}

MSPE.box

ada.MSPE <- apply(MSPE.box, 2, mean)
ada.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)


# extract statistics
ada.dat <- data.frame(cbind(ada.lambda.grid, round(ada.MSPE,4), round(ada.se.of.mspe, 6)))
names(ada.dat) <- c('lambda','mspe','se.of.mspe')
ada.dat

# assess top 10 models
library(dplyr)
table4 <-ada.dat %>% arrange(mspe) %>% head(15)

table4 <- xtable(table4, digits = c(0,6,4,6),
                 caption = 'ALASSO hyperparmeter tuning')

print(table4, scalebox=0.7, caption.placement = "top")




set.seed(1)
cv.ridge <- cv.glmnet(x = as.matrix(train.x), y = train.y, alpha = 0, standardize = TRUE)
weight <- 1/abs(matrix(coef(cv.ridge, s = cv.ridge$lambda.min)[-1, ]))
weight[,1][weight[, 1] == Inf] <- 999999999 # prevent Inf weight

ada.fit <- glmnet(x = as.matrix(train.x), y = train.y, alpha = 1, standardize = TRUE,
                  lambda = 0.10218394, penalty.factor = weight)




# 변수 개수
sum(coef(ada.fit) != 0)

# plotting -> train process
plot(adalasso)






## elastic net

library(glmnet)

als <- read.table("http://web.stanford.edu/~hastie/CASI_files/DATA/ALS.txt",header=TRUE)

str(als)

# split train, test set
train.ind <- as.logical(1 - als$testset)
test.ind <- als$testset 
train <- als[train.ind,]
test <- als[test.ind,]

# remove train, test index
train <- train[,-1]
test <- test[,-1]

train.x <- train[,-1]
train.y <- train[,1]

test.x <- test[,-1]
test.y <- test[,1] ## y : dFRS


?glmnet
ela.grid <- expand.grid(
  alpha = seq(0.1,0.9, length = 9),
  lambda = seq(0.05, 0.3, by = .05)
  )

# set.seed(20141285)
# ela.fit <- glmnet(x = as.matrix(train.x), y = train.y, alpha = 0.5,
#                   standardize = FALSE, lambda = 0.02)
# ela.pred <- predict(ela.fit, as.matrix(test.x))
# sum((test.y - ela.pred)^2) / dim(test)[1]

B = 5
MSPE.box <- matrix(NA, nrow = B, ncol = nrow(ela.grid))

for (j in 1:B){
  
  set.seed(j) # for stable result
  
  for(i in seq_len(nrow(ela.grid))) {
    
    ela.fit <- glmnet(x = as.matrix(train.x), y = train.y, alpha = ela.grid$alpha[i],
                      standardize = TRUE, lambda = ela.grid$lambda[i])
    ela.pred <- predict(ela.fit, as.matrix(test.x))
    
    MSPE.box[j,i] <- sum((test.y - ela.pred)^2) / dim(test)[1]
    
    print(paste(j, 'rep ', i, 'th completed!'))
    
  }
  
}

# warnings()
MSPE.box
ela.MSPE <- apply(MSPE.box, 2, mean)
ela.se.of.mspe <- apply(MSPE.box, 2, sd) / sqrt(B)

# extract statistics
ela.grid$mspe <- round(ela.MSPE,4)
ela.grid$se.of.mspe <- round(ela.se.of.mspe, 6)

# assess top 10 models
library(dplyr)
table6 <- ela.grid %>% arrange(mspe) %>% head(15)


table6 <- xtable(table6[,-4], digits = c(0,1,2,4),
                 caption = 'EN lambda hyperparmeter tuning')

print(table6, scalebox=0.7, caption.placement = "top")


glmnet(x.train, y.train, family="gaussian", alpha=.5)


library(glmnet)
ela.fit <- glmnet(x = as.matrix(train.x), y = train.y, alpha = 0.6,
                   standardize = TRUE, lambda = 0.05)

# coef(ela.fit)
sum(coef(ela.fit) != 0)


# cv.ela <- cv.glmnet(x = as.matrix(train.x), y = train.y,
#                     alpha = 0.6, standardize = TRUE)
# plot(cv.ela)


(0.3806 - 0.2547) / 0.3806

## SCAD, ALASSO와 변수 개수 비교하기


## interpretation : 몇 % prediction square error reduction이 이루어졌는지





