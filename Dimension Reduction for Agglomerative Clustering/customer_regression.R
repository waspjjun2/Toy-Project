
getwd()
setwd('C:/Users/c/Desktop/study/KU/2021-2/machine/proj')
library(tidyverse); library(ggplot2); library(dplyr)

mkt = read.csv("marketing_campaign.csv",header=TRUE,sep="\t")
str(mkt)

#income 결측치 제거
mkt  %>%  gather(key="key",value="val")  %>% 
  mutate(isna=is.na(val))  %>%  group_by(key) %>%
  summarise(na=sum(isna)) %>% print(n=29)

mkt = mkt %>% filter(Income!=is.na(Income))

# Z_CostContact, Z_Revenue 열 삭제
mkt=mkt[,-c(27,28)]

# Income에서 이상치 발견
summary(mkt$Income)
boxplot(mkt$Income)

# Income에서 이상치 제거 후, 요약 통계량 확인.
mkt=mkt[!(mkt$Income==666666),]
summary(mkt$Income)
boxplot(mkt$Income)

# Education 범주 3개로
mkt$Education = ifelse((mkt$Education=='Basic') | (mkt$Education=='2n Cycle'),
                       'Undergraduate',
                       ifelse(mkt$Education=='Graduation', 'Graduate', 'Postgraduate'))
table(mkt$Education)                      
#age 변수
mkt=mkt  %>%  mutate(Age=2021-Year_Birth)
boxplot(mkt$Age)
mkt=mkt  %>%  filter(!Year_Birth %in% c(1893,1899,1900))

#marital_state 처리
mkt=mkt  %>%  filter(!Marital_Status %in% c("YOLO","Absurd"))
mkt$Marital_Status = ifelse(mkt$Marital_Status=="Together" |
                              mkt$Marital_Status=="Married",'Together','Alone')
table(mkt$Marital_Status)

#dt_customer
mkt=mkt  %>%  mutate(Dt_Customer=as.Date(Dt_Customer,"%d-%m-%Y"))
summary(mkt$Dt_Customer)
q1=as.Date("2013-01-16")
q3=as.Date("2013-12-31")
mkt$Customer_period = ifelse(mkt$Dt_Customer <= q1,'Old', ifelse(mkt$Dt_Customer <= q3, 'Middle', 'New'))
table(mkt$Customer_period)

#식자재소비
mkt=mkt  %>%  mutate(foodexpense=MntWines+MntFruits+MntMeatProducts
                     +MntFishProducts+MntSweetProducts)
#필요없는 열 제거
mkt=mkt  %>%  dplyr::select(-c(Dt_Customer,ID,Year_Birth))
str(mkt)

mkt$Education = as.factor(mkt$Education)
mkt$Marital_Status = as.factor(mkt$Marital_Status)
mkt$Customer_period = as.factor(mkt$Customer_period)

########################################################################
### NumChild(자식수), NumResponded(마케팅오퍼에 반응한 횟수) 파생변수 생성
mkt = mkt  %>%  mutate(NumChild=Kidhome+Teenhome)
mkt = mkt  %>%  mutate(NumResponded=AcceptedCmp1+AcceptedCmp2+AcceptedCmp3+
                         AcceptedCmp4+AcceptedCmp5+Response)
hist(mkt$NumResponded)

########################################################################
### foodexpense에 나이, 소득, 학력 등과 같은 인적정보가 어떻게 영향을 미치는지 알아보기
mydata <- mkt %>% dplyr::select(c(Age, Education, Marital_Status, NumChild,
                                  Customer_period, NumResponded, Income, foodexpense))
head(mydata)
str(mydata)

#######################################################################

### Linear_regression
options(scipen = 100)
#train_test_set_split
set.seed(0)
train.index = sample(1:nrow(mydata), round(0.7*nrow(mydata)))
train = mydata[train.index,] #train data                              
test = mydata[-train.index,] #test data

full_model <- lm(foodexpense~., data=train)
summary(full_model)


fit.reg = lm(foodexpense ~ ., data = train)
fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #Stepwise variable selection
summary(fit.step.reg)

# Cross validation
set.seed(0)
id = sample(1:V, nrow(train), replace = T); id
V = 10 #V-fold CV
mse.train = 0; mse.test = 0
for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  val.index = which(id==i)
  mydata.train = train[-val.index,] #train data                              
  mydata.test  = train[val.index,] #test data
  
  ## Fitting
  
  fit.reg = lm(foodexpense ~ ., data = train)
  fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #Stepwise variable selection
  
  ## Predicting and Evaluating
  
  yhat.reg = predict(fit.step.reg, newdata=mydata.test, type="response")
  mse.test = mse.test + mean((mydata.test$foodexpense - yhat.reg)^2)  # MSE
  
}
cv.mse.test_lm  = mse.test/V;  cv.mse.test_lm  # test CV MSE

#test MSE
fit.reg = lm(foodexpense ~ ., data = train)
fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #Stepwise variable selection
yhat_lm = predict(fit.step.reg, newdata=test)
test_mse_lm = mean((test$foodexpense-yhat_lm)^2)
test_mse_lm

############################################
###Tree based method
library(tree); library(randomForest)
set.seed(0)
tree.fit <- tree(foodexpense ~., data=train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0)

# Cross validation
set.seed(0)
id = sample(1:V, nrow(train), replace = T)

V = 10 #10-fold CV
mse.train = 0; mse.test = 0

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  val.index = which(id==i)
  mydata.train = train[-val.index,] #train data                              
  mydata.test  = train[val.index,] #test data
  
  ## Fitting
  
  tree.fit <- tree(foodexpense ~., data=train)
  
  ## Predicting and Evaluating
  
  yhat = predict(tree.fit, newdata=mydata.test)
  mse.test = mse.test + mean((mydata.test$foodexpense - yhat)^2)  # MSE
  
}

cv.mse.test_tree  = mse.test/V;  cv.mse.test_tree  # test CV MSE

#test MSE
yhat_tree = predict(tree.fit, newdata=test)
test_mse_tree = mean((test$foodexpense-yhat_tree)^2)
test_mse_tree

#######RandomForest
rf.foodexpense <- randomForest(foodexpense ~., data=train)
rf.foodexpense

# Predictions on the test data set
yhat.rf <- predict(rf.foodexpense, newdata = test)
plot(yhat.rf, test$foodexpense)
abline(0, 1, col='red')


test_mse_rf = mean((yhat.rf - test$foodexpense)^2)
test_mse_rf


# Cross validation
set.seed(0)
id = sample(1:V, nrow(train), replace = T)

V = 10 #10-fold CV
mse.train = 0; mse.test = 0

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  val.index = which(id==i)
  mydata.train = train[-val.index,] #train data                              
  mydata.test  = train[val.index,] #test data
  
  ## Fitting
  
  rf.foodexpense <- randomForest(foodexpense ~., data=mydata.train)
  
  ## Predicting and Evaluating
  
  yhat = predict(rf.foodexpense, newdata=mydata.test)
  mse.test = mse.test + mean((mydata.test$foodexpense - yhat)^2)  # MSE
  
}

cv.mse.test_rf  = mse.test/V;  cv.mse.test_rf  # test CV MSE

# variable importance plot
importance(rf.foodexpense)
varImpPlot(rf.foodexpense)


############################################
### GAM (generalized additive model)
library(gam)

#validation_set approach로 validation mse 최소로 하는 df 찾기
set.seed(0)
index = sample(1:nrow(train), round(0.7*nrow(train)))
new_train = train[index,] #train data                              
new_test = train[-index,] #test data

error_list = c()
for (i in 2:5){
  for (j in 2:5){
    gam.fit <- lm(foodexpense~ns(Income, df=i)+ns(Age, df=j)+Education + Marital_Status + Customer_period + NumChild + NumResponded, data = new_train)
    pred <- predict(gam.fit, newdata=new_test)
    error <- mean((new_test$foodexpense-pred)^2)
    error_list <- append(error_list, error)
  }
}
error_list
which.min(error_list)
gam.fit <- lm(foodexpense ~ ns(Income, df = 4) + ns(Age, df = 3) + Education + Marital_Status + Customer_period + NumChild + NumResponded, data = train)
par(mfrow = c(2,4))
plot.Gam(gam.fit, se = TRUE, col = "red")

#Cross validation
V = 10 #V-fold CV
mse.train = 0; mse.test = 0

set.seed(0)
id = sample(1:V, nrow(train), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  val.index = which(id==i)
  mydata.train = train[-val.index,] #train data                              
  mydata.test  = train[val.index,] #test data
  
  ## Fitting
  
  gam.fit <- lm(foodexpense ~ ns(Income, df = 4) + ns(Age, df = 3) + Education + Marital_Status + Customer_period + NumChild + NumResponded, data = mydata.train)
  
  ## Predicting and Evaluating
  
  yhat = predict(gam.fit, newdata=mydata.test)
  mse.test = mse.test + mean((mydata.test$foodexpense - yhat)^2)  # MSE
  
}

cv.mse.test_gam  = mse.test/V;  cv.mse.test_gam  # test CV MSE

#test mse
gam.fit <- lm(foodexpense ~ ns(Income, df = 4) + ns(Age, df = 3) + Education +
                Marital_Status + Customer_period + NumChild + NumResponded, data = train)
yhat_gam = predict(gam.fit, newdata=test)
test_mse_gam = mean((test$foodexpense-yhat_gam)^2)
test_mse_gam

df <- data.frame('Linear Regression'=numeric(),
                 'Tree'=numeric(),
                 'Randomforest'=numeric(),
                 'GAM'=numeric())
df[1,] = c(cv.mse.test_lm, cv.mse.test_tree, cv.mse.test_rf, cv.mse.test_gam)
df[2,] = c(test_mse_lm, test_mse_tree, test_mse_rf, test_mse_gam)
rownames(df) = c('CV MSE', "Test MSE")
knitr::kable(df)

