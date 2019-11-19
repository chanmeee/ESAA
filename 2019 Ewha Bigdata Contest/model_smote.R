# Load library
set.seed(123)
library(data.table)
library(tidyverse)
library(caret) 

# Set directory
setwd("C:/Users/Chanmi Yoo/Desktop/ESAA/2019 Ewha Data Analysis Contest/data")

# Load Data
yn <- fread("weather_fire_yn.csv") 
yn <- yn %>% 
  select(c(5:8, 10)) %>% 
  mutate(fire=as.factor(fire))  #convert character into factor
str(yn)

# Divide into trainset and testset
set.seed(1886)
yn_idx = createDataPartition(yn$fire, 
                             p = 0.7, list = FALSE)
yn_train = yn[yn_idx, ]
yn_test = yn[-yn_idx, ]


# SMOTE 방법 소개 
## oversampling 방법은 너무 중복된 값이 많이 생성되고 undersampling은 중요한 데이터를 너무 많이 잃어 버린다.
## ROSE 는 이러한 문제를 해결한다. 

library(ROSE)

# Check classes distribution  
prop.table(table(yn_train$fire))
prop.table(table(yn_test$fire))
## [결과] fire가 F인 경우는 99%, T인 경우는 1%로 매우 불균형한 분포임을 알 수 있다. 

library(rpart)
rpartmod <- rpart(fire ~ ., data = yn_train)
pred.fire <- predict(rpartmod, newdata = yn_test)

# check model accuracy 
accuracy.meas(yn_test$fire, pred.fire[,2])
roc.curve(yn_test$fire, pred.fire[,2], plotit = F)
## [결과] precision: NaN, recall: 0.000, F: NaN, 예측을 못함 (매우 불균형한 분포이므로)
## 따라서 불균형한 분포 문제를 해결하기 위해 여러 방법을 사용해본다. 

# over sampling
data_balanced_over <- ovun.sample(fire ~ . , data = yn_train, method = "over", N=106908)$data
## [참고] N refers to number of observations in the resulting balanced set.
## [참고] N must be greater or equal than the actual sample size. 따라서 N이 53454 이상이어야 함. 

table(data_balanced_over$fire) 
## [결과] over sampling한 결과, fire 변수의 F와 T가 비슷해짐

# under sampling
data_balanced_under <- ovun.sample(fire ~ . , data = yn_train, method = "under", N=1000)$data
## [참고] N must be greater or equal than the number of minority class examples.
## 여기서 minority class인 T가 493개이므로 N값은 493 이상이어야 함. 
table(data_balanced_under$fire) 

# under and over smapling (both)
# the minority class is oversampled with replacement and majority class is undersampled without replacement
data_balanced_both <- ovun.sample(fire ~ ., data = yn_train, method = "both", p=0.5, N=60000, seed = 1)$data
table(data_balanced_both$fire)

data.rose <- ROSE(fire ~ ., data = yn_train, seed = 1)$data
table(data.rose$fire)

# Build decision tree models
tree.rose <- rpart(fire ~ ., data = data.rose)
tree.over <- rpart(fire ~ ., data = data_balanced_over)
tree.under <- rpart(fire ~ ., data = data_balanced_under)
tree.both <- rpart(fire ~ ., data = data_balanced_both)

# Plot decision tree models
par("mar")
par(mar=c(1,1,1,1)) # adjust margin

plot(tree.rose); text(tree.rose)
plot(tree.over); text(tree.over)
plot(tree.under); text(tree.under)
plot(tree.both); text(tree.both)
## [결과] 약간씩 모형이 다르다. 

# Predict testset
pred.tree.rose <- predict(tree.rose, newdata = yn_test)
pred.tree.over <- predict(tree.over, newdata = yn_test)
pred.tree.under <- predict(tree.under, newdata = yn_test)
pred.tree.both <- predict(tree.both, newdata = yn_test)

# Plot ROC curve
roc.curve(yn_test$fire, pred.tree.rose[,2]) #AUC ROSE
roc.curve(yn_test$fire, pred.tree.over[,2]) #AUC Oversampling
roc.curve(yn_test$fire, pred.tree.under[,2]) #AUC Undersampling
roc.curve(yn_test$fire, pred.tree.both[,2]) #AUC Both 

## AUC comparison
roc.curve(yn_test$fire, pred.tree.rose[,2]) ; roc.curve(yn_test$fire, pred.tree.over[,2], add.roc = T, ) ; roc.curve(yn_test$fire, pred.tree.under[,2], add.roc = T) ;roc.curve(yn_test$fire, pred.tree.both[,2], add.roc = T) 

## [결과] AUC값: ROSE는 0.757, oversampling은 0.766, undersampling은 0.768, both는 0.769 
## [결론] 통상적으로 ROSE package를 이용한 방법이 가장 정확도가 높게 나오지만, 이 경우 대부분이 비슷하지만 BOTH가 가장 높게 나왔다. 
