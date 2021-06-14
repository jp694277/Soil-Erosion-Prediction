library(readr)
library(class);library(caret);library(ggplot2)
library(dplyr);library(car);library(MASS);library(klaR)#LDA
library(xgboost) #boosting
# split data
data <- read_rds("RDS files/data.rds")
train <- read_rds("RDS files/soil_train.rds")
test <- read_rds("RDS files/soil_test.rds")
test_1 <- read_rds("RDS files/test_1.rds")
test_2 <- read_rds("RDS files/test_2.rds")
test_3 <- read_rds("RDS files/test_3.rds")
test_4 <- read_rds("RDS files/test_4.rds")
test_5 <- read_rds("RDS files/test_5.rds")


training <- train[1:58785,-c(17)]
testing1 <- test_1[1:11757,-c(17)]
testing2 <- test_2[1:11757,-c(17)]
testing3 <- test_3[1:11757,-c(17)]
testing4 <- test_4[1:11757,-c(17)]
testing5 <- test_5[1:11757,-c(17)]

train_purchase <- train[1:58785,c(17)]
test_purchase1 <- test_1[1:11757,c(17)]
test_purchase2 <- test_2[1:11757,c(17)]
test_purchase3 <- test_3[1:11757,c(17)]
test_purchase4 <- test_4[1:11757,c(17)]
test_purchase5 <- test_5[1:11757,c(17)]

################################ KNN ############################
require(class)
set.seed(12345)

# misclassification error rate

training <- training[,-c(5:9,11:12,16,20)]
testing1 <- testing1[,-c(5:9,11:12,16,20)]
testing2 <- testing2[,-c(5:9,11:12,16,20)]
testing3 <- testing3[,-c(5:9,11:12,16,20)]
testing4 <- testing4[,-c(5:9,11:12,16,20)]
testing5 <- testing5[,-c(5:9,11:12,16,20)]


#12
# texture tillage removal nirrcapcl nirrcapscl irrcapscl farmlndcl
predicted_purchase = knn(training, testing1, train_purchase, k=1)
mean(test_purchase1 != predicted_purchase)
table(predicted_purchase, test_purchase1)

# confusion matrix
require(caret)
confusionMatrix(predicted_purchase, test_purchase1, positive="Severe") 

# choose k
#test1
predicted_purchase = NULL
error_rate = NULL
for(i in 1:30){
  set.seed(1)
  predicted_purchase = knn(training, testing1, train_purchase, k=i)
  error_rate[i] = mean(test_purchase1 != predicted_purchase)
}
print(error_rate)
(min_error_rate = min(error_rate))
which(error_rate == min_error_rate)

# k=9
predicted_purchase = knn(training, testing1, train_purchase,k=9)
mean(test_purchase1 != predicted_purchase)
table(predicted_purchase, test_purchase1)
confusionMatrix(predicted_purchase, test_purchase1, positive="Severe") 
# 0.7652

## plot eror rate and k
require(ggplot2)
k_plot <- qplot(1:30, error_rate*100, 
                xlab = "K",
                ylab = "Error Rate %",
                ylim = c(0,30), 
                geom=c("point", "line"))
write_rds(error_rate, "RDS files/k_plot.rds")
#test2
predicted_purchase = NULL
error_rate = NULL
for(i in 1:30){
  set.seed(1)
  predicted_purchase = knn(training, testing2, train_purchase, k=i)
  error_rate[i] = mean(test_purchase2 != predicted_purchase)
}
print(error_rate)
(min_error_rate = min(error_rate))
which(error_rate == min_error_rate)
# k=9
predicted_purchase = knn(training, testing2, train_purchase, k=9)
mean(test_purchase2 != predicted_purchase)
table(predicted_purchase, test_purchase2)
confusionMatrix(predicted_purchase, test_purchase2, positive="Severe") 
# 0.7659

## plot eror rate and k
require(ggplot2)
k_plot <- qplot(1:30, error_rate*100, 
                xlab = "K",
                ylab = "Error Rate %",
                ylim = c(0,30), 
                geom=c("point", "line"))

#test3
predicted_purchase = NULL
error_rate = NULL
for(i in 1:30){
  set.seed(1)
  predicted_purchase = knn(training, testing3, train_purchase, k=i)
  error_rate[i] = mean(test_purchase3 != predicted_purchase)
}
print(error_rate)
(min_error_rate = min(error_rate))
which(error_rate == min_error_rate)
# k=13
predicted_purchase = knn(training, testing3, train_purchase, k=13)
mean(test_purchase3 != predicted_purchase)
table(predicted_purchase, test_purchase3)
confusionMatrix(predicted_purchase, test_purchase3, positive="Severe") 
# 0.7644

## plot eror rate and k
require(ggplot2)
k_plot <- qplot(1:30, error_rate*100, 
                xlab = "K",
                ylab = "Error Rate %",
                ylim = c(0,30), 
                geom=c("point", "line"))

#test4
predicted_purchase = NULL
error_rate = NULL
for(i in 1:30){
  set.seed(1)
  predicted_purchase = knn(training, testing4, train_purchase, k=i)
  error_rate[i] = mean(test_purchase4 != predicted_purchase)
}
print(error_rate)
(min_error_rate = min(error_rate))
which(error_rate == min_error_rate)
# k=9
predicted_purchase = knn(training, testing4, train_purchase, k=9)
mean(test_purchase4 != predicted_purchase)
table(predicted_purchase, test_purchase4)
confusionMatrix(predicted_purchase, test_purchase4, positive="Severe") 
# 0.7665

## plot eror rate and k
require(ggplot2)
k_plot <- qplot(1:30, error_rate*100, 
                xlab = "K",
                ylab = "Error Rate %",
                ylim = c(0,30), 
                geom=c("point", "line"))

#test5
predicted_purchase = NULL
error_rate = NULL
for(i in 1:30){
  set.seed(1)
  predicted_purchase = knn(training, testing5 ,train_purchase, k=i)
  error_rate[i] = mean(test_purchase5 != predicted_purchase)
}
print(error_rate)
(min_error_rate = min(error_rate))
which(error_rate == min_error_rate)

# k=7
predicted_purchase = knn(training, testing5, train_purchase, k=7)
mean(test_purchase5 != predicted_purchase)
table(predicted_purchase, test_purchase5)
confusionMatrix(predicted_purchase, test_purchase5, positive="Severe") 
# 0.7671

## plot eror rate and k
require(ggplot2)
k_plot <- qplot(1:30, error_rate*100, 
      xlab = "K",
      ylab = "Error Rate %",
      ylim = c(0,30), 
      geom=c("point", "line"))

rate = (0.7652+0.7659+0.7644+0.7665+0.7671)/5
# 0.76582

################################ LDA #######################
training <- train[,-c(4:9,11:12,16,20,21)]
testing1 <- test_1[,-c(4:9,11:12,16,20,21)]
testing2 <- test_2[,-c(4:9,11:12,16,20,21)]
testing3 <- test_3[,-c(4:9,11:12,16,20,21)]
testing4 <- test_4[,-c(4:9,11:12,16,20,21)]
testing5 <- test_5[,-c(4:9,11:12,16,20,21)]


#test1
modlda = train(soil_erosion ~ .,data=training,method="lda")
plda = predict(modlda,testing1)
ct <- table(plda,testing1$soil_erosion)
prop.table(ct, 1)
sum(diag(prop.table(ct)))
#0.8602535

#test2
plda = predict(modlda,testing2)
ct <- table(plda,testing2$soil_erosion)
prop.table(ct, 1)
sum(diag(prop.table(ct)))
#0.8618695

#test3
plda = predict(modlda,testing3)
ct <- table(plda,testing3$soil_erosion)
prop.table(ct, 1)
sum(diag(prop.table(ct)))
#0.8615383

#test4
plda = predict(modlda,testing4)
ct <- table(plda,testing4$soil_erosion)
prop.table(ct, 1)
sum(diag(prop.table(ct)))
#0.8618695

#test5
plda = predict(modlda,testing5)
ct <- table(plda,testing5$soil_erosion)
prop.table(ct, 1)
sum(diag(prop.table(ct)))
#0.8612741

rate = (0.8602535 + 0.8618695 + 0.8615383 + 0.8618695 + 0.8612741)/5
# 0.861361

fit <- lda(soil_erosion~., data=training)
fit1 <- predict(fit, testing1)
write_rds(fit, "RDS files/lda.rds")
write_rds(training, "RDS files/ldatr.rds")

plot(fit, col="steelblue3")
plot(fit1$posterior, col=(c("blue","red")))

################################ Boosting ##################
xgb_train <- train
xgb_test1 <- test_1
xgb_test2 <- test_2
xgb_test3 <- test_3
xgb_test4 <- test_4
xgb_test5 <- test_5

#10 categorical var
################################ dummy var ##################
# xgb_test1
# tfact
xgb_test5$tfact_1 = ifelse(test_5$tfact == 1, 1, 0)
xgb_test5$tfact_2 = ifelse(test_5$tfact == 2, 1, 0)
xgb_test5$tfact_3 = ifelse(test_5$tfact == 3, 1, 0)
xgb_test5$tfact_4 = ifelse(test_5$tfact == 4, 1, 0)
xgb_test5$tfact_5 = ifelse(test_5$tfact == 5, 1, 0)
# nirrcapcl
xgb_test5$nirrcapcl_1 = ifelse(test_5$nirrcapcl == 1, 1, 0)
xgb_test5$nirrcapcl_2 = ifelse(test_5$nirrcapcl == 2, 1, 0)
xgb_test5$nirrcapcl_3 = ifelse(test_5$nirrcapcl == 3, 1, 0)
xgb_test5$nirrcapcl_4 = ifelse(test_5$nirrcapcl == 4, 1, 0)
xgb_test5$nirrcapcl_5 = ifelse(test_5$nirrcapcl == 5, 1, 0)
xgb_test5$nirrcapcl_6 = ifelse(test_5$nirrcapcl == 6, 1, 0)
xgb_test5$nirrcapcl_7 = ifelse(test_5$nirrcapcl == 7, 1, 0)
xgb_test5$nirrcapcl_8 = ifelse(test_5$nirrcapcl == 8, 1, 0)
xgb_test5$nirrcapcl_o = ifelse(test_5$nirrcapcl == "other", 1, 0)
# nirrcapscl
xgb_test5$nirrcapscl_c = ifelse(test_5$nirrcapscl == "c", 1, 0)
xgb_test5$nirrcapscl_e = ifelse(test_5$nirrcapscl == "e", 1, 0)
xgb_test5$nirrcapscl_o = ifelse(test_5$nirrcapcl == "other", 1, 0)
xgb_test5$nirrcapscl_s = ifelse(test_5$nirrcapscl == "s", 1, 0)
xgb_test5$nirrcapscl_w = ifelse(test_5$nirrcapscl == "w", 1, 0)
# irrcapcl
xgb_test5$irrcapcl_1 = ifelse(test_5$irrcapcl == 1, 1, 0)
xgb_test5$irrcapcl_2 = ifelse(test_5$irrcapcl == 2, 1, 0)
xgb_test5$irrcapcl_3 = ifelse(test_5$irrcapcl == 3, 1, 0)
xgb_test5$irrcapcl_4 = ifelse(test_5$irrcapcl == 4, 1, 0)
xgb_test5$irrcapcl_5 = ifelse(test_5$irrcapcl == 5, 1, 0)
xgb_test5$irrcapcl_6 = ifelse(test_5$irrcapcl == 6, 1, 0)
xgb_test5$irrcapcl_7 = ifelse(test_5$irrcapcl == 7, 1, 0)
xgb_test5$irrcapcl_o = ifelse(test_5$irrcapcl == "other", 1, 0)
# irrcapscl
xgb_test5$irrcapscl_c = ifelse(test_5$irrcapscl == "c", 1, 0)
xgb_test5$irrcapscl_e = ifelse(test_5$irrcapscl == "e", 1, 0)
xgb_test5$irrcapscl_o = ifelse(test_5$irrcapcl == "other", 1, 0)
xgb_test5$irrcapscl_s = ifelse(test_5$irrcapscl == "s", 1, 0)
xgb_test5$irrcapscl_w = ifelse(test_5$irrcapscl == "w", 1, 0)
# farmlndcl
xgb_test5$farmlndcl_1 = ifelse(test_5$farmlndcl == "All areas are prime farmland", 1, 0)
xgb_test5$farmlndcl_2 = ifelse(test_5$farmlndcl == "Farmland of local importance", 1, 0)
xgb_test5$farmlndcl_3 = ifelse(test_5$farmlndcl == "Farmland of statewide importance", 1, 0)
xgb_test5$farmlndcl_4 = ifelse(test_5$farmlndcl == "Not prime farmland", 1, 0)
xgb_test5$farmlndcl_5 = ifelse(test_5$farmlndcl == "NULL", 1, 0)
xgb_test5$farmlndcl_6 = ifelse(test_5$farmlndcl == "Prime farmland if drained", 1, 0)
xgb_test5$farmlndcl_7 = ifelse(test_5$farmlndcl == "Prime farmland if drained and either protected from flooding or not frequently flooded during the growing season", 1, 0)
xgb_test5$farmlndcl_8 = ifelse(test_5$farmlndcl == "Prime farmland if irrigated", 1, 0)
xgb_test5$farmlndcl_9 = ifelse(test_5$farmlndcl == "Prime farmland if irrigated and drained", 1, 0)
xgb_test5$farmlndcl_10 = ifelse(test_5$farmlndcl == "Prime farmland if irrigated and either protected from flooding or not frequently flooded during the growing season", 1, 0)
xgb_test5$farmlndcl_11 = ifelse(test_5$farmlndcl == "Prime farmland if irrigated and reclaimed of excess salts and sodium", 1, 0)
xgb_test5$farmlndcl_12 = ifelse(test_5$farmlndcl == "Prime farmland if irrigated and the product of I (soil erodibility) x C (climate factor) does not exceed 60", 1, 0)
# texture
xgb_test5$texture_C = ifelse(test_5$texture == "C", 1, 0)
xgb_test5$texture_CBCL = ifelse(test_5$texture == "CB-CL", 1, 0)
xgb_test5$texture_CL = ifelse(test_5$texture == "CL", 1, 0)
xgb_test5$texture_CNL = ifelse(test_5$texture == "CN-L", 1, 0)
xgb_test5$texture_COS = ifelse(test_5$texture == "COS", 1, 0)
xgb_test5$texture_COSL = ifelse(test_5$texture == "COSL", 1, 0)
xgb_test5$texture_FS = ifelse(test_5$texture == "FS", 1, 0)
xgb_test5$texture_FSL = ifelse(test_5$texture == "FSL", 1, 0)
xgb_test5$texture_GRCL = ifelse(test_5$texture == "GR-CL", 1, 0)
xgb_test5$texture_GRL = ifelse(test_5$texture == "GR-L", 1, 0)
xgb_test5$texture_GRLS = ifelse(test_5$texture == "GR-LS", 1, 0)
xgb_test5$texture_GRS = ifelse(test_5$texture == "GR-S", 1, 0)
xgb_test5$texture_GRSL = ifelse(test_5$texture == "GR-SL", 1, 0)
xgb_test5$texture_GRVFSL = ifelse(test_5$texture == "GR--VFSL", 1, 0)
xgb_test5$texture_L = ifelse(test_5$texture == "L", 1, 0)
xgb_test5$texture_LCOS = ifelse(test_5$texture == "LCOS", 1, 0)
xgb_test5$texture_LFS = ifelse(test_5$texture == "LFS", 1, 0)
xgb_test5$texture_LS = ifelse(test_5$texture == "LS", 1, 0)
xgb_test5$texture_LVFS = ifelse(test_5$texture == "LVFS", 1, 0)
xgb_test5$texture_MPT = ifelse(test_5$texture == "MPT", 1, 0)
xgb_test5$texture_S = ifelse(test_5$texture == "S", 1, 0)
xgb_test5$texture_SCL = ifelse(test_5$texture == "SCL", 1, 0)
xgb_test5$texture_SIC = ifelse(test_5$texture == "SIC", 1, 0)
xgb_test5$texture_SICL = ifelse(test_5$texture == "SICL", 1, 0)
xgb_test5$texture_SIL = ifelse(test_5$texture == "SIL", 1, 0)
xgb_test5$texture_SL = ifelse(test_5$texture == "SL", 1, 0)
xgb_test5$texture_SPM = ifelse(test_5$texture == "SPM", 1, 0)
xgb_test5$texture_SRVFSL = ifelse(test_5$texture == "SR- VFSL", 1, 0)
xgb_test5$texture_STC = ifelse(test_5$texture == "ST-C", 1, 0)
xgb_test5$texture_STFSL = ifelse(test_5$texture == "ST-FSL", 1, 0)
xgb_test5$texture_VAR = ifelse(test_5$texture == "VAR", 1, 0)
xgb_test5$texture_VFSL = ifelse(test_5$texture == "VFSL", 1, 0)
# rotation
xgb_test5$rotation_CG = ifelse(test_5$rotation == "CG", 1, 0)
xgb_test5$rotation_CGCAWW = ifelse(test_5$rotation == "CGCAWW", 1, 0)
xgb_test5$rotation_CGFAWW = ifelse(test_5$rotation == "CGFAWW", 1, 0)
xgb_test5$rotation_SGFAWW = ifelse(test_5$rotation == "SGFAWW", 1, 0)
xgb_test5$rotation_WWFA = ifelse(test_5$rotation == "WWFA", 1, 0)
# tillage
xgb_test5$tillage_NT = ifelse(test_5$tillage == "NT", 1, 0)
xgb_test5$tillage_RT = ifelse(test_5$tillage == "RT", 1, 0)
# removal
xgb_test5$removal_MHH = ifelse(test_5$removal == "MHH", 1, 0)
xgb_test5$removal_NRH = ifelse(test_5$removal == "NRH", 1, 0)

xgb_test5 <- xgb_test5[,-c(4:9,11:12,16,21)]

################################ model###########
xgb_train <- read_rds("RDS files/xgb_train.rds")
xgb_test1 <- read_rds("RDS files/xgb_test1.rds")
xgb_test2 <- read_rds("RDS files/xgb_test2.rds")
xgb_test3 <- read_rds("RDS files/xgb_test3.rds")
xgb_test4 <- read_rds("RDS files/xgb_test4.rds")
xgb_test5 <- read_rds("RDS files/xgb_test5.rds")

xgb_train$soil_erosion <-  ifelse(xgb_train$soil_erosion=="Severe", 1 ,0)
xgb_train$soil_erosion <- as.factor(xgb_train$soil_erosion)
xgb_test1$soil_erosion <-  ifelse(xgb_test1$soil_erosion=="Severe", 1 ,0)
xgb_test1$soil_erosion <- as.factor(xgb_test1$soil_erosion)

#test1
#choose best rounds
cv.model = xgb.cv(
  params = param, 
  data = as.matrix(xgb_train[,-c(8)]),
  label = as.matrix(xgb_train[,c(8)]),
  nfold = 5,     
  nrounds=5000,
  early_stopping_rounds = 1000, 
  print_every_n = 20 ) 

bestround <- cv.model$best_iteration
# 4959
tmp <- cv.model$evaluation_log
plot(x=1:nrow(tmp), y= tmp$train_error_mean, col='orange', xlab="nround", ylab="Error rate", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_error_mean, col='skyblue') 
legend("topright", pch=1, col = c("orange", "skyblue"), 
       legend = c("Train", "Validation") )

write_rds(tmp, "RDS files/tmp.rds")
param <- list("objective" = "binary:logistic", eta=0.01, colsample_bytree = 0.95,
              subsample=0.55, max_depth=1, alpha=0.1)
fit_xgboost <- xgboost(param =param, data = as.matrix(xgb_train[,-c(8)])
                       , label = as.matrix(xgb_train[,c(8)]), nrounds=bestround)
#accuracy
pred <- predict(fit_xgboost, as.matrix(xgb_test1[,-c(8)]))
gbpred <- ifelse (pred > 0.5,1,0)
#confusion matrix
gbpred <- as.factor(gbpred)
confusionMatrix (gbpred, xgb_test1[,c(8)])
#Accuracy - 0.9193

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(xgb_train[,-c(8)]),model = fit_xgboost)
xgb.plot.importance (importance_matrix = mat[1:20]) 

#test2
xgb_test2$soil_erosion <-  ifelse(xgb_test2$soil_erosion=="Severe", 1 ,0)
xgb_test2$soil_erosion <- as.factor(xgb_test2$soil_erosion)

#accuracy
pred <- predict(fit_xgboost, as.matrix(xgb_test2[,-c(8)]))
gbpred <- ifelse (pred > 0.5,1,0)
#confusion matrix
gbpred <- as.factor(gbpred)
confusionMatrix (gbpred, xgb_test2[,c(8)])
#Accuracy - 0.923 

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(xgb_train[,-c(8)]),model = fit_xgboost)
xgb.plot.importance (importance_matrix = mat[1:20]) 

#test3
xgb_test3$soil_erosion <-  ifelse(xgb_test3$soil_erosion=="Severe", 1 ,0)
xgb_test3$soil_erosion <- as.factor(xgb_test3$soil_erosion)

#accuracy
pred <- predict(fit_xgboost, as.matrix(xgb_test3[,-c(8)]))
gbpred <- ifelse (pred > 0.5,1,0)
#confusion matrix
gbpred <- as.factor(gbpred)
confusionMatrix (gbpred, xgb_test3[,c(8)])
#Accuracy - 0.9208

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(xgb_train[,-c(8)]),model = fit_xgboost)
xgb.plot.importance (importance_matrix = mat[1:20]) 

#test4
xgb_test4$soil_erosion <-  ifelse(xgb_test4$soil_erosion=="Severe", 1 ,0)
xgb_test4$soil_erosion <- as.factor(xgb_test4$soil_erosion)

#accuracy
pred <- predict(fit_xgboost, as.matrix(xgb_test4[,-c(8)]))
gbpred <- ifelse (pred > 0.5,1,0)
#confusion matrix
gbpred <- as.factor(gbpred)
confusionMatrix (gbpred, xgb_test4[,c(8)])
#Accuracy - 0.9213` 

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(xgb_train[,-c(8)]),model = fit_xgboost)
xgb.plot.importance (importance_matrix = mat[1:20]) 


#test5
xgb_test5$soil_erosion <-  ifelse(xgb_test5$soil_erosion=="Severe", 1 ,0)
xgb_test5$soil_erosion <- as.factor(xgb_test5$soil_erosion)

#accuracy
pred <- predict(fit_xgboost, as.matrix(xgb_test5[,-c(8)]))
gbpred <- ifelse (pred > 0.5,1,0)
#confusion matrix
gbpred <- as.factor(gbpred)
confusionMatrix (gbpred, xgb_test5[,c(8)])
#Accuracy - 0.9206

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(xgb_train[,-c(8)]),model = fit_xgboost)
xgb.plot.importance (importance_matrix = mat[1:20]) 

# rate
rate <- (0.9193 + 0.923 + 0.9208 + 0.9213 + 0.9206) / 5
# 0.921