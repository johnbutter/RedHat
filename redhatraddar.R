library(data.table)
library(FeatureHashing)
library(xgboost)
library(dplyr)
library(Matrix)


train=fread("~/Desktop/Data/Kaggle/RedHat/act_train.csv") %>% as.data.frame()
test=fread("~/Desktop/Data/Kaggle/RedHat/act_test.csv") %>% as.data.frame()


#people data frame
people=fread("~/Desktop/Data/Kaggle/RedHat/people.csv") %>% as.data.frame()
people_mod=fread("~/Desktop/Data/Kaggle/RedHat/people_mod.csv") %>% as.data.frame()

# duplicate char_1 and char_2
people$char_1<-NULL #unnecessary duplicate to char_2
people_mod$char_1<-NULL #unnecessary duplicate to char_2

# change variable name so not to confuse with activity table
names(people)[2:length(names(people))]=paste0('people_',names(people)[2:length(names(people))])
names(people_mod)[2:length(names(people_mod))]=paste0('people_',names(people_mod)[2:length(names(people_mod))])

# convert T/F to 0/1
p_logi <- names(people)[which(sapply(people, is.logical))]
for (col in p_logi) set(people, j = col, value = as.numeric(people[[col]]))
p_logi <- names(people_mod)[which(sapply(people_mod, is.logical))]
for (col in p_logi) set(people_mod, j = col, value = as.numeric(people_mod[[col]]))


feature_people <- as.data.table(people)
setkey(feature_people, by="people_id")

feature_redhat<-NULL
redhat<-as.data.table(train)
redhat<-as.data.table(TestPred)

setkey(redhat,people_id,date)
feature_redhat<-redhat[,list(
  outcome_ttl=sum(outcome),
  outcome_mean=mean(outcome),
  outcome_median=as.numeric(median(outcome))),
  by=.(people_id)
  ] 

#find people that have transitioned in training and create new training set
people_transitioned<-NULL
people_transitioned<-subset(feature_redhat,outcome_mean>0&outcome_mean<1)
people_transitioned$outcome_convert<-1
people_transitioned$outcome_ttl<-NULL
people_transitioned$outcome_mean<-NULL
people_transitioned$outcome_median<-NULL
trainswitch<-as.data.table(merge(train, people_transitioned, by = "people_id",all.x = F))
testswitch<-as.data.table(merge(TestPred, people_transitioned, by = "people_id",all.x = F))
setkey(trainswitch,people_id, date)
setkey(testswitch,people_id, date)


#add outcome_median feature
people <- merge(people, feature_redhat, by = "people_id", all.x = T)
people_mod <- merge(people_mod, feature_redhat, by = "people_id", all.x = T)
people_mod$outcome_convert<-NULL #reset outcome convert
people_mod <- merge(people_mod, people_transitioned, by = "people_id", all.x = T)
#people_mod$outcome_convert[is.na(people_mod$outcome_convert)] <- 0

write.csv(data.frame(people_mod), "~/Desktop/Data/Kaggle/RedHat/people_outcomes.csv", row.names = F)


#merge people features into trainplus
trainplus<-merge(train, feature_redhat, by = "people_id", all.x=T)

#merge people features into testplus
testplus<-merge(TestPred, feature_redhat, by = "people_id", all.x=T)

#fix outcome
trainplus$outcome<-trainplus$outcome_median

#fix outcome for testplus
testplus$outcome<-testplus$outcome_median


#remove fields not wanted from train/trainplus
trainplus$outcome_ttl<-NULL
trainplus$outcome_mean<-NULL
trainplus$outcome_median<-NULL
people$outcome_ttl<-NULL
people$outcome_mean<-NULL
people$outcome_median<-NULL

#reducing group_1 dimension
people$people_group_1[people$people_group_1 %in% names(which(table(people$people_group_1)==1))]='group unique'

#fix NAs on outcome median
#people$outcome_median[is.na(people$outcome_median)] <- mean(train$outcome)
#people$outcome_ttl[is.na(people$outcome_ttl)] <- 0 #6.44 is mean, 0 is median

# put trainplus into train
train<-trainplus

#reducing char_10 dimension
unique.char_10=
  rbind(
    select(train,people_id,char_10),
    select(train,people_id,char_10)) %>% group_by(char_10) %>% 
  summarize(n=n_distinct(people_id)) %>% 
  filter(n==1) %>% 
  select(char_10) %>%
  as.matrix() %>% 
  as.vector()

train$char_10[train$char_10 %in% unique.char_10]='type unique'
test$char_10[test$char_10 %in% unique.char_10]='type unique'

d1 <- merge(train, people, by = "people_id", all.x = T)
d2 <- merge(test, people, by = "people_id", all.x = T)
Y <- d1$outcome
d1$outcome <- NULL


row.train=nrow(train)
gc()

D=rbind(d1,d2)
D$i=1:dim(D)[1]


###uncomment this for CV run
#set.seed(120)
#unique_p <- unique(d1$people_id)
#valid_p  <- unique_p[sample(1:length(unique_p), 40000)]
#valid <- which(d1$people_id %in% valid_p)
#model <- (1:length(d1$people_id))[-valid]

test_activity_id=test$activity_id
rm(train,test,d1,d2);gc()


char.cols=c('activity_category','people_group_1',
            'char_1','char_2','char_3','char_4','char_5','char_6','char_7','char_8','char_9','char_10',
            'people_char_2','people_char_3','people_char_4','people_char_5','people_char_6','people_char_7','people_char_8','people_char_9')
for (f in char.cols) {
  if (class(D[[f]])=="character") {
    levels <- unique(c(D[[f]]))
    D[[f]] <- as.numeric(factor(D[[f]], levels=levels))
  }
}


D.sparse=
  cBind(sparseMatrix(D$i,D$activity_category),
        sparseMatrix(D$i,D$people_group_1),
        sparseMatrix(D$i,D$char_1),
        sparseMatrix(D$i,D$char_2),
        sparseMatrix(D$i,D$char_3),
        sparseMatrix(D$i,D$char_4),
        sparseMatrix(D$i,D$char_5),
        sparseMatrix(D$i,D$char_6),
        sparseMatrix(D$i,D$char_7),
        sparseMatrix(D$i,D$char_8),
        sparseMatrix(D$i,D$char_9),
        sparseMatrix(D$i,D$people_char_2),
        sparseMatrix(D$i,D$people_char_3),
        sparseMatrix(D$i,D$people_char_4),
        sparseMatrix(D$i,D$people_char_5),
        sparseMatrix(D$i,D$people_char_6),
        sparseMatrix(D$i,D$people_char_7),
        sparseMatrix(D$i,D$people_char_8),
        sparseMatrix(D$i,D$people_char_9)
  )

D.sparse=
  cBind(D.sparse,
        D$people_char_10,
        D$people_char_11,
        D$people_char_12,
        D$people_char_13,
        D$people_char_14,
        D$people_char_15,
        D$people_char_16,
        D$people_char_17,
        D$people_char_18,
        D$people_char_19,
        D$people_char_20,
        D$people_char_21,
        D$people_char_22,
        D$people_char_23,
        D$people_char_24,
        D$people_char_25,
        D$people_char_26,
        D$people_char_27,
        D$people_char_28,
        D$people_char_29,
        D$people_char_30,
        D$people_char_31,
        D$people_char_32,
        D$people_char_33,
        D$people_char_34,
        D$people_char_35,
        D$people_char_36,
        D$people_char_37,
        D$people_char_38,
        #D$outcome_ttl,
        D$binay_sum)

train.sparse=D.sparse[1:row.train,]
test.sparse=D.sparse[(row.train+1):nrow(D.sparse),]



# Hash train to sparse dmatrix X_train
dtrain  <- xgb.DMatrix(train.sparse, label = Y)
dtest  <- xgb.DMatrix(test.sparse)

param <- list(objective = "binary:logistic", 
              eval_metric = "auc",
              booster = "gblinear", 
              eta = 0.02,
              subsample = 0.7,
              colsample_bytree = 0.7,
              min_child_weight = 0,
              max_depth = 10)


#####################
################# Iterate 50-100 Times, Only need to do when changing sample size 
# Tuning Hyperparameters for XGBOOST Algorithm
# Automated to run overnight - this takes a long time.
#####################
nloops<-100
best_param = list()
best_seednumber = 1969
best_error = Inf
best_error_index = 0
library(mlr)
for (iter in 1:nloops) {
  param <- list(objective = "binary:logistic", 
                eval_metric = "auc",
                booster = "gblinear", 
                max_depth = sample(8:20, 1), #8 
                eta = runif(1, .1, .3), #0.2784 
                gamma = runif(1, 0.0, 0.2), #0.134885
                subsample = runif(1, .6, .9), #0.7742556
                colsample_bytree = runif(1, .5, .8), #0.5917445
                min_child_weight = sample(1:40, 1), #9
                max_delta_step = sample(1:10, 1) #4
  )
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  cv.nround = 500
  cv.nfold = 5
  mdcv <- xgb.cv(data=dtrain, params = param, watchlist = watchlist, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early.stop.round=2, maximize=FALSE)
  
  min_error = min(mdcv$test.auc.mean)
  min_error_index = which.min( mdcv$test.auc.mean )
  
  if (min_error < best_error) {
    best_error = min_error
    best_error_index = min_error_index
    best_seednumber = seed.number
    best_param = param
  }
  cat("Loop:", iter,"\n");
}

nround = best_error_index
set.seed(best_seednumber)
cat("Best round:", nround,"\n");
cat("Best result:",best_error,"\n");
write.csv(data.frame(best_param), "~/Desktop/Data/Kaggle/RedHat/XGBPARAM.csv", row.names = F)
############################################
# END XGBoost Tuning
############################################


###uncomment this for CV run
#
#dmodel  <- xgb.DMatrix(train.sparse[model, ], label = Y[model])
#dvalid  <- xgb.DMatrix(train.sparse[valid, ], label = Y[valid])
#
#set.seed(120)
#m1 <- xgb.train(data = dmodel
#                , param
#                , nrounds = 500
#                , watchlist = list(valid = dvalid, model = dmodel)
#                , early.stop.round = 20
#                , nthread=11
#                , print_every_n = 10)

#[300]	valid-auc:0.979167	model-auc:0.990326

set.seed(120)
m2 <- xgb.train(data = dtrain, 
                param, nrounds = 305,
                watchlist = list(train = dtrain),
                print_every_n = 10)

# Predict
out <- predict(m2, dtest)
sub <- data.frame(activity_id = test_activity_id, outcome = out)
sub<-as.data.table(sub)
setkey(sub, activity_id)
write.csv(data.frame(sub), file = "~/Desktop/Data/Kaggle/RedHat/XGBPLUS.csv", row.names = F)
#write.csv(data.frame("id"=ensemble$id, "Demanda_uni_equil"=ensemble$hp1), "~/Desktop/Data/Kaggle/GrupoBimbo/HIER1.csv", row.names = F)

#0.98035

preds[preds<0] = 0
hold[is.na(Pred1)]$Pred1 <- exp(hold[is.na(Pred1)]$PR)*PRcf1+PRcf2

ensemble$lessraddar[ensemble$outcome.x<0.5]<-0
ensemble$minp<-apply(ensemble[,2:3],1,min)
ensemble$maxp<-apply(ensemble[,2:3],1,max)
write.csv(data.frame("activity_id"=ensemble$activity_id, "outcome"=ensemble$maxp), "~/Desktop/Data/Kaggle/RedHat/ENSEMBLEMAX.csv", row.names = F)


write.csv(data.frame("activity_id"=testplus$activity_id, "outcome"=testplus$rev_outcome), "~/Desktop/Data/Kaggle/RedHat/REV25.csv", row.names = F)
write.csv(data.frame("activity_id"=testplus$activity_id, "outcome"=testplus$outcome_mean), "~/Desktop/Data/Kaggle/RedHat/MEAN.csv", row.names = F)
write.csv(data.frame("activity_id"=testplus$activity_id, "outcome"=testplus$outcome_median), "~/Desktop/Data/Kaggle/RedHat/MEDIAN.csv", row.names = F)
write.csv(data.frame("activity_id"=TestPred$activity_id, "outcome"=TestPred$outcome), "~/Desktop/Data/Kaggle/RedHat/OUTCOME9909.csv", row.names = F)


hold[is.na(Pred1)]$Pred1 <- exp(hold[is.na(Pred1)]$PR)*PRcf1+PRcf2
testplus[outcome>0.8]$rev_outcome<-1
testplus[outcome<0.2]$rev_outcome<-0

testplus[outcome>0.8 & outcome<1]$rev_outcome<-testplus[outcome>0.8 & outcome<1]$outcome+0.2
testplus[outcome<0.2 & outcome>0]$rev_outcome<-testplus[outcome<0.2 & outcome>0]$outcome+0.25

testplus[outcome>0.8 & outcome<1]$rev_outcome<-testplus[outcome>0.8 & outcome<1]$outcome+0.2
submit5[outcome<0.2 & outcome>0]$saveoutcome<-submit5[outcome<0.2 & outcome>0]$outcome+0.1

submit5$saveoutcome<-submit5$outcome

myauc<-function (actual, predicted) {
  
  r <- as.numeric(rank(predicted))
  
  n_pos <- as.numeric(sum(actual == 1))
  n_neg <- as.numeric(length(actual) - n_pos)
  auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1)/2)/(n_pos *  n_neg)
  auc
}
