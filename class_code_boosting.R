setwd("~/Dropbox/March onwards/CBAP with R/Data")



# imports
library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)

# data prep
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

bd=read.csv("bike_sharing_hours.csv",stringsAsFactors = F)

bd=bd %>% select(-yr,-instant,-dteday,-temp,-casual,-registered)

for(var in c("season","mnth","hr","weekday")){
  bd=CreateDummies(bd,var,500)
}

set.seed(2)
s=sample(1:nrow(bd),0.8*nrow(bd))
bd_train=bd[s,]
bd_test=bd[-s,]

# GBM 
gbm.fit=gbm(cnt~.-weekday_6,
            data=bd_train,
            distribution = "gaussian",
            n.trees = 100,interaction.depth = 3)

test.predicted=predict.gbm(gbm.fit,newdata=bd_test,n.trees=100)
(test.predicted-bd_test$cnt)**2 %>% mean() %>% sqrt()
# xgboost
x_train=bd_train %>% select(-cnt)
y_train=bd_train$cnt
x_test=bd_test %>% select(-cnt)
xgb.fit=xgboost(data=data.matrix(x_train),
                label = y_train,
                objective='reg:linear',
                verbose=1,
                nrounds = 10)

test.predicted=predict(xgb.fit,data.matrix(x_test))
(test.predicted-bd_test$cnt)**2 %>% mean() %>% sqrt()

# stacking
# you can use as many algos as you want

mykfolds=function(nobs,nfold=5){
  
  t=cvFolds(nobs,K=nfold,type='random')
  
  folds=list()
  
  for(i in 1:nfold){
    
    test=t$subsets[t$which==i]
    train=t$subsets[t$which!=i]
    
    folds[[i]]=list('train'=train,'test'=test)
  }
  
  return(folds)
}

myfolds=mykfolds(nrow(bd_train),10)

bd_train_layer1=data.frame(rf_var=numeric(nrow(bd_train)),
                           gbm_var=numeric(nrow(bd_train)))

for(i in 1:10){
  print(c(i))
  fold=myfolds[[i]]
  
  train_data=bd_train[fold$train,]
  test_data=bd_train[fold$test,]
  
  print('rf')
  
  rf.fit=randomForest(cnt~.-weekday_6,data=train_data,ntree=100,mtry=10)
  
  rf_score=predict(rf.fit,newdata=test_data)
  
  print('gbm')
  gbm.fit=gbm(cnt~.-weekday_6,
              data=train_data,
              distribution = "gaussian",
              n.trees = 100,interaction.depth = 3)
  
  gbm_score=predict(gbm.fit,newdata=test_data,
                    n.trees=100)
  
  bd_train_layer1$rf_var[fold$test]=rf_score
  
  bd_train_layer1$gbm_var[fold$test]=gbm_score
}

### stack layer 2  data  for test & linear model

bd_test_layer2=data.frame(rf_var=numeric(nrow(bd_test)),
                          gbm_var=numeric(nrow(bd_test)))

full.rf=randomForest(cnt~.-weekday_6,data=bd_train,ntree=100,mtry=10)
full.gbm=gbm(cnt~.-weekday_6,
             data=bd_train,
             distribution = "gaussian",
             n.trees = 100,interaction.depth = 3)

bd_test_layer2$rf_var=predict(full.rf,newdata=bd_test)
bd_test_layer2$gbm_var=predict(full.gbm,newdata=bd_test,
                               n.trees=100)

# linear model

bd_train_layer1$cnt=bd_train$cnt
bd_test_layer2$cnt=bd_test$cnt

lin.model=lm(cnt~.,data=bd_train_layer1)

test.pred=predict(lin.model,newdata = bd_test_layer2)

(test.pred-bd_test_layer2$cnt)**2 %>% mean() %>% sqrt()



# Parameter Tuning Example for randomFOrest classfication
# same method can be used for any algorithm
# for regression problem there is no need for a 
# special scoring/cost function argument

ci=read.csv("census_income.csv",stringsAsFactors = F)


ci=ci %>% select(-education)

ci$Y=as.numeric(ci$Y==" >50K")
ci$Y=as.factor(ci$Y)

cat_var=names(ci)[sapply(ci,is.character)]


for(var in cat_var){
  ci=CreateDummies(ci,var,500)
}

set.seed(3)
s=sample(1:nrow(ci),0.8*nrow(ci))
ci_train=ci[s,]
ci_test=ci[-s,]

library(cvTools)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}


param=data.frame(mtry=5,
                 ntree=10)

k=cvTuning(randomForest,
           Y~.,
           data =ci_train,
           tuning =param,
           folds = cvFolds(nrow(ci_train), K=5, type = "random"),
           cost =mycost_auc, seed =2,
           predictArgs = list(type="prob"))

k$cv[,2]


param=list(mtry=c(5,10,15,20),
           ntree=c(10,50,100,200),
           maxnodes=c(5,10,15),
           nodesize=c(1,2,5,10))


subset_paras=function(full_list_para,n=2){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}


my_params=subset_paras(param,10)
my_params

myauc=0

for(i in 1:10){
  print('starting iteration')
  
  params=my_params[i,]
  
  k=cvTuning(randomForest,Y~.,
             data =ci_train,
             tuning =params,
             folds = cvFolds(nrow(ci_train), K=5, type = "random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob"))
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    myauc=score.this
    print(myauc)
  }
  
  print('DONE')
}

### mtry ntree maxnodes nodesize
# 5   200       15        1

rf.tuned.model=randomForest(Y~.,data=ci_train,
                            ntree=200,mtry=5,maxnodes=15,nodesize=1,do.trace=T)

test.score=predict(rf.tuned.model,newdata = ci_test,type='prob')[,1]
pROC::roc(ci_test$Y,test.score)
