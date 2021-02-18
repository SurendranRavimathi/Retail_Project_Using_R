

getwd()

setwd("C:/project/retail")

st_train=read.csv("store_train .csv",stringsAsFactors = F)
st_test=read.csv("store_test.csv",stringsAsFactors = F)

library(dplyr)
glimpse(st_train)

st_test$store= NA

st_test$data="test"
st_train$data="train"

st_all=rbind(st_train,st_test)

glimpse(st_all)

#country 
sort(table(st_all$country),decreasing = T)[1:30]

table(st_all$country,st_all$store)
sum(is.na(st_all$country))

# country is not affecting the target so ##   dummies freqcutoff=50

# State

table(st_all$State,st_all$store)

sort(table(st_all$State),decreasing = T)[1:10] #  dummies freqcutoff=50

#countyname #drop

sort(table(st_all$countyname),decreasing=T)[1:10] #drop
t=table(st_train$countyname,st_train$store)

# storecode  # drop

# Areaname # drop


sort(table(st_all$Areaname),decreasing = T)[1:20]


# countytownname #drop

table(st_all$countytownname)
sort(table(st_all$countytownname),decreasing = T)[1:20]

# state_alpha #drop
sort(table(st_all$state_alpha),decreasing = T)[1:20] 

# store_Type #create dumies
table(st_all$store_Type)

#-----creating dummies--------
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
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


dummies=c('country','State','CouSub','store_Type')

for(col in dummies){
  st_all=CreateDummies(st_all,col,50)
}

library(dplyr)
glimpse(st_all)

#----dropping---------
library(tidyr)

st_all=st_all %>% select(-countyname,-storecode,-Areaname,-countytownname,-Id,-state_alpha)


#------------------Na's-----------

lapply(st_all,function(x) sum(is.na(x)))

st_all$population[is.na(st_all$population)]

table(is.na(st_all$population))

for(col in names(st_all)){
  
  if(sum(is.na(st_all[,col]))>0 & !(col %in% c("data","store"))){
    
    st_all[is.na(st_all[,col]),col]=mean(st_all[st_all$data=='train',col],na.rm=T)
  }
  
}

st_train=st_all %>% filter(data=="train") %>% select(-data)
st_test=st_all %>% filter(data=="test") %>% select(-data)
#----------------
#-------logistic model---------------
set.seed(2)
s=sample(1:nrow(st_train),0.8*nrow(st_train))
st_train1=st_train[s,]
st_train2=st_train[-s,]

for_vif=lm(store~.,data=st_train)
library(car)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2-sales3,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2-sales3-sales1,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]


#####

log_fit=glm(store~.-sales0-sales2-sales3-sales1,data=st_train1)

log_fit=step(log_fit)

formula(log_fit)
###

log_fit=glm(store ~ sales4 + population  + country_19 + 
               country_11 + country_17 + country_9 + State_54 + 
              State_6 + State_8 + State_22  + State_72 + State_18 + 
              State_47 + State_13 + State_50 + State_33 + State_25,data=st_train1)

summary(log_fit)


#### performance of score model on validation data
library(pROC)

val.score=predict(log_fit,newdata = st_train2,type='response')

auc(roc(st_train2$store,val.score))


# since auc score is 0.7621 we need above 0.80 so moving to random forest


#-----
for_vif=lm(store~.,data=st_train)
library(car)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2-sales3,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2-sales3-sales1,data=st_train)
sort(vif(for_vif),decreasing = T)[1:3]

st_train$store=as.factor(st_train$store)

#-----randomforest------------  

library(randomForest)

rf_fit=randomForest(store~.,data=st_train1,ntree=20)

library(pROC)

val.score=predict(rf_fit,newdata = st_train2,type='response')

auc(roc(st_train2$store,val.score))









class(st_train$store)
st_all$store=as.factor(st_all$store)

library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)
library(lattice)
library(robustbase)

param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  #print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,store~., 
             data =st_train,
             tuning =params,
             folds = cvFolds(nrow(st_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  #print('DONE')
  # uncomment the line above to keep track of progress
}

## Values obtained  

# myauc=0.0.8073378
# best_params=data.frame(mtry=25,
#                        ntree=200,
#                        maxnodes=100,
#                        nodesize=1)

## Model on the entire training data

st.rf.final=randomForest(store~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=st_train
)

   

st.rf.final

test.score=predict(st.rf.final,newdata = st_test,type='prob')[,2]
write.csv(test.score,'Surendran_R_P2_part2.csv ',row.names = F)


## For hardclass prediciton we'll need to find a cutoff on score
## 
train.score=predict(st.rf.final,newdata = st_train,type='prob')[,2]

# train.score=predict(st.rf.final,newdata = st_train,type='response')

real=st_train$store
sum(is.na(real))
table(real)
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]
warnings() 

#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff


## Once we know the cutoff we can use it to convert test score to 
## hard classes

test.predicted=as.numeric(test.score>my_cutoff)
write.csv(test.predicted,'Surendran_R_P2_part2.csv',row.names = F)

r=read.csv("Surendran_R_P2_part2.csv")
r
table(r$x)
colnames(r)="store"
glimpse(r)

write.csv(r,'Surendran_R_P2_part2.csv',row.names = F)

getwd()
myauc
table(test.predicted)




#------gbm---

library(gbm)
library(cvTools)

## ------------------------------------------------------------------------
param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 10

## ----this code will take too long to run--------------------------------------------------------------
myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  # print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(gbm,store~.,
             data =st_train,
             tuning =params,
             args=list(distribution="bernoulli"),
             folds = cvFolds(nrow(st_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="response",n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    # print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    # print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  # print('DONE')
  # uncomment the line above to keep track of progress
}

glimpse(st_train)

## ----these values are from a previous run--------------------------------------------------------------
myauc= 0.8092231
best_params=data.frame(interaction.depth= 2 ,
                       n.trees=500,
                       shrinkage=0.1,
                       n.minobsinnode=5)

## ------------------------------------------------------------------------
myauc

## ------------------------------------------------------------------------
best_params

## ------------------------------------------------------------------------
st.gbm.final=gbm(store~.,data=st_train,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsinnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "bernoulli")

## ----use these for prediciton and submission on test data--------------------------------------------------------------
test.score=predict(st.gbm.final,newdata=st_test,type='response',
                    n.trees = best_params$n.trees)
## write.csv(test.score,"mysubmission.csv",row.names=F)







## For hardclass prediciton we'll need to find a cutoff on score
## 
train.score=predict(st.gbm.final,newdata = st_train,type='response',n.trees = best_params$n.trees)

# train.score=predict(st.rf.final,newdata = st_train,type='response')

real=st_train$store
sum(is.na(real))
table(real)
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]
warnings() 

#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff


# 0.452

## Once we know the cutoff we can use it to convert test score to 
## hard classes

test.predicted=as.numeric(test.score>my_cutoff)

table(test.predicted)
write.csv(test.predicted,'Surendran_R_P2_part2.csv',row.names = F)

r=read.csv("Surendran_R_P2_part2.csv")
View(r)
table(r$x)
colnames(r)="store"
glimpse(r)

write.csv(r,'Surendran_R_P2_part2.csv',row.names = F)

getwd()
myauc
table(test.predicted)
