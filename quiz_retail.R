
getwd()

setwd("C:/project/retail")

re_train=read.csv("store_train .csv" )



# what is the total sales (sum of all sales) of Supermarket Type1 in area Kennebec County, ME?

library(tidyr)
re_train %>% 
  filter(store_Type=="Supermarket Type1",Areaname=="Kennebec County, ME") %>% 
  select(sales0,sales1,sales2,sales3,sales4) %>% 
  sum()
  
# ans: 38680

# Should storecode be included in building models?
  
  # Note : Just write 'Yes' or 'No' . If you write sentences ,
# automated grading will consider it incorrect .Answers are not case sensitive . 


glimpse(re_train)

table(re_train$storecode)

length(unique(re_train$storecode)) # 1891 unique value

# so ans : No


# should country be treated as numeric type or character?
  
  # Note: Answers are not case sensitive . 

summary(re_train$country)
 table(re_train$store,re_train$country)
 
 # no relation to target
 # ans: character
 
 
 # Find out number of unique categories of variable Areaname.
 
 length(unique(re_train$Areaname))

 # ans: 1891
 
 
 # For store type grocery store what is the response rate ? 
 # [ what % of obs have response value as 1 ]  Round off to two decimal digits. 
 
 # Note : Answer needs to be in hundreds . Ex : 12.34 ( NOT 0.1234 )
 
 
 re_train %>% filter(store_Type=="Grocery Store") %>% 
   select(store) %>% table() %>% prop.table()
 
 
table(re_train$store_Type) 
 
round(0.4212963 *100,2)

# ans :42.13

# Do all the sales variable follow normal distribution?
  
  # Note : Just write 'Yes' or 'No' . If you write sentences ,
# automated grading will consider it incorrect . Answers are not case sensitive . 

shapiro.test(re_train$sales0)#no
shapiro.test(re_train$sales1)#no
shapiro.test(re_train$sales2)#no
shapiro.test(re_train$sales3)#no
shapiro.test(re_train$sales4)#no

# ans:No

# Number of outliers for total sales based on following limits
# (q1-1.5*IQR, q3+1.5*IQR)?





# correct

re_train=re_train %>% mutate(total_sales=sales0+sales1+sales2+sales3+sales4) 
length(boxplot(re_train$total_sales)$out)
# ans: 140


library(ggplot2)

ggplot(re_train,aes(y=total_sales))+geom_boxplot()


# which store type has maximum variance in total sales?
  
  # Note: you need to write name of just that category .
# Any further details will result in your answer being marked as wrong.
# Answers are not case sensitive .

table(re_train$store_Type)
re_train %>%  select(store_Type,total_sales) %>% 
  mutate(Gs=total_sales[store_Type=="Grocery Store"],
         st1=total_sales[store_Type=="Supermarket Type1"],
         st2=total_sales[store_Type=="Supermarket Type2"],
         st3=total_sales[store_Type=="Supermarket Type3"])



Gs=re_train$total_sales[re_train$store_Type=="Grocery Store"]
st1=re_train$total_sales[re_train$store_Type=="Supermarket Type1"]
st2=re_train$total_sales[re_train$store_Type=="Supermarket Type2"]
st3=re_train$total_sales[re_train$store_Type=="Supermarket Type3"]


var(Gs)
var(st1)
var(st2)
var(st3)


# ans :  Grocery Store



# How many dummies will you create for variable state_alpha?

library(dplyr)
glimpse(re_train)

# a=sort(table(re_train$state_alpha),decreasing = T)
# 
# cutoff=100
# 
# sum(a>cutoff)
# 
# # ans: 7

length(table(re_train$state_alpha))
# ans:54 and n-1 so 53

# What should be the type of categorical variable when using the function randomForest?
  
  # Note: Answers are not case sensitive . 

# ans: factor
