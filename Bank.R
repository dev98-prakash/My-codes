library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC)
library(ggplot2)
library(tidyr)
library(ROCit)


setwd("/Users/meghanasingh/Documents")

ld_train=read.csv("store_train.csv",stringsAsFactors = F)

ld_test= read.csv("store_test.csv",stringsAsFactors = F)

glimpse(ld_train)

vis_dat(ld_train)

dp_pipe=recipe(store~.,data=ld_train) %>% 
  update_role(Id,storecode,Areaname,State,countytownname,new_role = "drop_vars") %>%
  update_role(state_alpha,countyname,store_Type,new_role="to_dummies") %>% 
  step_rm(has_role("drop_vars")) %>% 
  step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>% 
  step_dummy(has_role("to_dummies")) %>%
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=ld_test)

vis_dat(train)

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

fit=lm(store~.,data=t1)
summary(fit)
vif(fit)

sort(vif(fit),decreasing = T)

summary(fit)

fit=stats::step(fit)

## AIC score 

summary(fit)

formula(fit)

## remove vars with vif higher than 10

for_vif=lm(store ~ sales0 + sales1 + sales2 + sales4 + CouSub + population + 
             state_alpha_GA + state_alpha_MA + state_alpha_ME + state_alpha_NH + 
             state_alpha_VT,
           data=t1)

summary(for_vif)

sort(vif(for_vif),decreasing = T)

#log_fit=glm(store~.-CouSub
#            -population
#            -sales4
#            -state_alpha_GA
#            -state_alpha_MA
#            -state_alpha_ME
#            -state_alpha_NH
#            -state_alpha_VT,
#            data=t1)

#summary(log_fit)

#log_fit=stats::step(log_fit)

#summary(log_fit)

#formula(log_fit)

log_fit=glm(store ~ sales4 + CouSub + population + state_alpha_GA + 
              state_alpha_MA + state_alpha_ME + state_alpha_NH + 
              state_alpha_VT,
            data=train)

summary(log_fit)

#### performance on t2 with auc score

train.score=predict(log_fit,newdata = train,type='response')

real=train$store
train.score

### now fitting model on the entire data

log_fit.final=glm(store ~ sales4 + CouSub + population + state_alpha_GA + 
                    state_alpha_MA + state_alpha_ME + state_alpha_NH + 
                    state_alpha_VT,
                  data=train)

summary(log_fit.final)

log_fit.final=stats::step(log_fit.final)

summary(log_fit.final)

m = measureit(score = round(train.score,3), class = real,
              measure = c("ACC", "SENS", "SPEC","PREC","FSCR"))

cutoff_data =data.frame(Cutoff = m$Cutoff,
                        TP=m$TP,
                        TN=m$TN,
                        FP=m$FP,
                        FN=m$FN, 
                        Depth = m$Depth,
                        Accuracy = m$ACC,
                        Sensitivity = m$SENS,
                        Specificity = m$SPEC, 
                        F1 = m$FSCR) %>% 
  mutate(P=TP+FN,
         N=TN+FP,
         KS=(TP/P)-(FP/N)) %>% 
  select(-P,-N) %>% 
  na.omit() %>% 
  arrange(Cutoff)


# Depth	:What portion of the observations fall on or above the cutoff.

#### visualize how these measures move across cutoffs

ggplot(cutoff_data,aes(x=Cutoff,y=KS))+geom_line()


cutoff_long=cutoff_data %>% 
  select(Cutoff,Accuracy:KS) %>% 
  gather(Measure,Value,Accuracy:KS)

ggplot(cutoff_long,aes(x=Cutoff,y=Value,color=Measure))+geom_line()

# KS plot

rocit = rocit(score = train.score, 
              class = real) 

kplot=ksplot(rocit)

# cutoff on the basis of KS

my_cutoff=kplot$`KS Cutoff`


# Lift Chart

gtable10 = gainstable(score = train.score, 
                      class = real, 
                      ngroup = 10)

print(gtable10)

plot(gtable10, type = 1)


### submission

test.prob.score= predict(log_fit.final,newdata = test,type='response')

write.csv(test.prob.score,"store_prediction_Submission3.csv",row.names = F)

test.predicted=as.numeric(test.prob.score>my_cutoff)

write.csv(test.predicted,"store_prediction_Submission3.csv",row.names = F)



