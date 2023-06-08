library(nlme)
library(ggplot2)
library(data.table)
library(purrr)
library(dplyr)

zip_cbsa_an2<-fread("C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/zip_cbsa_an2.csv", header=TRUE, drop=1)

zip_cbsa_an2$CBSA<-as.character(zip_cbsa_an2$CBSA)  #This needs to be a character variable so that we can use it as a random effect

## Center the data


center_this<-function(x) {
  x-mean(x,na.rm=TRUE)
}

centered_df<-zip_cbsa_an2 %>% 
  group_by(CBSA) %>%
  mutate(median_value_cbsa = center_this(median_value),
         median_income_cbsa = center_this(median_income),
         per_over65_cbsa = center_this(per_over65),
         per_own_home_cbsa = center_this(per_own_home),
         per_live_alone_cbsa = center_this(per_live_alone),
         per_pov_cbsa = center_this(per_pov),
         per_HI_cbsa = center_this(per_HI),
         per_white_nothis_cbsa = center_this(per_white_nothis),
         per_black_cbsa = center_this(per_black),
         per_hispanic_cbsa = center_this(per_hispanic),
         per_other_combined_cbsa = center_this(per_other_combined),
         per_public_trans_cbsa = center_this(per_public_trans),
         no_college_cbsa = center_this(no_college))

#check centering  ##check all with lapply?
sum(by(centered_df$median_value_cbsa, centered_df$CBSA, mean))
sum(by(centered_df$median_income_cbsa, centered_df$CBSA, mean))

scaled_df<-centered_df %>% 
  mutate(median_value_sc = median_value_cbsa/sd(median_value_cbsa),
         median_income_sc = median_income_cbsa/sd(median_income_cbsa),
         per_over65_sc = per_over65_cbsa/sd(per_over65_cbsa),
         per_own_home_sc = per_own_home_cbsa/sd(per_own_home_cbsa),
         per_live_alone_sc = per_live_alone_cbsa/sd(per_live_alone_cbsa),
         per_pov_sc = per_pov_cbsa/sd(per_pov_cbsa),
         per_HI_sc = per_HI_cbsa/sd(per_HI_cbsa),
         per_white_nothis_sc = per_white_nothis_cbsa/sd(per_white_nothis_cbsa),
         per_black_sc = per_black_cbsa/sd(per_black_cbsa),
         per_hispanic_sc = per_hispanic_cbsa/sd(per_hispanic_cbsa),
         per_other_combined_sc = per_other_combined_cbsa/sd(per_other_combined_cbsa),
         per_public_trans_sc = per_public_trans_cbsa/sd(per_public_trans_cbsa),
         no_college_sc = no_college_cbsa/sd(no_college_cbsa))


write.csv(scaled_df,"C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/sesvar_cbsa_norm.csv")

##Mixed effect model with random-slope

# Percent Public Transportation
model<-lme(ann_an_rate100k~per_public_trans_sc, random =~1+per_public_trans_sc|CBSA, data = scaled_df)
#model shows the random effects at each CBSA with percent of population that take public transportation to work. ~1 is used to get random intercepts
summary(model)
## Check Assumptions
# Homogeneity
plot(model)
#Normality
hist(model$residuals, main="Residual Distribution for model")
qqnorm(model$residuals)
qqline(model$residuals)

rans<-ranef(model)  #conditional means of the random effects based on public transportation (centered at zero) 
rans
plot(rans[,1]~rans[,2])
slopes<-rans[,2]+0.78069 #add per_public_trans coefficient to random effects 
hist(slopes)
## Most are  > 0. Increase in percent using public transportation increases the burden (attributable number)
mean(rans[,2])



######## Mixed Effects for each CBSA
#### Extract coefficient and standard error for each CBSA
CBSA_uni<-unique(scaled_vars$CBSA)
coef_se<-data.frame(matrix(nrow=1,ncol=4))
for(i in 1:120) {
CBSA<-CBSA_uni[i]
cbsa_df<-subset(scaled_vars, CBSA_uni[i]==scaled_vars$CBSA)
n<-nrow(cbsa_df)
model_lin<-lm(ann_an_rate100k~per_public_trans, data=cbsa_df)
coef<-coef(model_lin)[[2]]
se<-summary(model_lin)$coefficients[2,2]
coef_se[i,]<-cbind(CBSA,coef,se,n)
}
colnames(coef_se)<-c("CBSA","coef","se","n")
mean(coef_se$coef) #3.01
mean(coef_se$se) #4.94
### Mean shows that the model without random slopes underestimates the SE of public transportation leading to a 
### larger t-statistic and a smaller p-value where type I errors are more likely to occur. (false positives)
### observations violate the independence assumption in the linear regression model

model2<-lme(ann_an_rate100k~per_public_trans, random =~1|CBSA, data = scaled_vars)  #mixed effects without random slope
## Between CBSA variation = 21.57 (intercept) and within CBSA is 9.43 (residual)
## Correlation = 0.016 (Between intercept and slope of fixed effects)
## BIC = 57664
model1<-lm(ann_an_rate100k~per_public_trans, data=scaled_vars) #linear regression
model3<-lme(ann_an_rate100k~per_public_trans, random =~1+per_public_trans|CBSA, data = scaled_vars) #mixed effects with random slope
## between CBSA variation is 22.93? or 5.989 (StdDev of Intercept or variable), within CBSA variation is 9.11 (StdDeve of Residual)
## Corr = 0.796 (between intercept and slope of random effect)
## Correlation = 0.67 (between intercept and slope of fixed effect)
## BIC = 57244
## Plot of random effect intercept and slope shows strong correlation (0.8)
## running a linear model with each CBSA shows that the SE within each CBSA is larger than is shown in the fixed effects of the mixed model
anova(model2,model1,model3)

##Check assumptions. Transformations? Other Models? 
## Look at fixed effects confidence intervals for model with and without random slope. Does it change the CI or estimate values? 
## When the estimates change, it shows that the grouping effect is significant to the fixed effect? 
## Doing this for all variables will show which variables can be used to characterize CBSAs?
## Is the mixed effect model greatly affected by unbalanced groups? lme() handles unbalanced groups well, but 
## check the confidence intervals for the random effects variation. Abnormally large indicated bad fit. 

ggplot(scaled_vars,aes(y=ann_an_rate100k, x=per_public_trans, color=as.factor(CBSA))) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  theme(legend.position="none")

## Slope of each CBSA generally fan out indicating a positive correlation between intercept and slope and many show a slope of zero. 

model4<-lm(ann_an_rate100k~CBSA/per_public_trans, data=scaled_vars) ## nested model? Unbalanced groups

#### transformed data
hist(scaled_vars$per_public_trans)
log_df<-zip_cbsa_an2
log_df$per_public_trans<-log_df$per_public_trans + 0.0001
log_df$per_public_trans<-log(log_df$per_public_trans,base=10)
hist(log_df$per_public_trans)
plot(log_df$per_public_trans,log_df$ann_an_rate100k)

model5<-lme(ann_an_rate100k~per_public_trans,random =~1+per_public_trans|CBSA, data=log_df)
summary(model5)
plot(model5)
rans4<-ranef(model5)
plot(rans4[,1]~rans4[,2],ylab="Intercept",xlab="Slope", main="Random Effects based on Public Transportation")

ggplot(log_df,aes(y=ann_an_rate100k, x=per_public_trans, color=as.factor(CBSA))) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  theme(legend.position="none")

qqnorm(model5$residuals)
qqline(model5$residuals)

model6<-lm(ann_an_rate100k~per_public_trans, data=log_df)
summary(model6)
BIC(model6)

#### No improvement in the model with transformed predictor variable, but correlation (0.52) between random effects intercept and slope slightly improves
#### Within CBSA variation increases (9.34) and between CBSA variation decreases (1.93). 
#### Correlation between intercept and slope of fixed effects decreased to (0.431)
#### Intercept and slope of fixed effects decreased


## model misspecification: are there other factors that are not being considered within the CBSA? (neighborhoods, zip codes, etc)
## not possible to make smaller groupings unless we have repeated data (etc. multiple years?) However, we do not have multiple years
## for the response (right?)

## Should I pull the confidence intervals from the fixed effects or the random effects? 
## How to increase to multiple independent variables? Does trend change?

############################################################################
######### Models with classes designate by LPA #############################
############################################################################
##### Models with class as designated by LPA
scaled_vars<-zip_cbsa_an2 %>% mutate_at(c(6:20), ~scale(.))
scaled_vars<-as.data.frame(scaled_vars)

scaled_vars$median_value<-scaled_vars$median_value[,1]
scaled_vars$median_income<-scaled_vars$median_income[,1]
scaled_vars$per_over65<-scaled_vars$per_over65[,1]
scaled_vars$per_over65y2<-scaled_vars$per_over65y2[,1]
scaled_vars$per_own_home<-scaled_vars$per_own_home[,1]
scaled_vars$per_rent_home<-scaled_vars$per_rent_home[,1]
scaled_vars$per_live_alone<-scaled_vars$per_live_alone[,1]
scaled_vars$per_pov<-scaled_vars$per_pov[,1]
scaled_vars$per_HI<-scaled_vars$per_HI[,1]
scaled_vars$per_white_nothis<-scaled_vars$per_white_nothis[,1]
scaled_vars$per_black<-scaled_vars$per_black[,1]
scaled_vars$per_hispanic<-scaled_vars$per_hispanic[,1]
scaled_vars$per_other_combined<-scaled_vars$per_other_combined[,1]
scaled_vars$per_public_trans<-scaled_vars$per_public_trans[,1]
scaled_vars$no_college<-scaled_vars$no_college[,1]

randfor<-scaled_vars[,-c(1:5,9,11)]

set.seed(907)
randfor[,-14] %>%
  single_imputation() %>%
  estimate_profiles(4) %>% 
  plot_profiles()

m4 <- randfor[,-14] %>%
  single_imputation() %>% 
  estimate_profiles(4)

est<-as.data.frame(get_data(m4))

est %>% count(Class)  

##merge class data with variables, delete unneeded output for models

## check the merge because more observations were added
## 18 rows are duplicated in est, but zero duplicated in scaled_vars. Est has lost zip code information. 
## Going to try dropping the dupicated rows and adding the columns based on rownames to see if it changes the analysis
df_to_join<-unique(est)
class_vars<-left_join(scaled_vars, df_to_join, multiple="all")
class_vars<-class_vars[,-c(22:27)]

class_vars2<-merge(scaled_vars, est, by="row.names", all.x=TRUE)
var_dup<-subset(class_vars2,class_vars2$per_black.x!=class_vars2$per_black.y)  #look for mistakes in merging by row numbers
var_dup<-subset(class_vars2,class_vars2$median_value.x!=class_vars2$median_value.y)
var_dup<-subset(class_vars2,class_vars2$per_public_trans.x!=class_vars2$per_public_trans.y)
rm(var_dup)
class_vars2<-class_vars2[,-c(1,23:41)]

## Mixed effect with random intercept
model8<-lme(ann_an_rate100k~as.factor(Class), random =~1|CBSA, data = class_vars)
summary(model8)
## Mixed effect with random intercept and slope
model9<-lme(ann_an_rate100k~as.factor(Class), random =~1+as.factor(Class)|CBSA, data = class_vars)
summary(model9)
## all classes significant in model8 for the fixed effects, but class4 is not significant for the fixed effect in model9

##### Try models with class_vars2 to see if the datasets give the same results (Should be exactly the same)
model10<-lme(ann_an_rate100k~as.factor(Class), random =~1|CBSA, data = class_vars2)
summary(model10)
## Mixed effect with random intercept and slope
model11<-lme(ann_an_rate100k~as.factor(Class), random =~1+as.factor(Class)|CBSA, data = class_vars2)
summary(model11)
##### They are exactly the same, the merges produced the same dataset.Summary statistics are the same with scaled_vars, so no data lost
model12<-lme(ann_an_rate100k~as.factor(Class)-1, random =~1+as.factor(Class)|CBSA, data = class_vars2)
summary(model11)

rans10<-ranef(model10)
rans11<-ranef(model11)
plot(rans11[,1]~rans11[,2],ylab="Intercept",xlab="Slope Class 2", main="Random Effects based LPA Class")
ggplot(rans11, aes(x=rans11[,1], y=rans11[,2])) + geom_point(color = "blue") +
  geom_point(aes(x=rans11[,1],y=rans11[,3]), color = "red") +
  geom_point(aes(x=rans11[,1],y=rans11[,4]),color="green")

## Mixed effects model with random intercept and slope has lower BIC (lowest of all the models so far), but the 
## random slopes are correlated with the random intercept (class2 is negative, class 3 and 4 are positive)
## Class 4 is not different enough from the other classes? (Class 4 highly correlated with class2) 
## Intercept and the slopes are highly correlated negatively. 

plot(model11, CBSA~fitted(.))
confid<-intervals(model11, which="fixed")
confid<-data.frame(confid$fixed)
fit<-fitted(model11)

confidenceInt<-function(confid) {
  confid2<-confid
  confid2[2]<-confid[1]+confid[2]
  confid2[3]<-confid[3]+confid[1]
  confid2[4]<-confid[4]+confid[1]
  return(confid2)
}

confid2<-confid %>% mutate(
  lower = confidenceInt(confid$lower),
  est. = confidenceInt(confid$est.),
  upper = confidenceInt(confid$upper)
)
## Which is the correct way to plot the confidence intervals? Do you need to calculate the actual values with the Cov2Cov() 
## Calculate by hand
ggplot(data=confid2, aes(y=reorder(rownames(confid2),est.), x=est., xmin=lower, xmax=upper)) + 
  geom_point() +
  geom_errorbarh(height=.1) +
  labs(title = "Fixed Effect 95% Confidence Intervals") + xlab("Mean Estimated Coefficient")
geom_vline(xintercept=0, color = "black", linetype = "dashed", alpha = .5) +
  theme(axis.title.y=element_blank())

ggplot(data=confid, aes(y=reorder(rownames(confid),est.), x=est., xmin=lower, xmax=upper)) + 
  geom_point() +
  geom_errorbarh(height=.1) +
  labs(title = "Fixed Effect 95% Confidence Intervals") + xlab("Mean Estimated Coefficient") +
geom_vline(xintercept=0, color = "black", linetype = "dashed", alpha = .5) +
  theme(axis.title.y=element_blank())
## model without fixed effect intercept
plot(model12, CBSA~fitted(.))
confid<-intervals(model12, which="fixed")
confid<-data.frame(confid$fixed)
fit2<-fitted(model12)

ggplot(data=confid, aes(y=reorder(rownames(confid),est.), x=est., xmin=lower, xmax=upper)) + 
  geom_point() +
  geom_errorbarh(height=.1) +
  labs(title = "Fixed Effect 95% Confidence Intervals") + xlab("Mean Estimated Coefficient") +
  geom_vline(xintercept=0, color = "black", linetype = "dashed", alpha = .5) +
  theme(axis.title.y=element_blank())

class2<-subset(class_vars, Class==2)
summary(class2)
class3<-subset(class_vars, Class==3)
summary(class3)
summary(class_vars)

ggplot(class_vars, aes(x=ann_an_rate100k, y=no_college, color=as.factor(Class))) + geom_point()
ggplot(class_vars, aes(x=ann_an_rate100k, y=median_value, color=as.factor(Class))) + geom_point()
ggplot(class_vars, aes(x=ann_an_rate100k, y=median_income, color=as.factor(Class))) + geom_point()
ggplot(class_vars, aes(x=ann_an_rate100k, y=per_public_trans, color=as.factor(Class))) + geom_point()

plot(class_vars2$ann_an_rate100k~class_vars2$Class)

model12<-lme(ann_an_rate100k~per_public_trans.x+median_value.x, random=~1+per_public_trans.x+median_value.x|CBSA, data=class_vars2)
summary(model12)
## Lowest BIC so far. 


#### Box Plot summaries of class
library(reshape2)

ggplot(melt(class_vars2[,-c(1:5,9,11,18,21)],id.var="Class")) + 
  geom_boxplot(aes(as.factor(Class), value, color=as.factor(Class))) + 
  facet_wrap(~variable) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank()) +
  scale_color_discrete(name="Class") +
  scale_y_continuous(expand = c(0,0))

### Multiple predictors
model13<-lme(ann_an_rate100k~per_public_trans+per_pov+per_over65+median_value, random=~1+per_public_trans+per_pov+per_over65+median_value|CBSA, data=scaled_vars)
summary(model13)

## Lower BIC, but some p-values are not significant


## All variables in a linear regression model
model14<-lm(ann_an_rate100k~.,data=randfor)
summary(model14)
BIC(model14)
### High BIC with all variable showing significance, except no_college and per_pov. Public transportation shows the largest effect with 
### percent white. Factor scores model did not show effect of public transportation, neither did class model. 

model15<-lme(ann_an_rate100k~median_value.x+median_income.x+per_over65.x+per_own_home.x+per_live_alone.x+per_pov.x+
               per_HI.x+per_white_nothis.x+per_black.x+per_hispanic.x+per_other_combined.x+per_public_trans.x+
               no_college.x,random = ~1|CBSA, data = class_vars2)
summary(model15)

model16<-lme(ann_an_rate100k~median_value.x+median_income.x+per_over65.x+per_own_home.x+per_live_alone.x+per_pov.x+
               per_HI.x+per_white_nothis.x+per_black.x+per_hispanic.x+per_other_combined.x+
               no_college.x,random = ~1|CBSA, data = class_vars2)
summary(model16)

########## Cross-validation without public transportation and with ###########
##############################################################################

#without Public Transportation
set.seed(43)
train<-sample(1:nrow(class_vars2),nrow(class_vars2)*.9)
model17<-lme(ann_an_rate100k~median_value.x+median_income.x+per_over65.x+per_own_home.x+per_live_alone.x+per_pov.x+
               per_HI.x+per_white_nothis.x+per_black.x+per_hispanic.x+per_other_combined.x+
               no_college.x,random = ~1|CBSA, data = class_vars2[train,])
yhat<-predict(model17, newdata=class_vars2[-train,])
class_test<-class_vars2[-train,"ann_an_rate100k"]
plot(yhat,class_test)
abline(0,1)
mean((yhat-class_test)^2)          ###66.35
sqrt(mean((yhat-class_test)^2))    ###8.15

#with Public Transportation
model18<-lme(ann_an_rate100k~median_value.x+median_income.x+per_over65.x+per_own_home.x+per_live_alone.x+per_pov.x+
               per_HI.x+per_white_nothis.x+per_black.x+per_hispanic.x+per_other_combined.x+
               no_college.x+per_public_trans.x,random = ~1|CBSA, data = class_vars2[train,])
yhat<-predict(model18, newdata=class_vars2[-train,])
class_test<-class_vars2[-train,"ann_an_rate100k"]
plot(yhat,class_test)
abline(0,1)
mean((yhat-class_test)^2)            ###65.0811
sqrt(mean((yhat-class_test)^2))      ###8.07

## fixed effect model with public transportation
model19<-lm(ann_an_rate100k~., data = randfor[train,])
yhat<-predict(model19, newdata=randfor[-train,])
class_test<-randfor[-train,"ann_an_rate100k"]
plot(yhat,class_test)
abline(0,1)
mean((yhat-class_test)^2)            ###593.01
sqrt(mean((yhat-class_test)^2))      ### 24.35

## With factor scores
model20<-lme(ann_an_rate100k~MR1+MR2+MR3+MR4, random=~1+MR1+MR2+MR3+MR4|CBSA, data=scaled_vars[train,])
yhat<-predict(model20, newdata=scaled_vars[-train,])
scaled_test<-scaled_vars[-train,"ann_an_rate100k"]
plot(yhat,scaled_test)
abline(0,1)
mean((yhat-scaled_test)^2)            ###64.28
sqrt(mean((yhat-scaled_test)^2))      ###8.02

## With Classes
yhat<-predict(model11, newdata=class_vars2[-train,])
class_test<-class_vars2[-train,"ann_an_rate100k"]
plot(yhat,class_test)
abline(0,1)
mean((yhat-class_test)^2)            ###64.78
sqrt(mean((yhat-class_test)^2))      ###8.05