#Gender-based disparities in the business sector of developing countries.
#set workdiercation
setwd("C:/Users/mb/Desktop/FINAL.PROJECT DATA")
load("C:/Users/mb/Desktop/FINAL.PROJECT DATA/BPRdata_R_Full (1).RData"
attach(BPRdata)

##  Summary statistics ##
summary(country)

table(country, male)

indust_indx <- factor((manuf + 2*services + 3*trade), levels=c(1,2,3),labels = c("manuf","services","trade"))
summary (indust_indx)
table(country, indust_indx)
summary(ownerage[bangladesh == 1])

summary(ownerage[kenya == 1])

summary(ownerage[mexico == 1])

summary(ownerage[srilanka == 1])

summary(ownerage[nigeria == 1])

summary(ownerage[chile == 1])
summary(ownerage[ghana == 1])
summary(monthlyprofit)

##Logit. Female likelihood in the different sectors  ###
#Dependant variable:
 male - Owner is male
#Independent variables:
manuf -Sector is manufacturing
services -Sector is services
trade - Sector is trade
cropandanimal -Sector is crop and animal

logitSectorMale<-glm(male~manuf+services+trade+cropandanimal, family = binomial(link = "logit"))
logitSectorFemale<-glm(!male~manuf+services+trade+cropandanimal, family = binomial(link = "logit"))
library("stargazer", lib.loc="~/R/win-library/3.3")
stargazer(logitSectorMale, logitSectorFemale, type="text", title=" Female likelihood in the different sectors")
##Logit model average marginal effects. Female likelihood in different sectors.
#Code for female:
logitSectorFemale<-glm(!male~manuf+services+trade+cropandanimal, family = binomial(link = "logit"))
LogitMAME<-mean(dlogis(predict(logitSectorFemale, type = "link")))
LogitMAME*coef(logitSectorFemale)
#Code for male:
logitSectorMale<-glm(male~manuf+services+trade+cropandanimal, family = binomial(link = "logit"))
LogitMAMEMale<-mean(dlogis(predict(logitSectorMale, type = "link")))
LogitMAMEMale*coef(logitSectorMale)

##Does size matter?###
#Dependant variable:
V20 monthlyprofit - Monthly profit
#Independent variables:
capitalstock - Capital stock
paidworkers - Number of paid workers
inventories - Value of inventories
monthlysales - Sales in the last month

modelsize<-lm(monthlyprofit~capitalstock+paidworkers+inventories+monthlysales)

modelsizemale<-lm(monthlyprofit[male==1]~capitalstock[male==1]+paidworkers[male==1]+inventories[male==1]+monthlysales[male==1])

modelsizefemale<-lm(monthlyprofit[male==0]~capitalstock[male==0]+paidworkers[male==0]+inventories[male==0]+monthlysales[male==0])

stargazer(modelsize, modelsizemale, modelsizefemale, type="text")

##Lm Interaction. Does more work hours translate to more profits? ##
#Dependant variable:
monthlyprofit - Monthly profit
  
#Independent variables:
ownershours - Hours worked by owner in last week interacted with male -Owner is male

ownershours - Hours worked by owner in last week interacted with Owner is female
ModelPOH<-lm(monthlyprofit~ownershours:male+ownershours:!male)
summary(ModelPOH)

##Logit: Are female run businesses more profitable than male ones? ##

#Dependant variables:
male -Owner is male
female-Owner is female

#Independent variables:
 monthlyprofit - Monthly profit

logitMoreProfitMale<-glm(male~monthlyprofit, family = binomial(link = "logit")) 
logitMoreProfitFemale<-glm(!male~monthlyprofit, family = binomial(link = "logit"))
library("stargazer", lib.loc="~/R/win-library/3.3")
stargazer(logitMoreProfitMale, logitMoreProfitFemale, type = "text", title = "Male/Female business profitability likelihood")

##Profit and education level ###

plot(monthlyprofit[male==1]~ednyears[male==1], ylim=c(0,10000000), xlab = "Education Years for Males", ylab = "Profit for Males", main="Profit per Years of Educaton for Males" )
lines(lowess(monthlyprofit[male==1]~ednyears[male==1], f=0.5, delta=0.0), col="red")

plot(monthlyprofit[male==0]~ednyears[male==0], ylim=c(0,10000000), xlab = "Education Years for Females", ylab = "Profit for Females", main="Profit per Years of Educaton for Females" )
lines(lowess(monthlyprofit[male==0]~ednyears[male==0], f=0.5, delta=0.0), col="red")

## #FINANCIAL PLANING##
#The dependent variable:
  .	female/male owner. 
#The independent variables :
reviewing the financial performance monthly(bp_f1).
having a target set for sales over the next year (bp_f2).
comparing their sales achieved to their target at least monthly(bp_f3).
having a budget of the likely costs their business will face over the next year (bp_f4).
preparing profit and loss statement (bp_f5).
preparing cashflow statement (bp_f6). 
preparing balance sheet (bp_f7).
preparing income and expenditure statement (bp_f8).

ibrary("car")
Female<-Recode(male, '1=0;0=1')
#logit
#to see how female performance in workplace compare to the male
logit2<-glm(Female ~ bp_f1 + bp_f2 + bp_f3 + bp_f4 + bp_f5 + bp_f6 + bp_f7 +bp_f8, family = binomial(link = "logit"))
summary(logit2)
logit3<-glm(male ~ bp_f1 + bp_f2 + bp_f3 + bp_f4 + bp_f5 + bp_f6 + bp_f7 +bp_f8, family = binomial(link = "logit"))
summary(logit3)
require(stargazer)
stargazer(logit2, logit3,  type = "text", title="female/male parformance in Financial Planning", digits=1, out="table1.txt")
# linear regrssion  and interaction
model5 <- lm(monthlyprofit ~ Female + bp_f1 + bp_f2 + bp_f3 + bp_f4 + bp_f5 + bp_f6 + bp_f7 +bp_f8 + bp_f1:Female + bp_f2:Female + bp_f3:Female + bp_f4:Female + bp_f5:Female + bp_f6:Female + bp_f7:Female +bp_f8:Female + ednyears + bp_f1:ednyears:Female + bp_f2:ednyears:Female + bp_f3:ednyears:Female + bp_f4:ednyears:Female + bp_f5:ednyears:Female + bp_f6:ednyears:Female + bp_f7:ednyears:Female + bp_f8:ednyears:Female)
summary(model5)
model6 <- lm(monthlyprofit ~ male + bp_f1 + bp_f2 + bp_f3 + bp_f4 + bp_f5 + bp_f6 + bp_f7 +bp_f8 + bp_f1:male + bp_f2:male + bp_f3:male + bp_f4:male + bp_f5:male + bp_f6:male + bp_f7:male +bp_f8:male + ednyears + bp_f1:ednyears:male + bp_f2:ednyears:male + bp_f3:ednyears:male + bp_f4:ednyears:male + bp_f5:ednyears:male + bp_f6:ednyears:male + bp_f7:ednyears:male + bp_f8:ednyears:male)
summary(model6)
stargazer(model5, model6,  type = "text", title="linear regrassion Financial Planning as modive for monthely profit male/female ", digits=1, out="table1.txt")
model9 <- lm(monthlysales ~ Female + ageoffirm +  Female:ageoffirm + bp_m1 + bp_m2 + bp_m3 + bp_m4 + bp_m5 + bp_m6 + bp_m7 + bp_m1:Female + bp_m2:Female + bp_m3:Female + bp_m4:Female + bp_m5:Female + bp_m6:Female + bp_m7:Female + + bp_m1:Female:inventories + bp_m2:Female:inventories + bp_m3:Female:inventories + bp_m4:Female:inventories + bp_m5:Female:inventories + bp_m6:Female:inventories + bp_m7:Female:inventories)
summary(model9)

## Marketing ##
#The Dependent variable:
Monthly sales
#The Independent variables:
Visited competitor's business to see prices (bp_m1)
Visited competitor's business to see products (bp_m2)
Asked existing customers what other products they should offer (bp_m3)
Talked with former customer to see why stopped buying (bp_m4)
Asked supplier what products selling well (bp_m5)
Used a special offer to attract customers (bp_m6)
Have done advertising in last 6 months (bp_m7) 
Value of inventories

model9 <- lm(monthlysales ~ Female + ageoffirm +  Female:ageoffirm + bp_m1 + bp_m2 + bp_m3 + bp_m4 + bp_m5 + bp_m6 + bp_m7 + bp_m1:Female + bp_m2:Female + bp_m3:Female + bp_m4:Female + bp_m5:Female + bp_m6:Female + bp_m7:Female + + bp_m1:Female:inventories + bp_m2:Female:inventories + bp_m3:Female:inventories + bp_m4:Female:inventories + bp_m5:Female:inventories + bp_m6:Female:inventories + bp_m7:Female:inventories)
summary(model9)
model10 <- lm(monthlysales ~ male + ageoffirm +  male:ageoffirm + bp_m1 + bp_m2 + bp_m3 + bp_m4 + bp_m5 + bp_m6 + bp_m7 + bp_m1:male + bp_m2:male + bp_m3:male + bp_m4:male + bp_m5:male + bp_m6:male + bp_m7:male + + bp_m1:male:inventories + bp_m2:male:inventories + bp_m3:male:inventories + bp_m4:male:inventories + bp_m5:male:inventories + bp_m6:male:inventories + bp_m7:male:inventories)
summary(model10)
stargazer(model9,  type = "text", title="linear regrassion of Marketing as modive for monthely sales female owners ", digits=5, out="table1.txt")
stargazer(model9,  type = "text", title="linear regrassion of Marketing as modive for monthely sales male owners ", digits=5, out="table1.txt")

#Linear relationship between Explanatory Variables
Y <- cbind(monthlyprofit)
X <- cbind(!male, ownerage,  I(ownerage^2), ednyears, inventories, ownershours, bus_indx)

olsreg = lm(Y~X)
summary(olsreg)
plot(olsreg)
abline(olsreg)

---------------------------
# Misc codes to help us with our analysis
# female in manuf
fraction_manuf <- manuf/!male
fraction_manuf1 <- is.finite(fraction_manuf)

summary(fraction_manuf[fraction_manuf1])
modelmanuf <- lm(monthlyprofit[fraction_manuf1] ~ ownerage[fraction_manuf1] + monthlysales[fraction_manuf1] + ednyears[fraction_manuf1] + bus_indx[fraction_manuf1] + training_treat[fraction_manuf1])
summary(modelmanuf)
summary(monthlyprofit[fraction_manuf1])

# simple regression of female in service. 
fraction_serv <-monthlysales/allpaidhours
fraction_serv1 <- is.finite(fraction_serv)
summary(fraction_serv[fraction_serv1])
modelserv <- lm(!male[fraction_serv1] ~ ownerage[fraction_serv1] + inventories[fraction_serv1] + ednyears[fraction_serv1] + manuf[fraction_serv1] + trade[fraction_serv1] + capitalstock[fraction_serv1] + ageoffirm[fraction_serv1])
summary(modelserv)
summary(ownerage[fraction_serv1])
summary(monthlyprofit[fraction_serv1])
summary(ednyears[fraction_serv1])
summary(training_treat[fraction_serv1])


 # Fraction of females int eh service sector in each country
fraction_serv[is.infinite(fraction_serv)] <- NA
fraction_serv[fraction_serv<0] <- NA
summary(fraction_serv)
summary(fraction_serv[as.logical(kenya)])
summary(fraction_serv[as.logical(bangladesh)])
summary(fraction_serv[as.logical(nigeria)])
summary(fraction_serv[as.logical(chile)])
summary(fraction_serv[as.logical(srilanka)])
summary(fraction_serv[as.logical(ghana)])
summary(fraction_food[as.logical(white & !Hispanic)])

fraction_manuf[is.infinite(fraction_manuf)] <- NA
fraction_manuf[fraction_serv<0] <- NA 
summary(fraction_manuf)
summary(fraction_manuf[as.logical(kenya)])
summary(fraction_manuf[as.logical(bangladesh)])
summary(fraction_manuf[as.logical(nigeria)])
summary(fraction_manuf[as.logical(chile)])
summary(fraction_manuf[as.logical(srilanka)])

summary(!male[as.logical(ghana)])

summary(!male[as.logical(nigeria)])
