#setting working directory
setwd("C:/Users/CeX/Desktop/SCHOOL/3rd YEAR/Microeconomics/project")

#Loading libraries
library(readr)
library(Hmisc)
library(haven)
library(dplyr)
library(corrplot)
library(ggplot2)#EDA and Visualisation
library(ggthemes)# EDA
library(DMwR2)
library(tidyr)
library(sandwich)
library(lmtest)

crime.data <- read_dta("csew1314_teaching_eul2.dta")
crime.data

dem.crime <- crime.data[,c("nadults","sex","age","nchil","yrsarea","mthsaddr","work2","hominsur","tothhin2","inner","ethgrp2a","club", "infstdy2")]
att.crime <- crime.data[,c("mottheft","worryx","motstole","cardamag","yrhotry","homethef","yrhostol",
                         "yrdeface","persthef","delibdam","delibvio","ndelibv","threviol","sexattak","hhldviol","bcsvictim")]
CJS.crime <- crime.data[,c("ratpol2","polatt1","polatt2","polatt3","polatt5","polatt6","polatt7","ratpol3")]
ASB.crime <- crime.data[,c("vandcomm","poorhou")]

#merging into one dataset and removing rows with many NAs
crime.data <- tibble(cbind(dem.crime,att.crime,CJS.crime,ASB.crime))# merging datasets into one column

crime.data <- crime.data%>%filter(complete.cases(worryx)) #Narrowing down the dataset to module C
crime.data


crime.data <- crime.data[ , colSums(is.na(crime.data))/nrow(crime.data)<0.1]#removing columns with >10% NA


crime.data <- knnImputation(crime.data, k = 10)# replacing NAs with most similar 10 case values #this code does not change the data much and therefore will not affect results.

#Correlations
names(crime.data)

#crime.data <- crime.data[,-c(4,5,12, 14,17,19,21,23,25,26,28)] #These columns are statistically insignficant. Removing them reduces the dataset and allows the correlation plot to look quite neat

#Plotting correlation

cor.cr1 <- print(cor(crime.data))
corrplot(cor.cr1, type="upper", tl.pos="d")
corrplot(cor.cr1, add=TRUE, type="lower", method="number", diag=FALSE, tl.pos="n", cl.pos="n")

cor.cr1["worryx",]# Analysing correlation between worryx and each variable in the dataset
  ##no significant correlation found.


# Creating dammy (d) VARIBLES for the categorical data
crime.dt <- crime.data%>%mutate(fem_d =  as.numeric(crime.data$sex==2),
                              viol_d =   as.numeric(crime.data$threviol==1) ,
                              stol_d =   as.numeric(crime.data$yrhostol==1) ,
                              work_d =   as.numeric(crime.data$work2==1) ,
                              prel_d = as.numeric(crime.data$polatt1 >=3& crime.data$polatt1<=4),
                              innc_d =   as.numeric(crime.data$inner==1) ,
                              pdam_d =   as.numeric(crime.data$delibdam==1),
                              whit_d = as.numeric(crime.data$ethgrp2a ==1),
                              club_d = as.numeric(crime.data$club ==1),
                              cexp_d = as.numeric(crime.data$bcsvictim==1),
                              pfair_d = as.numeric(crime.data$polatt3==1),
                              pcof_d = as.numeric(crime.data$polatt7==1),
                              pers_d = as.numeric(crime.data$persthef==1),
                              insu_d = as.numeric(crime.data$hominsur==2),
                              phou_d = as.numeric(crime.data$poorhou<=2))


#Correlation among our test variables

cor.cd <- cor(crime.dt[, -c(4:19)])
corrplot(cor.cd, type="upper", tl.pos="d")
corrplot(cor.cd, add=TRUE, type="lower", method="number", diag=FALSE, tl.pos="n", cl.pos="n")
corrplot(cor.cd, title(main = "Correlation between Variables of Interest"))
  ##There's a perfect correlation between sex and its dammy variable. Omit the sex variable in the OLS model

#Exploratory Data Analysis
summary(crime.dt)

ggplot(aes(y = worryx, x = nadults), data = subset(crime.dt, age<=120))+
  geom_point (color = "blue")+
  labs(title = "Monthly Average Cyclying Duration",
       x = "Numbber of Adults",
       y = "Level of Worry")+
  theme(panel.background = element_rect(fill="white")) 
  ##No obvious relationship between household adults number and crime worry
                              
                              
#Correlations among the dammy variables variables
ncol(crime.dt)

x <- print(cor(crime.dt[,20:34]))
symnum(x)

corrplot(x, type="upper", tl.pos="d")
corrplot(x, add=TRUE, type="lower", method="number", diag=FALSE, tl.pos="n", cl.pos="n")
  ##There is no strong correlation among the dammys to cause co-linearity and affect our OSL regression

#Regressions
#Model Version 1
AICworryx <- c()
reg1 <- lm(worryx ~ age + nadults + fem_d + viol_d + stol_d + work_d + prel_d + innc_d + pdam_d + 
          whit_d + club_d + cexp_d + pfair_d + pcof_d + pers_d + insu_d + phou_d, data=crime.dt)

summary(reg1)

AIC(reg1)
AICworryx <- c(AICworryx, Model.One = AIC(reg1))

anova(reg1)

#Model Version2

reg2 <- lm(worryx ~ age + nadults + fem_d + viol_d + prel_d + innc_d + whit_d +
             club_d + cexp_d + pfair_d + pcof_d + pers_d + phou_d, data=crime.dt) # removed stol_d, work_d, pdam_d and insu_d

summary(reg2)

AIC(reg2)
AICworryx <- print(c(AICworryx, Model.Two = AIC(reg2)))
anova(reg1,reg2)

anova(reg2)# recommends removing nadults and prel_d

#Model Versipon 3
reg3 <- lm(worryx ~ age + fem_d + viol_d + innc_d +whit_d + insu_d+ nadults + prel_d +
             club_d + cexp_d + pfair_d + pcof_d + pers_d + phou_d, data=crime.dt) # Added insu_d
summary(reg3)

AIC(reg3)
AICworryx <- c(AICworryx, Model.Three = AIC(reg3))
anova(reg2,reg3)

anova(reg3)

#Model Final version
reg4 <- lm(worryx ~ age + nadults + fem_d + viol_d + work_d + prel_d + innc_d + whit_d + 
             club_d + cexp_d + pfair_d + pcof_d + pers_d + insu_d + phou_d, data = crime.dt) # Added work_d

summary(reg4)

AIC(reg4)
AICworryx <- c(AICworryx, Model.Four = AIC(reg4))
anova(reg3,reg4)

print(AICworryx)# Viewing

#Testing for heteroscedasticity
#studentized Breusch-Pagan test
bptest(reg4)
  ##Data is heteroscedastic

#Adjusting for heteroscedasticity
coeftest(reg4, vcov = vcovHC(reg4, type="HC1"))
summary(reg4)

anova(reg1,reg4)

#reset tests
#Null = Our model better explains the data
resettest(reg1)
resettest(reg2)
resettest(reg3)
resettest(reg4) 
  ##P-value = 0.08523; cannot reject null hypothesis

#Method 2 #Factoring
y <- as_factor(crime.data)
y <- mutate(y, age = as.double(age))
print(y)

#Model 1
AICs <- c()
lm.y <- lm(worryx~., data=y)
summary(lm.y)
AIC(lm.y)
AICs <- c(AICs,AIC(lm.y))
anova(lm.y)

#Model 2
lm.y2 <- lm(worryx ~sex + age + work2 + hominsur + inner + ethgrp2a + club + persthef +
              threviol + bcsvictim + polatt1 + polatt3 + polatt7 + vandcomm, data = y)
summary(lm.y2)

AIC(lm.y2)
AICs <- print(c(AICs,AIC(lm.y2)))
anova(lm.y,lm.y2)
anova(lm.y2)

#Model 3
final.lm <- lm(worryx ~ nadults + sex + age + work2 + inner + ethgrp2a + 
                    club + persthef + threviol + bcsvictim + polatt1 + polatt3 + 
                    polatt7 + vandcomm, data = y)#drop hominsur #add nadults

summary(final.lm)

anova(lm.y2, final.lm)
AIC(final.lm)
AICs <- c(AICs,AIC(final.lm))
names(AICs) <- c("Model 1", "Model 2", "Model 3")
print(AICs)

