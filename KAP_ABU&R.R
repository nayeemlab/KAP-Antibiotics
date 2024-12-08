library(MASS)
require(foreign)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)



setwd('E:\\ResearchProject\\Mizba')
data_abur <- read.csv("KAP_ABU&R.csv")

#Sex
x <- table(data_abur$Sex)
x
round(prop.table(x),4)*100


#Age groups
x <- table(data_abur$Age)
x
round(prop.table(x),4)*100


#Education
x <- table(data_abur$Education)
x
round(prop.table(x),4)*100


#Study Area
x <- table(data_abur$study.area)
x
round(prop.table(x),4)*100


#Faculty
x <- table(data_abur$faculty_cat)
x
round(prop.table(x),4)*100


#University.Types
x <- table(data_abur$University.Types)
x
round(prop.table(x),4)*100

#Parents.have.Medical.Background
x <- table(data_abur$Parents.have.Medical.Background)
x
round(prop.table(x),4)*100


#Home.town
x <- table(data_abur$Home.town)
x
round(prop.table(x),4)*100


#Family.type
x <- table(data_abur$Family.type)
x
round(prop.table(x),4)*100



#Knowledge
x <- table(data_abur$X1..Antibiotics.are.supposed.to.kill.all.bacteria.in.the.body_CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X2..Antibiotics.are.effective.for.the.treatment.of.bacterial.infections_CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X3..Antibiotics.are.effective.for.the.treatment.of.viral.infections_CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X4..Antibiotic.resistance.is.the.loss.of.activity.of.an.antibiotic.._CA)
x
round(prop.table(x),4)*100

x <- table(data_abur$X5..Antibiotic.resistance.can.be.caused.by.the.over.use.of.antibiotics.._CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X6..Is.antibiotic.resistance.an.important.issue.._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X6..Is.antibiotic.resistance.an.important.issue.._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X7..Is.anyone.in.your.household.taking.antibiotics.at.the.moment._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X8..People.travelling.outside.country.risk.bringing.resistance.to.Bangladesh.._CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X9..Resistance.can.spread.from.animals.to.humans.._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X10..Resistance.can.spread.from..person.to.person......_CA)
x
round(prop.table(x),4)*100

median(data_abur$Knowledge)

data_abur$KnowledgeCat[data_abur$Knowledge < 7]  = 0
data_abur$KnowledgeCat[data_abur$Knowledge >= 7]  = 1

data_abur$KnowledgeCat <- factor(data_abur$KnowledgeCat,levels=c(0,1),
                                 labels = c('Poor','Good'))
data_abur$KnowledgeCat

x <- table(data_abur$KnowledgeCat)
x
round(prop.table(x),4)*100





#Attitude
x <- table(data_abur$X1..Nowadays..Antibiotic.resistance.is.a.serious.concern.in.the.Bangladesh_CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X2..Nowadays..Antibiotic.resistance.has.become.a.major.issue.all.over.the.world_CA)
x
round(prop.table(x),4)*100




x <- table(data_abur$X3..Do.you.genuinely.think.we.should.become.more.concerned.about.antibiotic.use._CA)
x
round(prop.table(x),4)*100




x <- table(data_abur$X4..More.awareness.should.be.taken.to.overcome.antibiotic.resistance_CA)
x
round(prop.table(x),4)*100




x <- table(data_abur$X5..Do.you.believe.doctors.are.often.prescribe.antibiotics.unnecessarily._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X6..Is.antibiotic.resistance.an.important.issue.._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X7..The.government.should.increase.more.awareness.regarding.antibiotic.resistance_CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X8..People.travelling.outside.country.risk.bringing.resistance.to.Bangladesh.._CA)
x
round(prop.table(x),4)*100



median(data_abur$Attitude)

data_abur$AttitudeCat[data_abur$Attitude < 7]  = 0
data_abur$AttitudeCat[data_abur$Attitude >= 7]  = 1

data_abur$AttitudeCat <- factor(data_abur$AttitudeCat,levels=c(0,1),
                                 labels = c('Poor','Good'))
data_abur$AttitudeCat

x <- table(data_abur$AttitudeCat)
x
round(prop.table(x),4)*100







#Practice

x <- table(data_abur$X1..Have.you.ever.taken.antibiotics._CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X2..Do.you.taken.antibiotics.during.Covid.19.._CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X3..How.do.you.generally.take.antibiotics...check.as.required._CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X4..When.do.you.generally.take.antibiotics..check.as.required.._CA)
x
round(prop.table(x),4)*100




x <- table(data_abur$X5..How.many.times.have.you.consumed.antibiotics.during.the.past.12.months._CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X6..How.many.times.have.another.adult.in.your.household..over.18.years.old..received.antibiotics.during.the.past.12.months._CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X7..Is.anyone.in.your.household.taking.antibiotics.at.the.moment._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X8..What.illness.symptoms.have.you.had.in.the.last.month._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X9..What.have.you.taken.your.last.illness_CA)
x
round(prop.table(x),4)*100



x <- table(data_abur$X10..Do.you.fail.to.complete.the.doses.of.antibiotic...If.yes.what.are.the.causes.of.incomplete.medication._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X11..Have.you.taken.any.antibiotics.within.the.last.six.months._CA)
x
round(prop.table(x),4)*100


x <- table(data_abur$X12..Have.you.ever.faced.antibiotic.resistance._CA)
x
round(prop.table(x),4)*100



median(data_abur$Practice)

data_abur$PracticeCat[data_abur$Practice < 7]  = 0
data_abur$PracticeCat[data_abur$Practice >= 7]  = 1

data_abur$PracticeCat <- factor(data_abur$PracticeCat,levels=c(0,1),
                                labels = c('Poor','Good'))
data_abur$APracticeCat

x <- table(data_abur$PracticeCat)
x
round(prop.table(x),4)*100


#######################################Knowledge CrossTabs#####################################


c <- table(data_abur$Sex ,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)


c <- table(data_abur$Age ,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Education,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$study.area ,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$University.Types ,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Parents.have.Medical.Background ,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Home.town ,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Family.type ,data_abur$KnowledgeCat)
c
round(prop.table(c,1)*100,2)
summary(c)


#adjusted model
model <- glm(relevel(factor(data_abur$KnowledgeCat), ref = "Poor")~ 
               relevel(factor(data_abur$Sex), ref = "Female")
             + relevel(factor(data_abur$Age), ref = "16-25 Years")
             + relevel(factor(data_abur$Education), ref = "Higher Secondary")
             + relevel(factor(data_abur$Parents.have.Medical.Background), ref = "No")
             + relevel(factor(data_abur$Home.town), ref = "Urban"),
             family=binomial(link='logit'),data=data_abur)
summary(model)
exp(cbind(coef(model), confint(model)))















#######################################Attitude CrossTabs#####################################


c <- table(data_abur$Sex ,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)


c <- table(data_abur$Age ,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Education,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$study.area ,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$University.Types ,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Parents.have.Medical.Background ,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Home.town ,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Family.type ,data_abur$AttitudeCat)
c
round(prop.table(c,1)*100,2)
summary(c)


#adjusted model
model <- glm(relevel(factor(data_abur$AttitudeCat), ref = "Poor")~ 
               relevel(factor(data_abur$Sex), ref = "Female")
             + relevel(factor(data_abur$Education), ref = "Higher Secondary")
             + relevel(factor(data_abur$study.area), ref = "Non-biology  Students")
             + relevel(factor(data_abur$Parents.have.Medical.Background), ref = "No")
             + relevel(factor(data_abur$Home.town), ref = "Urban"),
             family=binomial(link='logit'),data=data_abur)
summary(model)
exp(cbind(coef(model), confint(model)))








#######################################Practice CrossTabs#####################################


c <- table(data_abur$Sex ,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)


c <- table(data_abur$Age ,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Education,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$study.area ,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$University.Types ,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Parents.have.Medical.Background ,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Home.town ,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(data_abur$Family.type ,data_abur$PracticeCat)
c
round(prop.table(c,1)*100,2)
summary(c)


#adjusted model
model <- glm(relevel(factor(data_abur$PracticeCat), ref = "Poor")~ 
               relevel(factor(data_abur$Sex), ref = "Female")
             + relevel(factor(data_abur$Education), ref = "Higher Secondary")
             + relevel(factor(data_abur$study.area), ref = "Non-biology  Students")
             + relevel(factor(data_abur$Parents.have.Medical.Background), ref = "No")
             + relevel(factor(data_abur$Home.town), ref = "Urban"),
             family=binomial(link='logit'),data=data_abur)
summary(model)
exp(cbind(coef(model), confint(model)))
