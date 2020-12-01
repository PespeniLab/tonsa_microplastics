
##############################################################################################################
############################################Nauplii Data Analysis#############################################

#install.packages("devtools")
#install.packages("car")
#install.packages("carData")
#install.packages("magrittr")
#install.packages("rlang")
#install.packages("nlme")
#install.packages("lmerTest")

library(patchwork)
library(lmerTest)
library(ggplot2)
library(car)
library(rlang)
library(ggpubr)
library(nlme)
library(Matrix)
library(dplyr)
library(knitr)

#Read in data table
dat <- read.table("nauplii.csv", header= TRUE, sep=",")
head(dat)

#Setting uniform colors of the figures 
colors <- c("azure4", "white")

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

## The effects of microplastics on nauplii body length analysis

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-10

off <- max(dat$length)/starsspacing
starsheight <-max(dat[dat[,"treatment"]=="Plastic",]$length)
starsheight + off

#Getting to the figure

p1 <- ggplot(data=dat, mapping=aes(x=treatment, y=length)) + geom_boxplot() + 
  geom_boxplot(fill=colors) + theme_bw(base_size = 14) + 
  ylab (expression(paste("Body Length (",mu, m,")", sep=""))) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  geom_text(y=starsheight[1]+off,x=2,size=7, label = "*")
 
p1

#Setting replicate as a factor to allow us to make it a random effect
dat$replicate <- as.factor(dat$replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
model <- lme(length ~ treatment, random=~1|replicate, data=dat, method="REML")

summary(model)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(model)

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

## The effects of microplastics on nauplii body width

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-10

off <- max(dat$length)/starsspacing
starsheight <-max(dat[dat[,"treatment"]=="Plastic",]$width)
starsheight + off

#Getting to the plot
p2 <- ggplot(data=dat, mapping=aes(x=treatment, y=width)) + geom_boxplot() + 
  geom_boxplot(fill=colors) + theme_bw(base_size = 14) + 
  ylab (expression(paste("Body Width (",mu, m,")", sep=""))) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  geom_text(y=starsheight[1]+off,x=2,size=7, label = "***")
 
p2

#Setting replicate as a factor to allow us to make it a random effect
dat$replicate <- as.factor(dat$replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
avatar <- lme(width ~ treatment, random=~1|replicate, data=dat, method="REML")

summary(avatar)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(avatar)

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

## Nauplii survival after a 5 day exposure period using a Generalized Linear Mixed Effects Model

#Read in the table
data <- read.table("Nauplii_GLM.csv", header= TRUE, sep=",")
daat <- data
head(data)

#Setting replicate as a factor to allow us to make it a random effect
data$replicate <- as.factor(daat$replicate)

#Data analysis with Generalized Linear Mixed Effects Model with replicate as a random effect
modelrandomm <- glmer(success ~ treatment + (1 | replicate), data = daat, binomial)

summary(modelrandomm) 
#------------------------------------------------------
## Figure for nauplii survival after a 5 day microplastic exposure

#Read in the data
data <- read.table("nauplii_survival.csv", header= TRUE, sep=",")
head(data)

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-10

off <- max(data$survival)/starsspacing
starsheight <-max(data[data[,"Treatment"]=="Plastic",]$survival)
starsheight + off

#Plotting the data
p3 <- ggplot(data=data, mapping=aes(x=Treatment, y=survival)) + geom_boxplot() + 
  geom_boxplot(fill=colors) + theme_bw(base_size = 14) + ylab("Survival (%)") + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  geom_text(y=starsheight+off,x=2,size=7, label = "**")
  
p3

#--------------------------------------------------------
#--------------------------------------------------------

#Stitching the figures together
p3+p1+p2
