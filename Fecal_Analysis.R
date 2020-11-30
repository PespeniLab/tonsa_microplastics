
##################################################################################################
######################################Fecal Data Analysis#########################################

library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggpubr)
library(lmerTest)
library(car)
library(rlang)
library(ggpubr)
library(nlme)
library(Matrix)
library(dplyr)
library(knitr)
library(lme4)

#Read in data table
data <- read.table("F1Feces.csv", header= TRUE, sep=",")
dat <- data
head(dat)

#Setting uniform colors of the figures 
colors <- c("azure4", "white")

#---------------------------------------------------------------
#---------------------------------------------------------------

## Plotting the effects of microplastics on fecal length

g1 <- ggplot(data=dat, mapping=aes(x=Treatment, y=poop.length)) + geom_boxplot() + 
  geom_boxplot(fill=colors) + theme_bw(base_size = 25) + 
  ylab (expression(paste("Fecal Length (",mu, m,")", sep=""))) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Setting replicate as a factor to allow us to make it a random effect
data$Replicate <- as.factor(dat$Replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
model <- lme(poop.length ~ Treatment, random=~1|Replicate, data=dat, method="REML")

summary(model)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(model)

g1

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

## Plotting the effects of microplastics on fecal width

g2 <- ggplot(data=dat, mapping=aes(x=Treatment, y=width)) + geom_boxplot() + 
  geom_boxplot(fill=colors) + theme_bw(base_size = 25) + 
  ylab (expression(paste("Fecal Width (",mu, m,")", sep=""))) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Setting replicate as a factor to allow us to make it a random effect
data$Replicate <- as.factor(dat$Replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
figure <- lme(width ~ Treatment, random=~1|Replicate, data=dat, method="REML")

summary(figure)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(figure)

g2

#-------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

## Plotting the effects of microplastics on fecal volume using previous length and width

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-10

off <- max(dat$Volume)/starsspacing
starsheight <-max(dat[dat[,"Treatment"]=="Plastic",]$Volume)
starsheight + off

#Getting to the plot
g3 <- ggplot(data=dat, mapping=aes(x=Treatment, y=Volume)) + geom_boxplot() + 
  geom_boxplot(fill=colors) + theme_bw(base_size = 14) + 
  ylab (expression(paste("Fecal Volume (",mu, m^3, " ",pellet^{-1}, ")", sep=""))) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) + 
  geom_text(y=starsheight+off,x=2,size=7, label = "***")

g3

#Setting replicate as a factor to allow us to make it a random effect
data$Replicate <- as.factor(dat$Replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
design <- lme(Volume ~ Treatment, random=~1|Replicate, data=dat, method="REML")

summary(design)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(design)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## Plotting the effects of microplastics on fecal sinking rates

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-10

off <- max(dat$sinking)/starsspacing
starsheight <-max(dat[dat[,"Treatment"]=="Plastic",]$sinking)
starsheight + off

#Getting to the plot
g4 <- ggplot(data=dat, mapping=aes(x=Treatment, y=sinking)) + geom_boxplot() +
 geom_boxplot(fill=colors) + theme_bw(base_size = 14) +
 ylab(expression("Sinking Rate" ~ (m ~ day^{-1}))) + xlab("Treatment") +
 theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) + 
 geom_text(y=starsheight[1]+off,x=2,size=7, label = "***")

g4

#Setting replicate as a factor to allow us to make it a random effect
data$Replicate <- as.factor(dat$Replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
sketch <- lme(sinking ~ Treatment, random=~1|Replicate, data=dat, method="REML")

summary(sketch)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(sketch)

#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Read in data table
data <- read.table("average_feces.csv", header= TRUE, sep=",")
daat <- data
head(daat)

## Average number of fecal pellets per individual

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-10

off <- max(daat$apoop)/starsspacing
starsheight <-max(daat[daat[,"treatment"]=="plastic",]$apoop)
starsheight + off

#Getting to the plot
g5 <- ggplot(data=daat, mapping=aes(x=treatment, y=apoop)) + geom_boxplot() +
  geom_boxplot(fill=colors) + theme_bw(base_size = 14) +
  ylab(expression("Number of Fecal Pellets" ~ (individual^{-1}))) + xlab("Treatment") +
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=c("Control","Plastic")) +
  geom_text(y=starsheight[1]+off,x=2,size=7, label = "*")

g5

#Setting replicate and "dish" as a factor to allow us to make it a random effect
#Animals were raised in separate replicates for plastic/control, then put into petri dishes to asses fecal properties
data$Replicate <- as.factor(daat$replicate)
data$dish <- as.factor(daat$dish)

#Data analysis with Linear Mixed Effects Model with replicate and dish as separate random effects
parade <- lme(apoop ~ treatment, random=~1|replicate/dish, data=daat, method="REML")

summary(parade)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(parade)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## Total fecal matter produced per individual (multiplying fecal volume and average number of fecal pellets per individual)

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-10

off <- max(daaat$total_volume)/starsspacing
starsheight <-max(daaat[daaat[,"treatment"]=="plastic",]$total_volume)
starsheight + off

#Getting to the plot
g6 <- ggplot(data=daaat, mapping=aes(x=treatment, y=total_volume)) + geom_boxplot() +
 geom_boxplot(fill=colors) + theme_bw(base_size = 14) +
 ylab(expression(paste("Total Fecal Matter (",mu, m^3," ",individual^{-1},")", sep=""))) + 
 theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
 scale_x_discrete(labels=c("Control","Plastic")) +
 geom_text(y=starsheight[1]+off,x=2,size=7, label = "***")

g6

#Setting replicate and "dish" as a factor to allow us to make it a random effect
#Animals were raised in separate replicates for plastic/control, then put into petri dishes to asses fecal properties
data$Replicate <- as.factor(daaat$replicate)
data$dish <- as.factor(daaat$dish)

#Data analysis with Linear Mixed Effects Model with replicate and dish as separate random effects
avatar <- lme(total_volume ~ treatment, random=~1|replicate/dish, data=daaat, method="REML")

summary(avatar)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(avatar)

#---------------------------------------------------------------------------

## Stitching all the figures together for the final plot
g3+g5+g6+g4

