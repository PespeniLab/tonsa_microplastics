
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
install.packages(tidyverse)

png("fecal_properties.png", height=7, width=9.75, units="in", res=300)
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

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
## Plotting the effects of microplastics on fecal volume using previous length and width

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-5.8

off <- max(dat$Volume)/starsspacing
starsheight <-max(dat[dat[,"Treatment"]=="Plastic",]$Volume)
starsheight + off

#Getting to the plot
g3 <- ggplot(data=dat, mapping=aes(x=Treatment, y=Volume)) +
  geom_boxplot(fill=colors, outlier.size=2.6) +
  theme_bw(base_size = 22) + 
  xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) + 
  geom_text(y=starsheight+off,x=2,size=9, label = "***") +
  labs(tag = "A", y = expression(atop("Fecal Volume", 
                                      paste("(", 
                                            mu, 
                                            m^3,
                                            " ",
                                            pellet^-1,
                                            ")")))) +
  theme(plot.tag = element_text(vjust = 2))

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

#Read in data table
data2 <- read.table("F1Feces_New.csv", header= TRUE, sep=",")

head(data2)

##To get significance asterisks uniformly displayed in all plots

#The maximum value for each data frame was divided by a uniform value, 10, to display the asterisks 
#the same distance away from each outlier or whiskers
starsspacing <-5.8

off <- max(data2$sinking)/starsspacing
starsheight <-max(data2[data2[,"treatment"]=="plastic",]$sinking)
starsheight + off

#Getting to the plot
g4 <- ggplot(data=data2, mapping=aes(x=treatment, y=sinking)) + 
  geom_boxplot(fill=colors, outlier.size=2.6) +
 theme_bw(base_size = 22) +
 theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) + 
 geom_text(y=starsheight[1]+off,x=2,size=9, label = "***") +
 scale_x_discrete(labels=c("Control", "Plastic")) +
 labs(tag = "D", y = expression(atop("Sinking Rate", 
                                     paste("(", 
                                           m, 
                                           " ",
                                           day^-1,
                                           ")")))) +
 theme(plot.tag = element_text(vjust = 2))

g4

#Setting replicate as a factor to allow us to make it a random effect
data$replicate <- as.factor(data2$replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
sketch <- lme(sinking ~ treatment, random=~1|replicate, data=data2, method="REML")

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
starsspacing <-5.8

off <- max(daat$apoop)/starsspacing
starsheight <-max(daat[daat[,"treatment"]=="plastic",]$apoop)
starsheight + off

#Getting to the plot

g5 <- ggplot(data=daat, mapping=aes(x=treatment, y=apoop)) + 
  geom_boxplot(fill=colors, outlier.size=2.6) + 
  theme_bw(base_size = 22) +
  theme(axis.title.y=element_text(hjust=0.5)) +
  xlab("Treatment") +
  ylab(expression(atop("Number of Fecal", paste("Pellets", " ", (individual^{-1})))))+
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=c("Control","Plastic")) +
  geom_text(y=starsheight[1]+off,x=2,size=9, label = "*") +
  labs(tag = "B") +
  theme(plot.tag = element_text(vjust = 2))

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
starsspacing <5.8

off <- max(daat$total_volume)/starsspacing
starsheight <-max(daat[daat[,"treatment"]=="plastic",]$total_volume)
starsheight + off
                        
#Getting to the plot
g6 <- ggplot(data = daat, mapping = aes(x=treatment, y=total_volume)) + 
  geom_boxplot(fill=colors, outlier.size=2.6) +
  geom_text(y = 2000000, x = 2, size = 9, label = "***") +
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), axis.title.x = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("Control", "Plastic")) +
  labs(tag = "C", y = expression(atop("Total Fecal Matter", 
                           paste("(", 
                                 mu, 
                                 m^3,
                                 " ",
                                 individual^-1,
                                 ")")))) +
  theme(plot.tag = element_text(vjust = 2))
 
g6

#Setting replicate and "dish" as a factor to allow us to make it a random effect
#Animals were raised in separate replicates for plastic/control, then put into petri dishes to asses fecal properties
data$Replicate <- as.factor(daat$replicate)
data$dish <- as.factor(daat$dish)

#Data analysis with Linear Mixed Effects Model with replicate and dish as separate random effects
avatar <- lme(total_volume ~ treatment, random=~1|replicate/dish, data=daat, method="REML")

summary(avatar)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(avatar)

#---------------------------------------------------------------------------

## Stitching all the figures together for the final plot
g3+g5+g6+g4

dev.off()
