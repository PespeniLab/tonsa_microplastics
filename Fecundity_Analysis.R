
#############################################################################################################
######################################Fecundity and Egg Data Analysis########################################

#install.packages("devtools")
#install.packages("car")
#install.packages("carData")
#install.packages("magrittr")
#install.packages("rlang")
#install.packages("nlme")
#install.packages("lmerTest")

library(patchwork)
library(magrittr)
library(lmerTest)
library(ggplot2)
library(car)
library(rlang)
library(ggpubr)
library(nlme)
library(Matrix)
library(dplyr)
library(knitr)
library(lme4)

png("fecundity.png", height=4, width=11, units="in", res=300)

#Setting uniform colors of the figures 
colors <- c("azure4", "white")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## Differences in total fecundity (nauplii and unhatched eggs)

#Read in data table
data <- read.table("CopepoditesAdults.csv", header= TRUE, sep=",")
datt <- data
head(datt)

g1 <- ggplot(data=datt,mapping=aes(x=Treatment,y=Fecundity)) + 
  geom_boxplot(fill=colors, outlier.size=2.6) +
  theme_bw(base_size = 22) + 
  ylab(expression("Fecundity" ~ (48 ~ hr^{-1}))) + xlab("Treatment") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  labs(tag = "B") +
  theme(plot.tag = element_text(vjust = 2))

g1

#Setting replicate as a factor to allow us to make it a random effect
data$Replicate <- as.factor(datt$Replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
model <- lme(Fecundity ~ Treatment, random=~1|Replicate, data=datt, method="REML")

summary(model)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(model)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

## Copepodite survival to the adult stage data analysis using a Generalized Linear Mixed Effects Model

#Read in data frame
data <- read.table("GLM egg data.csv", header= TRUE, sep=",")
datt <- data

#Setting replicate as a factor to allow us to make it a random effect
data$Replicate <- as.factor(datt$Replicate)

#Data analysis with Generalized Linear Mixed Effects Model with replicate as a random effect
modelrandom <- glmer(Success ~ Treatment + (1 | Replicate), data = datt, binomial)

#analysis
summary(modelrandom)
#-----------------------------------------
## Figure for copepodites that survived to the adult stage

#Read in the data
data <- read.table("copepodite_survival.csv", header= TRUE, sep=",")
dat <- data
head(dat)

#Getting to the plot
g2 <- ggplot(data=dat, mapping=aes(x=Treatment,y=survival)) + geom_boxplot() + 
  geom_boxplot(fill=colors, outlier.size=2.6) +
  theme_bw(base_size = 22) + ylab("Survival (%)") + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  labs(tag = "A") +
  theme(plot.tag = element_text(vjust = 2))

g2

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

## Effects of microplastics on egg diameter

#Read in the data
data <- read.table("diameter.csv", header= TRUE, sep=",")
dat <- data
head(dat)

#Getting to the plot
g3 <- ggplot(data=dat, mapping=aes(x=Treatment, y=diameter)) + geom_boxplot() + 
  geom_boxplot(fill=colors, outlier.size=2.6) + 
  theme_bw(base_size = 22) + xlab("Treatment") + 
  ylab (expression(paste("Egg Diameter (",mu, m,")", sep=""))) + geom_boxplot(fill=colors) + 
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), panel.grid.minor = element_blank()) +
  geom_text(y=85,x=2,size=10, label = "***") + ylim(58,90) +
  labs(tag = "C") +
  theme(plot.tag = element_text(vjust = 2))

g3

#Setting replicate as a factor to allow us to make it a random effect
data$replicate <- as.factor(dat$replicate)

#Data analysis with Linear Mixed Effects Model with replicate as a random effect
design <- lme(diameter ~ Treatment, random=~1|replicate, data=dat, method="REML")

summary(design)

#Linear Mixed Effects Models don't provide a P value, thus an Anova (Type 11 Chi Square test) was utilized
Anova(design)

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

## Stitching all the figures together for the final plot
g2+g1+g3

dev.off()


