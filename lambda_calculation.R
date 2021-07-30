

# set your working directory
setwd("C:/Users/james/Documents/Grad_school/Melissa_and_Emily//")

library(data.table)
library(dplyr)
library(tidyr)
library(popbio)
library(ggplot2)
library(car)

Surv.data <- fread("Surv_data.txt") # read in the data



# create a list of the data frames
Surv.data.ls <- split(Surv.data, f = Surv.data$Category)


# change the order of the list. Help from https://stackoverflow.com/questions/40615542/r-change-the-order-of-elements-in-a-list
Surv.data.ls <- Surv.data.ls[c("Naup", "Cope", "Adult")]





EPR.data <- fread("epr_data.txt")

#EPR.ls <- split(EPR.data, f = EPR.data$Treatment)


# create a brand new dataframe with dummy variables in the first row which will be deleted
lambda.results <- data.frame(naup.treatment = 0,
                             naup.surv = "dunno",
                             cope.treatment = "duh",
                             cope.surv = "sure",
                             adult.treatment = "whatever",
                             adult.surv = "huh",
                             fec.treatment = "dummy",
                             fec.value = "notsure",
                             lambda = "whocares")

#i=1
#j=1
#k=1
#l = 1

for(i in 1:length(Surv.data.ls$Naup$Prob.Surv)) {
  
  naup.surv.value <- Surv.data.ls$Naup$Prob.Surv[i]
  
  naup.treat <- Surv.data.ls$Naup$Treatment[i]
  
  
  for(j in 1:length(Surv.data.ls$Cope$Prob.Surv)) {
  
  cope.surv.value <- Surv.data.ls$Cope$Prob.Surv[j]
  
  cope.treat <- Surv.data.ls$Cope$Treatment[j]
  
  
   
   for(k in 1:length(Surv.data.ls$Adult$Prob.Surv)) {
    
    adult.surv.value <- Surv.data.ls$Adult$Prob.Surv[k]
    
    adult.treat <- Surv.data.ls$Adult$Treatment[k]  
  
    
    
    surv.vector <- c(naup.surv.value, cope.surv.value, adult.surv.value)
    
    
    surv.matrix <- diag(surv.vector)
  
    
    
    for(l in 1:length(EPR.data$Fecundity)) {
      
      fec.value <- EPR.data$Fecundity[l]
      
      fec.treat <- EPR.data$Treatment[l]
      
      zero <- matrix(0, nrow = 1, ncol = 2)
      
      fecundity.row <- c(zero, fec.value)
      
      leslie.matrix <- rbind(fecundity.row, surv.matrix)
      
      leslie.matrix[nrow(leslie.matrix)-1,ncol(leslie.matrix)] <- leslie.matrix[nrow(leslie.matrix), ncol(leslie.matrix)]
      
      matrix.final <- leslie.matrix[-nrow(leslie.matrix),]
      
      eigen.calcs <- eigen.analysis(matrix.final, zero = FALSE) # calculate the eigen values
      
      lambda.value <- eigen.calcs$lambda1 # extract the dominant eigen values
      
      lambda.row <- data.frame(naup.treatment = naup.treat,
                               naup.surv = naup.surv.value,
                               cope.treatment = cope.treat,
                               cope.surv = cope.surv.value,
                               adult.treatment = adult.treat,
                               adult.surv = adult.surv.value,
                               fec.treatment = fec.treat,
                               fec.value = fec.value,
                               lambda = lambda.value) # create a 1x2 data frame to add to the end of the final data frame
      # data frame has to have the same colnames in order to rbind
      
      colnames(lambda.row) <- colnames(lambda.results) # make sure the data frames have the same names
      
      lambda.results <- rbind(lambda.results, lambda.row)
      
    }
    
      
  
  
  }
  
  
  }
}



# remove the rows where treatments aren't equal
lambda.results1 <- lambda.results[lambda.results$naup.treatment==lambda.results$cope.treatment,]
lambda.results1 <- lambda.results1[lambda.results1$naup.treatment==lambda.results1$adult.treatment,]
lambda.results1 <- lambda.results1[lambda.results1$naup.treatment==lambda.results1$fec.treatment,]

nrow(lambda.results1)
View(lambda.results1)

lambda.results1$lambda <- as.numeric(lambda.results1$lambda)

fwrite(lambda.results1, file = "lambda_results.txt", sep = "\t")

lambda.results <- fread("lambda_results.txt")

lambda.mean <- lambda.results %>%
  group_by(naup.treatment) %>%
  summarise(mean = mean(lambda, na.rm = TRUE),
            sd = sd(lambda, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n-1)*se,
         upper.ci = mean + qt(1 - (0.05 / 2), n-1)*se)

fwrite(lambda.mean, file = "lambda_mean.txt", sep = "\t")


lambdaBoxplot <- ggplot(data = lambda.results, aes(naup.treatment, lambda))+
  geom_boxplot(lwd = 1.1, aes(fill = factor(naup.treatment)),
               outlier.size = 1.5)+ # can't make a continuous x axis with boxplots
  
  theme(legend.title = element_text(colour = "black", size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("blue", "red"))+
  theme_bw()+
  labs(y="Lambda", x="Treatment")+
  #scale_x_discrete(breaks = c(0,2,4))+
  scale_y_continuous(breaks = waiver(), minor_breaks = waiver())+
  #ylim(1.402, 1.405)+
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20))


lambdaBoxplot

starsspacing <-10

off <- max(lambda.mean$upper.ci)/starsspacing
starsheight <-max(lambda.mean[lambda.mean[,"naup.treatment"]=="Plastic",]$upper.ci)
starsheight + off


# create points with different fills and colors help from https://stackoverflow.com/questions/15965870/fill-and-border-colour-in-geom-point-scale-colour-manual-in-ggplot
lambdaPlotTotal <- ggplot(data = lambda.mean, aes(naup.treatment, mean))+
  
  geom_point(aes(colour = factor(naup.treatment),
                 fill = factor(naup.treatment)),
                 shape = 21,
                 size = 7,
             stroke = 0.7)+
             
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci, colour = factor(naup.treatment)), size=0.4, width=0)+
  
  scale_fill_manual(values = c("azure4", "white"))+
  
  scale_color_manual(values = c("azure4", "black"))+
  
  
  theme(legend.title = element_text(colour = "black", size=12))+
  theme_bw()+ 
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  
  
  labs(y="Lambda", x="Treatment")+
  
  scale_y_continuous(breaks = waiver(), minor_breaks = waiver())+
  #ylim(1.402, 1.405)+
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  
  geom_text(y=starsheight[1]+off,x=2,size=20, label = "***")

lambdaPlotTotal

ggsave(filename = "lambda_plot_bw.pdf", 
       plot = lambdaPlotTotal, 
       path = "C:/Users/james/Documents/Grad_school/Melissa_and_Emily//", 
       width = 8.264, height = 5.819, units = "in")

##### lambda stats #####

m <- glm(lambda~as.factor(naup.treatment), data = lambda.results)
summary(m)

Anova(m)



