library(tidyverse)

MechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

summary(lm(MechaCar$mpg ~ MechaCar$`vehicle length` + MechaCar$`vehicle weight` + MechaCar$`spoiler angle` + MechaCar$`ground clearance` + MechaCar$AWD,data=MechaCar)) #generate summary statistics

##Liner Model with Vehicle Length
model <- lm(MechaCar$`vehicle weight` ~ MechaCar$mpg,MechaCar) #create linear model

yvals <- model$coefficients["MechaCar$mpg"]*MechaCar$mpg + model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt20 <- ggplot(MechaCar,aes(x=mpg,y=`vehicle weight`)) #import dataset into ggplot2

plt20 + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model



##Suspension Coil Summary

Suspension <- read.csv(file = 'Suspension_Coil.csv')

summary(Suspension)

STDD = sd(Suspension$PSI)

Var = var(Suspension$PSI)


##Suspension Coil T-Test
suspension_sample_table <- Suspension %>% sample_n(50) #generate 50 randomly sampled data points
suspension_sample_table2 <- Suspension %>% sample_n(50) #generate another 50 randomly sampled data points

TSummary = t.test(log10(suspension_sample_table$PSI),log10(suspension_sample_table2$PSI)) #compare means of two samples

print(t.test(Suspension$PSI, mu=1500))

##Design your study

##Finding the cor-matrix for each
used_matrix <- as.matrix(MechaCar) #convert data frame into numeric matrix
print(cor(used_matrix))

#Shapiro
shapiro.test(used_matrix)


