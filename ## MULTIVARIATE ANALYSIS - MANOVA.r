## MULTIVARIATE ANALYSIS - MANOVA - R SELF NOTES

#########################################
## Hotelling T^2
#########################################

library(ICSNP)

math.teach <- data.frame(teacher = factor(rep(1:2,
c(3, 6))), satis = c(1, 3, 2, 4, 6, 6, 5,5, 4), know = c(3, 7, 2, 6, 8, 8, 10, 10, 6))
math.teach
with(math.teach, tapply(know, teacher, mean))
with(math.teach, tapply(satis, teacher, mean))

par(mfrow = c(2, 1))
boxplot(know ~ teacher, math.teach, main = "Teacher Knowledge",horizontal = T)
boxplot(satis ~ teacher, math.teach, main = "Teacher Satisfaction",horizontal = T)

m1 <- with(math.teach, HotellingsT2(cbind(satis,know) ~ teacher))
m1
qf(.95, df1=2, df2=5) 

#########################################
## One-Way MANOVA
#########################################

manova.data <- data.frame(group = as.factor(rep(1:3,c(4, 3, 5))), y1 = c(2, 3, 5, 2, 4, 5, 6,
 7, 8, 10, 9, 7), y2 = c(3, 4, 4, 5, 8, 6,7, 6, 7, 8, 5, 6))

manova.data

with(manova.data, tapply(y1, group, mean))
with(manova.data, tapply(y2, group, mean))

par(mfrow = c(2, 1))
boxplot(y1 ~ group, manova.data, main = "y1 Boxplot",horizontal = T)
boxplot(y2 ~ group, manova.data, main = "y2 Boxplot",horizontal = T)

m1 <- manova(cbind(y1, y2) ~ group, manova.data)
m1
summary(m1, test = "Wilks")
summary.aov(m1) 

#########################################
## Two-Way MANOVA with Interaction
#########################################

## There are two factors, listed as RATE and ADDITIVE in the production test (each with two levels, Low and High)
## and a total of 20 runs. There are 3 response variables, tear, gloss and opacity, 
## which describe measured characteristics of the resultant film.

tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
Y <- cbind(tear, gloss, opacity)  
rate     <- gl(2,10, labels = c("Low", "High"))
additive <- gl(2, 5, length = 20, labels = c("Low", "High"))

dataset <- cbind(rate,additive,Y)
dataset

par(mfrow = c(3, 1))
boxplot(tear ~ rate, dataset, main = "tear Boxplot",horizontal = T)
boxplot(gloss ~ rate, dataset, main = "gloss Boxplot",horizontal = T)
boxplot(opacity ~ rate, dataset, main = "opacity Boxplot",horizontal = T)

par(mfrow = c(3, 1))
boxplot(tear ~ additive, dataset, main = "tear Boxplot",horizontal = T)
boxplot(gloss ~additive, dataset, main = "gloss Boxplot",horizontal = T)
boxplot(opacity ~ additive, dataset, main = "opacity Boxplot",horizontal = T)

fit <- manova(Y ~ rate * additive)
summary(fit, test = "Wilks") # ANOVA table of Wilks' lambda
summary.aov(fit)             # univariate ANOVA tables

#########################################
## Hotelling T^2 (example)
#########################################

library(Hotelling)
data(container.df)
container.df
apply(container.df[1:10,],2,mean)
apply(container.df[11:20,],2,mean)
fit = hotelling.test(.~gp, data = container.df)
fit


######################################################
## Box’s M-test for homogeneity of covariance matrices
######################################################

library(biotools)

a=boxM(math.teach[,-1], math.teach[,1])
a
a$pooled

boxM(manova.data[,-1], manova.data[,1])

tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
Y <- cbind(tear, gloss, opacity)  
rate     <- gl(2,10, labels = c("Low", "High"))
dataset <- cbind(rate,Y)
dataset

boxM(dataset[,-1],dataset[,1])

###############################################################

