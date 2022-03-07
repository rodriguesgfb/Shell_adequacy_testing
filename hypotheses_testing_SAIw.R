#Load packages
source("vaznull.R") #Function extracted from Vázquez DP, Melian CJ, Williams NM, Blüthgen N, Krasnov BR and Poulin R. 2007. Species abundance and asymmetric interaction strength in ecological networks. Oikos 116: 1120-1127.
require(bipartite)
require (tidyverse)

#Load data
SAI<- read.csv("SAILC.csv", head = T, row.names = 1, sep =";")
SAI<-data.matrix(SAI)
rede<- read.csv("matrix.csv", head = T, row.names = 1, sep =";")
rede<- data.matrix(rede)
match<- read.csv("po_matchLC.csv", head = T, row.names = 1, sep =";")
match<- data.matrix(match)
abd_po<- read.csv("po_abd.csv", head = T, row.names = 1, sep =";")
abd_po<- data.matrix(abd_po)

#To calculate observed mean weighted Shell adequacy index (SAI) 
observed_SAI<- mo.dist(rede, SAI)
observed_SAI[[1]] #mean
observed_SAI[[2]] #sd

##Simulate SAI values under each of the three proposed Hypothesis

#Neutral hypothesis testing
null_neutral<- vaznull(N = 10000, rede)
models_neutral<- unlist(sapply(null_neutral, mo.dist, SAI))
quantile(models_neutral[1,], c(0.025, 0.975)) #mean
quantile(models_neutral[2,], c(0.025, 0.975)) #sd


#Morphological match hypothesis testing
null_match <- vaznullmodif(N = 10000, web = rede, m.hyp = match)
models_match<- unlist(sapply(null_match, mo.dist, SAI))
quantile(models_match[1,], c(0.025, 0.975)) #mean 
quantile(models_match[2,], c(0.025, 0.975)) #median


##Abundance hypothesis testing
null_abd <- vaznullmodif(N = 10000, web = rede, m.hyp = abd_po)
models_abd<- unlist(sapply(null_abd, mo.dist, SAI))
quantile(models_abd[1,], c(0.025, 0.975)) #mean 
quantile(models_abd[2,], c(0.025, 0.975)) #median

##test for significance among models

#create dataframe with simulated values
models<- data.frame(SAI = c(models_abd[1,],models_match[1,], models_neutral[1,]), hypothesis = c(rep("Abundance", 10000), rep("Match", 10000), rep("Neutral", 10000)))

#Test for normality 
shapiro.test(models$SAI[1:5000])

#Test for Homoscedasticity 
library(car)
leveneTest(models$SAI, group = models$hypothesis)

#linear models testing
model<- lm(SAI ~ hypothesis, data = models)
summary(model)