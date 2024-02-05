# Code used to simulate the distribution of SAI under three different hypotheses: (1) null hypothesis; (2) Forbidden links and (3) Morphological match.

source("vaznull.R") #Function extracted from Vázquez DP, Melian CJ, Williams NM, Blüthgen N, Krasnov BR and Poulin R. 2007. Species abundance and asymmetric interaction strength in ecological networks. Oikos 116: 1120-1127.

#load packages
require(bipartite)
require (tidyverse)

#Probability match/mismatch function
function_match<- function(SAI){
  for(i in SAI)
    out<- 1 - (abs(log(SAI))/(1+abs(log(SAI))))
  return(out)
}

##Forbidden links funtion
function_forbidden<- function(SAI){
  for(i in SAI) {
      out<- ifelse(i < 0.25 || i > 1.75, 0, 1)
      }
    return(out)
}

#Load data
SAI<- read.csv("SAI_matrix.csv", head = T, row.names = 1, sep =";") #Matrix of Shell adequacy index of each interaction 
SAI<-data.matrix(SAI)
net<- read.csv("final_matrix.csv", head = T, row.names = 1, sep =";") #Matrix of hermit crab - gastropod interaction
net<- data.matrix(rede)
match<- function_match(SAI) #probability matrix based on morphological match/mismatch
fl<- apply(SAI, 1:2,function_forbidden) #probability matrix based on forbidden links
abd_po<- read.csv("po_abd_final.csv", head = T, row.names = 1, sep =";")
abd_po<- data.matrix(abd_po) #probability matrix based on abundance (neutral hypothesis)

#To calculate observed mean weighted Shell adequacy index (SAI) 
observed_SAI<- mo.dist(rede, SAI)
observed_SAI[[1]] #mean
observed_SAI[[2]] #sd

##Simulate SAI values under each of the three proposed Hypothesis

#Neutral hypothesis testing (Abundance)
null_abd <- vaznullmodif(N = 10000, web = rede, m.hyp = abd_po)
models_abd<- unlist(sapply(null_abd, mo.dist, SAI))
quantile(models_abd[1,], c(0.025, 0.975)) #mean 
quantile(models_abd[2,], c(0.025, 0.975)) #sd


#Morphological match hypothesis testing
null_match <- vaznullmodif(N = 10000, web = rede, m.hyp = match)
models_match<- unlist(sapply(null_match, mo.dist, SAI))
quantile(models_match[1,], c(0.025, 0.975)) #mean 
quantile(models_match[2,], c(0.025, 0.975)) #sd


##Forbidden links hypothesis testing
forbidden_links <- vaznullmodif(N = 10000, web = rede, m.hyp = fl)
models_fl<- unlist(sapply(forbidden_links, mo.dist, SAI))
quantile(models_fl[1,], c(0.025, 0.975)) #mean 
quantile(models_fl[2,], c(0.025, 0.975)) #sd
