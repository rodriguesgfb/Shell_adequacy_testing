##Rarefaction
require(iNEXT)
require(tidyverse)
data<-read.csv("rare_site.csv", head = T, row.names = 1, sep =";")
rare<- colSums(data)
out2 <- iNEXT(data, q=0, datatype="abundance", endpoint = 200) 
ggiNEXT(out2) + theme_classic() + labs( y = "Richness")

out2$DataInfo # showing basic data information.
out2$AsyEst # showing asymptotic diversity estimates.
out2$iNextEst # showing diversity estimates with rarefied and extrapolated.


