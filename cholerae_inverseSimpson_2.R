library(readxl)
library(dplyr)
#Cleaning up the raw data to get the CDR3b sequences for the respective patients

my_data <- read_xlsx("SEGD44.xlsx", col_types = "text", col_names = TRUE)
new_data<-my_data[,c(2,5)]

new_data1<-new_data%>%filter(CDR3b != "NA")


#Getting the individual CDR3b sequences


library(dplyr)
P1<-new_data1 %>% filter(Patient_info == "SEGD44 Day 180 (LPL)")
P2<-new_data1 %>% filter(Patient_info == "SEGD44 Day 180 (PBMC)")


P1<-P1[,2]
P2<-P2[,2]


# Random sampling of the CDR3b sequences. Take 19 samples (19 is the lowest number of CDR3b sequence for the samples)
#and sample them randomly for 1000 times without replacement

P1R<-replicate(n=10000, expr={P1[sample(nrow(P1),19,replace=F),]},simplify=F)
P2R<-replicate(n=10000, expr={P2[sample(nrow(P2),19,replace=F),]},simplify=F)


# Counting the number of times each sequence occurs in each random sample

f2<-function(x){table(unlist(x))}

P1R <-lapply(P1R, f2)
P2R <-lapply(P2R, f2)

#Finding the diversity for each randomly selected groups of 19 sequences

library(vegan)
f3=function(x){diversity(x,"invsimpson")}
result1<-lapply(P1R, f3)
result2<-lapply(P2R, f3)


P1M<-unlist(result1)
P2M<-unlist(result2)


#Finding the median diversity value
Diversity1<-median(P1M)
Diversity2<-median(P2M)




                                  


