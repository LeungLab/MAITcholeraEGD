library(readxl)
library(dplyr)
#Cleaning up the raw data to get the CDR3b sequences for the respective patients
my_data <- read_xlsx("Cholerae raw data_Ioana.xlsx", col_types = "text", col_names = TRUE)
new_data<-my_data[,-c(1,2,6,7,8,9,11,14,15,20,21,22,23,24,25,26,29,35,36,37,38,39,40,41,42,43,44,45)]

new_data$Patient_info<-do.call(paste0, new_data[c(1,2,3)])
new_data1<-new_data[,-c(1,2,3)]


new_data1<-new_data1%>%filter(CDR3b != "NA", Patient_info != "NANANA" )
new_data2<-new_data1[,c(26,3)]

#Getting the individual CDR3b sequences


library(dplyr)
P1<-new_data2 %>% filter(Patient_info == "SEGD  [LPL]46d2")
P2<-new_data2 %>% filter(Patient_info == "SEGD[PBMC]46d2")
P3<-new_data2 %>% filter(Patient_info == "SEGD [LPL]45d180")
#P4<-new_data2 %>% filter(Patient_info == "SEGD46d2")
P5<-new_data2 %>% filter(Patient_info == "SEGD46d7")
P6<-new_data2 %>% filter(Patient_info == "SMIC262d90")
P7<-new_data2 %>% filter(Patient_info == "SMIC271d90")
P8<-new_data2 %>% filter(Patient_info == "SMIC288d2")
P9<-new_data2 %>% filter(Patient_info == "SMIC288d7")
P10<-new_data2 %>% filter(Patient_info == "SMIC289d2")
P11<-new_data2 %>% filter(Patient_info == "SMIC289d7")
P12<-new_data2 %>% filter(Patient_info == "SMIC290d2")
P13<-new_data2 %>% filter(Patient_info == "SMIC291d2")
P14<-new_data2 %>% filter(Patient_info == "SMIC291d7")
P15<-new_data2 %>% filter(Patient_info == "SMIC294d2")
P16<-new_data2 %>% filter(Patient_info == "SMIC295d2")
P17<-new_data2 %>% filter(Patient_info == "SMIC295d7")
P18<-new_data2 %>% filter(Patient_info == "SMIC296d2")
P19<-new_data2 %>% filter(Patient_info == "SMIC305d2")
P20<-new_data2 %>% filter(Patient_info == "SMIC306d7")

P1<-P1[,2]
P2<-P2[,2]
P3<-P3[,2]
#P4<-P4[,2]
P5<-P5[,2]
P6<-P6[,2]
P7<-P7[,2]
P8<-P8[,2]
P9<-P9[,2]
P10<-P10[,2]
P11<-P11[,2]
P12<-P12[,2]
P13<-P13[,2]
P14<-P14[,2]
P15<-P15[,2]
P16<-P16[,2]
P17<-P17[,2]
P18<-P18[,2]
P19<-P19[,2]
P20<-P20[,2]

# Random sampling of the CDR3b sequences. Take 19 samples (19 is the lowest number of CDR3b sequence for the samples)
#and sample them randomly for 1000 times without replacement

P1R<-replicate(n=10000, expr={P1[sample(nrow(P1),19,replace=F),]},simplify=F)
P2R<-replicate(n=10000, expr={P2[sample(nrow(P2),19,replace=F),]},simplify=F)
P3R<-replicate(n=10000, expr={P3[sample(nrow(P3),19,replace=F),]},simplify=F)
#P4R<-replicate(n=10000, expr={P4[sample(nrow(P4),19,replace=F),]},simplify=F)
P5R<-replicate(n=10000, expr={P5[sample(nrow(P5),19,replace=F),]},simplify=F)
P6R<-replicate(n=10000, expr={P6[sample(nrow(P6),19,replace=F),]},simplify=F)
P7R<-replicate(n=10000, expr={P7[sample(nrow(P7),19,replace=F),]},simplify=F)
P8R<-replicate(n=10000, expr={P8[sample(nrow(P8),19,replace=F),]},simplify=F)
P9R<-replicate(n=10000, expr={P9[sample(nrow(P9),19,replace=F),]},simplify=F)
P10R<-replicate(n=10000, expr={P10[sample(nrow(P10),19,replace=F),]},simplify=F)
P11R<-replicate(n=10000, expr={P11[sample(nrow(P11),19,replace=F),]},simplify=F)
P12R<-replicate(n=10000, expr={P12[sample(nrow(P12),19,replace=F),]},simplify=F)
P13R<-replicate(n=10000, expr={P13[sample(nrow(P13),19,replace=F),]},simplify=F)
P14R<-replicate(n=10000, expr={P14[sample(nrow(P14),19,replace=F),]},simplify=F)
P15R<-replicate(n=10000, expr={P15[sample(nrow(P15),19,replace=F),]},simplify=F)
P16R<-replicate(n=10000, expr={P16[sample(nrow(P16),19,replace=F),]},simplify=F)
P17R<-replicate(n=10000, expr={P17[sample(nrow(P17),19,replace=F),]},simplify=F)
P18R<-replicate(n=10000, expr={P18[sample(nrow(P18),19,replace=F),]},simplify=F)
P19R<-replicate(n=10000, expr={P19[sample(nrow(P19),19,replace=F),]},simplify=F)
P20R<-replicate(n=10000, expr={P20[sample(nrow(P20),19,replace=F),]},simplify=F)

# Counting the number of times each sequence occurs in each random sample

f2<-function(x){table(unlist(x))}

P1R <-lapply(P1R, f2)
P2R<-lapply(P2R,f2)
P3R <-lapply(P3R, f2)
#P4R<-lapply(P4R,f2)
P5R <-lapply(P5R, f2)
P6R<-lapply(P6R,f2)
P7R <-lapply(P7R, f2)
P8R<-lapply(P8R,f2)
P9R <-lapply(P9R, f2)
P10R<-lapply(P10R,f2)
P11R <-lapply(P11R, f2)
P12R<-lapply(P12R,f2)
P13R <-lapply(P13R, f2)
P14R<-lapply(P14R,f2)
P15R <-lapply(P15R, f2)
P16R<-lapply(P16R,f2)
P17R <-lapply(P17R, f2)
P18R<-lapply(P18R,f2)
P19R <-lapply(P19R, f2)
P20R<-lapply(P20R,f2)

#Finding the diversity for each randomly selected groups of 19 sequences

library(vegan)
f3=function(x){diversity(x,"invsimpson")}
result1<-lapply(P1R, f3)
result2<-lapply(P2R, f3)
result3<-lapply(P3R, f3)
#result4<-lapply(P4R, f3)
result5<-lapply(P5R, f3)
result6<-lapply(P6R, f3)
result7<-lapply(P7R, f3)
result8<-lapply(P8R, f3)
result9<-lapply(P9R, f3)
result10<-lapply(P10R, f3)
result11<-lapply(P11R, f3)
result12<-lapply(P12R, f3)
result13<-lapply(P13R, f3)
result14<-lapply(P14R, f3)
result15<-lapply(P15R, f3)
result16<-lapply(P16R, f3)
result17<-lapply(P17R, f3)
result18<-lapply(P18R, f3)
result19<-lapply(P19R, f3)
result20<-lapply(P20R, f3)


P1M<-unlist(result1)
P2M<-unlist(result2)
P3M<-unlist(result3)
#P4M<-unlist(result4)
P5M<-unlist(result5)
P6M<-unlist(result6)
P7M<-unlist(result7)
P8M<-unlist(result8)
P9M<-unlist(result9)
P10M<-unlist(result10)
P11M<-unlist(result11)
P12M<-unlist(result12)
P13M<-unlist(result13)
P14M<-unlist(result14)
P15M<-unlist(result15)
P16M<-unlist(result16)
P17M<-unlist(result17)
P18M<-unlist(result18)
P19M<-unlist(result19)
P20M<-unlist(result20)

#FInding the median diversity value
Diversity1<-median(P1M)
Diversity2<-median(P2M)
Diversity3<-median(P3M)
#Diversity4<-median(P4M)
Diversity5<-median(P5M)
Diversity6<-median(P6M)
Diversity7<-median(P7M)
Diversity8<-median(P8M)
Diversity9<-median(P9M)
Diversity10<-median(P10M)
Diversity11<-median(P11M)
Diversity12<-median(P12M)
Diversity13<-median(P13M)
Diversity14<-median(P14M)
Diversity15<-median(P15M)
Diversity16<-median(P16M)
Diversity17<-median(P17M)
Diversity18<-median(P18M)
Diversity19<-median(P19M)
Diversity20<-median(P20M)

TotalDiversity<-rbind(Diversity1,Diversity2,Diversity3,Diversity5,Diversity6,Diversity7,
                      Diversity8,Diversity9,Diversity10,Diversity11,Diversity12,Diversity13,Diversity14,
                      Diversity15, Diversity16, Diversity17, Diversity18, Diversity19, Diversity20)



rownames(TotalDiversity)[1:19]<-c("SEGD  [LPL]46d2","SEGD  [PBMC]46d2","SEGD [LPL]45d180","SEGD46d7","SMIC262d90","SMIC271d90","SMIC288d2",
                                  "SMIC288d7","SMIC289d2","SMIC289d7","SMIC290d2","SMIC291d2","SMIC291d7","SMIC294d2","SMIC295d2","SMIC295d7",
                                  "SMIC296d2","SMIC305d2","SMIC306d7")

write.csv(TotalDiversity, "Inverse Simpson Diversity for all patients_Cholerae.csv")
write.csv(new_data2, "Patient info and CDR3.csv")
                                  


