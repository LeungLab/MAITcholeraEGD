#Load the libraries
library(readxl)
library(dplyr)
library(reshape2)
library(pheatmap)

#Clean up the data to get only those data which has TRBV sequence

my_data <- read_xlsx("Cholerae raw data_Ioana.xlsx", col_types = "text", col_names = TRUE)
new_data<-my_data[,-c(1,2,6,7,8,9,11,14,15,20,21,22,23,24,25,26,29,35,36,37,38,39,40,41,42,43,44,45)]

new_data$Patient_info<-do.call(paste0, new_data[c(1,2,3)])
new_data1<-new_data[,-c(1,2,3)]

new_data2<-new_data1[,c(26,1)]
new_data2<-new_data2 %>% filter(Patient_info != "NANANA", TRBV != "NA")

#Count and find the proportion of each TRBV sequence for each patient

test<- count(new_data2,Patient_info,TRBV)

final500<-acast(test, TRBV~Patient_info, value.var = "n", fun.aggregate = sum)
final600<- apply(final500,2,function(x){x/sum(x)})

final700<-final600[,c(1,3,4,7,8,9,10,12,13,15,16)]

colnames(final700)<-c("SEGD[LPL]46d2","SEGD[PBMC]46d2","SEGD46d7","SMIC288d2"," SMIC288d7","SMIC289d2","SMIC289d7","SMIC291d2","SMIC291d7","SMIC295d2","SMIC295d7")

#Make the heatmap
library(pheatmap)
result <-pheatmap(final700, color = colorRampPalette (c("white","navy","firebrick3"))(100), cluster_rows = TRUE, cluster_cols = FALSE,clustering_distance_rows = "euclidean", clustering_method = "complete",
                  gaps_col =c(3,5,7,9))

#write.csv(final500, "TRBV for Cholerae patients.csv")
