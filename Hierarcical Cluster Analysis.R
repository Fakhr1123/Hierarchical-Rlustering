install.packages("psych")
install.packages("NbClust")
install.packages("cluster")
install.packages("clustertend")
installed.packages("factoextra")
install.packages("tidyverse")
install.packages("mice")
install.packages("writexl")
install.packages("clValid")
library (NbClust)
library (cluster)
library (clustertend)
library (factoextra)
library(tidyverse)
library(readxl)
library(mice)
library(writexl)

#import data
data_1<-read_excel("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas_Panen.xlsx")
#view(data_1)
data_2<- read_excel("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas tanam.xlsx")
#view(data_2)
data_3<- read_excel("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/jumlah produksi.xlsx")
#view(data_3)
#ringkasan data dan cek data hilang
summary(data_1)
summary(data_2)
summary(data_3)
str(data_1)
str(data_2)
str(data_3)
is.na(data_1)
sum(is.na(data_1))
is.na(data_2)
sum(is.na(data_2))
is.na(data_3)
sum(is.na(data_3))
#mengisi data kosong dengan mean/median/modus
#1 luas panen
data_1$`Ubi kayu`
#view(data_1[,2])
Ubi1<-mean(data_1$`Ubi kayu`,na.rm = TRUE)
data_1$`Ubi kayu` = ifelse((is.na(data_1$`Ubi kayu`)), Ubi1,data_1$`Ubi kayu`)
#view(data_1[,2])

data_1$`Kacang tanah`
#view(data_1[,4])
Kacang1<-mean(data_1$`Kacang tanah`, na.rm = TRUE)
RataKacang1<-round(Kacang1, digits = 2)
data_1$`Kacang tanah` = ifelse((is.na(data_1$`Kacang tanah`)), RataKacang1,data_1$`Kacang tanah`)
#view(data_1[,4])


data_1$Terung
#view(data_1[,6])
data_1$Terung = ifelse((is.na(data_1$Terung)), mean(data_1$Terung, na.rm = TRUE),data_1$Terung)
#view(data_1[,6])
#view(data_1)

write_xlsx(data_1,"E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas_PanenCLEAN.xlsx")
data_1<-read_excel("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas_PanenCLEAN.xlsx")
write.csv(data_1,"E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas_PanenCLEAN.xlsx")
data_1<-read.csv("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas_PanenCLEAN.xlsx")
data1<-data_1[,-1]

#2 Luas Tanam
data_2$`Ubi kayu`
#view(data_2[,1])
Ubi2<-mean(data_2$`Ubi kayu`, na.rm = TRUE)
Rataubi2<-round(Ubi2, digits = 2)
data_2$`Ubi kayu` = ifelse((is.na(data_2$`Ubi kayu`)), Rataubi2, data_2$`Ubi kayu`)
#view(data_2[,2])


data_2$`Kacang tanah`
#view(data_2[,4])
Kacang2<-mean(data_2$`Kacang tanah`, na.rm = TRUE)
Ratakacang2<-round(Kacang2, digits = 2)
data_2$`Kacang tanah` = ifelse((is.na(data_2$`Kacang tanah`)), Ratakacang2,data_2$`Kacang tanah`)
#view(data_2[,4])

data_2$Terung
#view(data_2[,6])
Terung2<-mean(data_2$Terung, na.rm = TRUE)
Rataterung2<-round(Terung2, digits = 2)
data_2$Terung = ifelse((is.na(data_2$Terung)), Rataterung2, data_2$Terung)
#view(data_2[,6])

#view(data_2)
write_xlsx(data_2,"E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas tanamCLEAN.xlsx")
data_2<-read_excel("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas tanamCLEAN.xlsx")
write.csv(data_2,"E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas tanamCLEAN.xlsx")
data_2<-read.csv("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/luas tanamCLEAN.xlsx")
data2<-data_2[,-1]

#3 Jumlah Produksi
data_3$`Ubi kayu`
#view(data_3[,2])
Ubi3<-mean(data_3$`Ubi kayu`, na.rm = TRUE)
Rataubi3<-round(Ubi3, digits = 1)
data_3$`Ubi kayu` = ifelse((is.na(data_3$`Ubi kayu`)), Rataubi3 ,data_3$`Ubi kayu`)
#view(data_3[,2])

data_3$`Kacang tanah`
#view(data_3[,4])
Kacang3<-mean(data_3$`Kacang tanah`, na.rm = TRUE)
Ratakacang3<-round(Kacang3, digits = 1)
data_3$`Kacang tanah` = ifelse((is.na(data_3$`Kacang tanah`)), Ratakacang3,data_3$`Kacang tanah`)
#view(data_3[,4])

data_3$Terung
#view(data_3[,6])
Terung3<-mean(data_3$Terung, na.rm = TRUE)
Rataterung3<-round(Terung3, digits = 1)
data_3$Terung = ifelse((is.na(data_3$Terung)), Rataterung3,data_3$Terung)
#view(data_3[,6])

#view(data_3)
write_xlsx(data_3,"E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/jumlah produksiCLEAN.xlsx")
data_3<-read_excel("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/jumlah produksiCLEAN.xlsx")
write.csv(data_3,"E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/jumlah produksiCLEAN.xlsx")
data_3<-read.csv("E:/@Kuliah/magang kominfo/Laporan Magang 2023/data bersih/mantahan/jumlah produksiCLEAN.xlsx")
data3<-data_3[,-1]

sum(is.na(data_1))
sum(is.na(data_2))
sum(is.na(data_3))


#analisis deskriptif
summary(data1)
summary(data2)
summary(data3)
str(data1)
str(data2)
str(data3)

###uji multikolinearitas###
install.packages("car")
library(car)
data_1
# UJI ASUMSI MULTIKOLIEARITAS
lm_model1<-lm(Ubi.kayu ~ Jagung + Cabai.keriting + Kacang.tanah + Terung, data = data1)
lm_model2<-lm(Ubi.kayu ~ Jagung + Cabai.keriting + Kacang.tanah + Terung, data = data2)
lm_model3<-lm(Ubi.kayu ~ Jagung + Cabai.keriting + Kacang.tanah + Terung, data = data3)


# MENCARI NILAI VIF
vif_result1 <- vif(lm_model1)
vif_result2 <- vif(lm_model2)
vif_result3 <- vif(lm_model3)
# HASIL VIF
print(vif_result1)
print(vif_result2)
print(vif_result3)

###penaganan multikolinearitas
model2 <- lm(Ubi.kayu ~ Jagung + Cabai.keriting + Kacang.tanah + Terung, data = data2)

vif_result2 <- car::vif(model2)
print(vif_result2)
if (vif_result2["Cabai.keriting"] >= 5 || vif_result2["Kacang.tanah"] >= 5) {
  # Apply log transformation to the problematic variables
  data2$Cabai.keriting <- log(data2$Cabai.keriting)
  data2$Kacang.tanah <- log(data2$Kacang.tanah)
  
  # Refit the model with the transformed variables
  model2_transformed <- lm(Ubi.kayu ~ Jagung + Cabai.keriting + Kacang.tanah + Terung, data = data2)
  
  # Calculate VIF for the transformed model
  vif_result2_transformed <- car::vif(model2_transformed)
  
  # Check VIF values for the transformed model
  print(vif_result2_transformed) }

data2$Kacang.tanah[5]<-0.000001

###Uji KMO dan MSA###
library(psych)
data1
data2
data3
# Select variables for analysis (you can modify this)
selected_variables1 <- c("Ubi.kayu", "Kacang.tanah", "Jagung", "Cabai.keriting", "Terung")
# Subset the dataset with selected variables
subset_data1 <- data1[, selected_variables1]
subset_data2 <- data2[, selected_variables1]
subset_data3 <- data3[, selected_variables1]

# Compute KMO and MSA
kmo_result <- KMO(subset_data1)
kmo_result <- KMO(subset_data2)
kmo_result <- KMO(subset_data3)

# Menentukan metode terbaik analisis klaster hirarki
data.1<-data1[,-1]
data.2<-data2[,-1]
data.3<-data3[,-1]

# menampilkan ukuran jarak distribusi setiap data
distace_matrix.1<-dist(subset_data1, method = 'euclidian')
distace_matrix.2<-dist(subset_data2, method = 'euclidian')
distace_matrix.3<-dist(subset_data3,method = 'euclidian')



#menentukan metode klaster terbaik
#1
data_sigl1<-hclust(d=distace_matrix.1 , method = "single")
sigl_coph1<-cophenetic(data_sigl1)
cor(sigl_coph1, distace_matrix.1)

data_comp1<-hclust(d=distace_matrix.1 , method = "complete")
comp_coph1<-cophenetic(data_comp1)
cor(comp_coph1, distace_matrix.1)

data_avg1<-hclust(d=distace_matrix.1 , method = "average")
avg_coph1<-cophenetic(data_avg1)
cor(avg_coph1, distace_matrix.1)

data_wrd1<-hclust(d=distace_matrix.1 , method = "ward.D")
wrd_coph1<-cophenetic(data_wrd1)
cor(wrd_coph1, distace_matrix.1)

#2
data_sigl2<-hclust(d=distace_matrix.2 , method = "single")
sigl_coph2<-cophenetic(data_sigl2)
cor(sigl_coph2, distace_matrix.2)

data_comp2<-hclust(d=distace_matrix.2 , method = "complete")
comp_coph2<-cophenetic(data_comp2)
cor(comp_coph2, distace_matrix.2)

data_avg2<-hclust(d=distace_matrix.2 , method = "average")
avg_coph2<-cophenetic(data_avg2)
cor(avg_coph2, distace_matrix.2)

data_wrd2<-hclust(d=distace_matrix.2 , method = "ward.D")
wrd_coph2<-cophenetic(data_wrd2)
cor(wrd_coph2, distace_matrix.2)

#3
data_sigl3<-hclust(d=distace_matrix.3 , method = "single")
sigl_coph3<-cophenetic(data_sigl3)
cor(sigl_coph3, distace_matrix.3)

data_comp3<-hclust(d=distace_matrix.3 , method = "complete")
comp_coph3<-cophenetic(data_comp3)
cor(comp_coph3, distace_matrix.3)

data_avg3<-hclust(d=distace_matrix.3 , method = "average")
avg_coph3<-cophenetic(data_avg3)
cor(avg_coph3, distace_matrix.3)

data_wrd3<-hclust(d=distace_matrix.3 , method = "ward.D")
wrd_coph3<-cophenetic(data_wrd3)
cor(wrd_coph3, distace_matrix.3)



#menghitung nilai klaster berdasarkan metode single linkage dan complate linkage
hc1 <- hclust(distace_matrix.1, method = 'average')
hc2 <- hclust(distace_matrix.2, method = 'single')
hc3 <- hclust(distace_matrix.3, method = 'average')

###menentukan jumlah klaster terbaik
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
fviz_nbclust(data1, FUN = hcut, method = "wss")
fviz_nbclust(data2, FUN = hcut, method = "wss")
fviz_nbclust(data3, FUN = hcut, method = "wss")
#nilai klaster optimal adalah 4

##membentuk klaster
#1
fviz_dend(hc1, cex= 1, 
          main= "luas panen")
fviz_dend(hc1, k=3, k_colors = "jco", rect=T,  
          main= "Luas Panen(average linkage)")
cluster1<-cutree(hc1, k=3)

#2
fviz_dend(hc2, cex= 0.6, 
          main= "luas tanam")
fviz_dend(hc2, k=3, k_colors = "jco", rect=T,  
          main= "Luas Tanam(single linkage)")
cluster2<-cutree(hc2, k=3)

#3
fviz_dend(hc3, cex= 0.6, 
          main= "jumlah produksi")
fviz_dend(hc3, k=3, k_colors = "jco", rect=T,  
          main= "Jumlah Produksi(average linkage)")
cluster3<-cutree(hc3, k=3)



# Add cluster information to the original dataset
data1<-data.frame(data1, cluster1)
data2<-data.frame(data2, cluster2)
data3<-data.frame(data3, cluster3)

# Print the number of states in each cluster
table(data1$cluster1)
table(data2$Cluster2)
table(data3$cluster3)

#Validasi dan profilling
library(clValid)
#1
rownames(data.1) <- 1:nrow(data.1)
internal1<-clValid(data.1, nClust = 2:4,
                  clMethods = "hierarchical",
                  validation = "internal",
                  metric = "euclidean",
                  method = "average")
summary(internal1)

#2
rownames(data.2) <- 1:nrow(data.2)
internal2<-clValid(data.2, nClust = 2:4,
                  clMethods = "hierarchical",
                  validation = "internal",
                  metric = "euclidean",
                  method = "average")
summary(internal2)

#3
rownames(data.3) <- 1:nrow(data.3)
internal3<-clValid(data.3, nClust = 2:4,
                  clMethods = "hierarchical",
                  validation = "internal",
                  metric = "euclidean",
                  method = "average")
summary(internal3)

###
#1
klaster1.1<-subset(data1, cluster1==1)
klaster2.1<-subset(data1, cluster1==2)
klaster3.1<-subset(data1, cluster1==3)


Kluster1.1<-sapply(klaster1.1[2:6], mean)
Kluster2.1<-sapply(klaster2.1[2:6], mean)
Kluster3.1<-sapply(klaster3.1[2:6], mean)


mean_total1<-rbind(Kluster1.1, Kluster2.1, Kluster3.1)
mean_total1

#2
klaster1.2<-subset(data2, cluster1==1)
klaster2.2<-subset(data2, cluster1==2)
klaster3.2<-subset(data2, cluster1==3)


Kluster1.2<-sapply(klaster1.2[2:6], mean)
Kluster2.2<-sapply(klaster2.2[2:6], mean)
Kluster3.2<-sapply(klaster3.2[2:6], mean)


mean_total2<-rbind(Kluster1.2, Kluster2.2, Kluster3.2)
mean_total2

#3
klaster1.3<-subset(data3, cluster1==1)
klaster2.3<-subset(data3, cluster1==2)
klaster3.3<-subset(data3, cluster1==3)


Kluster1.3<-sapply(klaster1.3[2:6], mean)
Kluster2.3<-sapply(klaster2.3[2:6], mean)
Kluster3.3<-sapply(klaster3.3[2:6], mean)


mean_total3<-rbind(Kluster1.3, Kluster2.3, Kluster3.3)
mean_total3

sort(mean_total1, decreasing = T)







