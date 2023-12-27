ANN
// Membaca dataset
ProjDAP <- read_excel("C:/Kuliah/CCIT/Semester 3/DAP/ProjDAP.xlsx",
col_types = c("numeric", "numeric", "numeric",
"numeric", "numeric", "numeric","numeric"))

data2 <-ProjDAP

// Membagi data menjadi data train dan test
n<-round(nrow(data2)*0.75);n
set.seed(12);samp=sample(1:nrow(data2),n)
training_data <- data2[samp, ]
testing_data <- data2[-samp, ]

// Inisiasi formula menggunakan nama kolom
feats <- names(data2[,1:7])
f<-paste(feats,collapse='+');f
f1<-paste('Season ~',f);f1
f2<-as.formula(f1);f2

// ann analysis using neuralnet library
library(neuralnet)
nn<-neuralnet(f2,training_data,hidden=3)

// visualisasi
plot(nn)

KMEANS

// Preparation 
cluster <- 3
centroid <- data2[1:cluster,]

// kmeans analysis in r 
k_means_result <- kmeans(data2, centroid, iter.max = 10, nstart = 1,
algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

print(k_means_result)

// visualisasi
attributes <- data2[ ,c("Color","Season")]
num_cluster <- 3


plot(attributes, col = k_means_result$cluster, pch = 20, main = "Trends with kmeans")
points(k_means_result$centers, col = 1:num_cluster, pch = 3, cex = 2)
legend("topright", legend = 1:num_cluster, col = 1:num_cluster, pch = 3)
