#Previewing wineData
wineData = read.csv("wineData.csv", header = TRUE)
wineData
nrow(wineData)
ncol(wineData)
head(wineData)

min(wineData$Alcohol)
max(wineData$Alcohol)
mean(wineData$Alcohol)

min(wineData$Malic.acid)
max(wineData$Malic.acid)
mean(wineData$Malic.acid)


summary(wineData)
plot(wineData)


#Remove first column feature
wineData$Class<-NULL
wineData

#Normalize data
wineData.stand<- scale(wineData[-1])
View(wineData.stand)

#Clean & Prepare Dataset
summary(wineData)
plot(wineData)
which(is.na(wineData), arr.ind = TRUE)

max(wineData$Alcohol)
wineDataClean= wineData[wineData$Alcohol<140,]
nrow(wineDataClean)
nrow(wineData)

#Correcting Errors
#the dataset has no missing values as mentioned in website
wineData = read.csv("wineData - Original.csv", header = TRUE)
which(is.na(wineData), arr.ind=TRUE)

wineDataClean = wineData[rowSums(is.na(wineData)) == 0,]
nrow(wineData) #No of rows

#Sometimes, there may be errors in data. For example, if we suspect that the largest amount chemical in winedata is an error,
#we can eliminate that row using the following commands:
max(wineDataClean$Alcohol)
wineDataClean4 = wineDataClean[wineDataClean$Alcohol < 1079549,]
nrow(wineDataClean4)


#Feature Engineering Technique

#Measure of Location
mean(wineData$Alcohol)
median(wineData$Alcohol)
mode(wineData$Alcohol)
midrange = (max(wineData$Alcohol) - min(wineData$Alcohol))/2
midrange

#Measure of Dispersion
range = max(wineData$Alcohol) - min(wineData$Alcohol)
range
var(wineData$Alcohol)
sd(wineData$Alcohol)
IQR(wineData$Alcohol)

#Histogram of wineData$Alcohol
hist(wineData$Alcohol, cex.main = 0.75) #cex.main set font size of graph title
#We can control the number of bins used in the histogram
par(mfrow=c(1, 2))
hist(wineData$Alcohol, 20, cex.main = 0.75)
hist(wineData$Alcohol, 5, cex.main = 0.75)

#Another useful representation is the Commutative Distribution Function (CDF)
p = ecdf(wineData$Alcohol)
plot(p, cex.main = 0.75)

#compare the distribution of other attributes such as Alcohol and Malic.Acid
par(mfrow=c(1, 2))
plot(ecdf(wineData$Alcohol), cex.main = 0.75)
plot(ecdf(wineData$Malic.acid), cex.main = 0.75)

Alcoholu = mean(wineData$Alcohol)
Alcohol = sd(wineData$Alcohol)
Malic.acidu = mean(wineData$Malic.acid)
Malic.acid = sd(wineData$Malic.acid)
Alcoholu
Alcohol
Malic.acidu
Malic.acid

#Percentiles
#we can identify the minimum amount of alcohol and Malic Acid of 75% in the wine 
quantile(wineData$Alcohol, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(wineData$Malic.acid, probs = c(0, 0.25, 0.5, 0.75, 1))

#Coefficient of Variation (CoV)
Alcoholu/Alcohol
Malic.acidu/Malic.acid
#It can be seen standard deviation of alcohol is still relatively higher than standard deviation of Malic.acid,
#meaning alcohol in the wine tends to have a high deviation compared to malic.acid

d = density(wineData$Alcohol)
d

par(mfrow=c(1, 2))
plot(wineData$Alcohol, cex.main = 0.75)
plot(d, cex.main = 0.75)

#Boxplot
par(mfrow=c(1, 2))
boxplot(wineData$Alcohol)
boxplot(wineData$Malic.acid)

#We can use the following command to calculate group-wise statistics:
aggregate(Alcohol ~ Malic.acid, wineData, mean)
aggregate(Alcohol ~ Malic.acid, wineData, sd)

#Measure of Shape
install.packages("e1071", dep = TRUE)
library(e1071)
skewness(wineData$Alcohol)

kurtosis(wineData$Alcohol) 
#Based on the Coefficient of Skewness (CS) we estimate the symmetry of a distribution:

#Measure of Association
cov(wineData$Alcohol, wineData$Malic.acid)

par(mfrow=c(1, 2))
plot(wineData$Alcohol, wineData$Malic.acid, col="blue", cex.axis = 0.5)

#cex.axis set font size axis labels
cor(wineData$Alcohol, wineData$Malic.acid)

#We can use the following command to calculate all pairs of correlations:
cor(wineData[,1:4])

#Principal Componenent Analysis
wineDataPCA = prcomp(wineData[1:8], center = TRUE, scale = TRUE) #Scale & center data
summary(wineDataPCA)

screeplot(wineDataPCA, type="lines", cex.main = 0.75) #Draw a line plot
wineDataPCA
par(mfcol=c(1,2))
plot(wineDataPCA$x[,1:2])
plot(wineDataPCA$x[,2:3])

#Log Transformation
wineData.log = log(wineData[, 1:4]) #Log transform the dataset
wineData.log.pca = prcomp(wineData.log[1:4], center = TRUE, scale = TRUE)
summary(wineData.log.pca)

kc.wineData.log.pca = kmeans(wineData.log.pca$x[,1:2], 3)
par(mfcol=c(1,2))
plot(wineData.log.pca$x[,1:2])
plot(wineData.log.pca$x[,1:2], col=kc.wineData.log.pca$cluster)

wineData = read.csv("wineData.data")
head(wineData)  
plot(wineData[,1:13])

wineData.pca = prcomp(wineData[,1:13], scale =TRUE, center = TRUE)
kc.wineData.pca = kmeans(wineData.pca$x[,1:2], 3)
par(mfcol=c(1,2))
plot(wineData.pca$x[,1:2])
plot(wineData.pca$x[,1:2], col=kc.wineData.pca$cluster)

table(wineData$Alcohol, kc.wineData.pca$cluster)
kc.wineData = kmeans(wineData[,1:13], 3)
table(wineData$Alcohol, kc.wineData$cluster)


#Cluster Analysis
plot(wineData)
plot(wineData[,1:2])
plot(wineData[,1:4])

#k-Means Clustering
#Let us cluster the IQ values using k-Means, where k = 2 as we see 2 clusters.
kc = kmeans(wineData[,1:2], 2)
kc
results<-kmeans(wineData.stand,3)
attributes(results)
#gives centers of each attribute
results$centers

par(mfrow=c(1, 2))
plot(wineData[,1:2], col=kc$cluster)
points(kc$centers[,1:2], col=1:2, pch=8, cex=2)
plot(wineData[,2:3], col=kc$cluster)
points(kc$centers[,1:2], col=1:2, pch=8, cex=2)

table(wineData$Alcohol, kc$cluster)

plot(wineData[,1:3], col=kc$cluster, cex.main = 0.75)
points(kc$centers[, 1:2], col=1:3, pch=8, cex=2)

table(wineData$Malic.acid, kc$cluster) 
#Compare k-means and data

#Hierarchical Clustering
dis = dist(wineData[1:2], method="euclidean") #Euclidean distance between points
hcwineAve = hclust(dis, method="ave") #Group average as similarity measure
hcwineWard = hclust(dis, method="ward.D") #Ward's method as similarity measure
par(mfcol=c(1,2))
plot(hcwineAve, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcwineAve, k=2, border="green")
plot(hcwineWard, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcwineWard, k=2, border="green")

hcwineAveCut = cutree(hcwineAve, 2)
hcwineAveCut
#List cluster membership

hcwineWardCut = cutree(hcwineWard, 2)
par(mfcol=c(1,2))
plot(wineData[,1:2], col=hcwineAveCut, cex.main = 0.75)
plot(wineData[,1:2], col=hcwineWardCut, cex.main = 0.75)


#evaluate with original
table(wineData$Class, results$cluster)
#Evaluate Clustering
newPoint1 = c(5.2, 3.4, 1.3, 0.3)
newPoint2 = c(6.2, 3.0, 5.3, 1.6)
newPoints = rbind(newPoint1, newPoint2)
newPoints

library(class) #Load library
wineData.pred3 = knn(train=wineData[,1:4], test=newPoints, cl=wineData$Alcohol, k=3)
wineData.pred3

set.seed(1234)
indx = sample(2, nrow(wineData), replace=TRUE, prob=c(0.67, 0.33)) #Split dataset 2/3 & 1/3
wineData.train = wineData[indx==1, 1:4] #Where sample index is 1
wineData.test = wineData[indx==2, 1:4] #Where sample index is 2
summary(wineData.train)













