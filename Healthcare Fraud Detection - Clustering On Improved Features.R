install.packages('corrplot')
install.packages("FactoMineR")
install.packages("factoextra")
install.packages('cluster')
install.packages("psych")
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(tidyr)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(cluster)
library(psych)

#Loading feature-engineered data set from the initial assignment on Healthcare data abuse detection
setwd('C:/myfiles/Anomaly/assignment5')
payment_V2=read.csv('payment_V2.csv')
str(payment_V2)

head(payment_V2)


DRG_State_avg <- payment_V2 %>% group_by(DRG.Definition, Provider.State) %>%
  summarise( mean_average_covered_charges   = mean(Average.Covered.Charges),
             mean_average_total_payments    = mean(Average.Total.Payments),
             mean_average_medicare_payments = mean(Average.Medicare.Payments),                 
             mean_count_trans =n()
  )

head(DRG_State_avg,5)

# Append the average statistics back to the data to derive the features on ratios. Based on the feedback from the last assignment,
#the following features are created to perform peer comparisons on payments and charges among providers of the same state in addition to
# to comparing individual provider's covered charge to total charge through existing features
payment_V3 <- payment_V2 %>% 
  left_join(DRG_State_avg, by=c("DRG.Definition","Provider.State")) %>%
  mutate( 
    ratio_average_covered_charges   = Average.Covered.Charges/mean_average_covered_charges,
    ratio_average_total_payments    = Average.Total.Payments/mean_average_total_payments,
    ratio_average_medicare_payments = Average.Medicare.Payments/mean_average_medicare_payments) %>%
  select(-mean_average_covered_charges,
         -mean_average_total_payments,
         -mean_average_medicare_payments,
         -mean_count_trans
  ) %>% arrange(Provider.Id, DRG.Definition) 

str(payment_V3)

#Plotting ratio_average_covered_charges vs DRG by State
payment_V3 <- as.data.table(payment_V3)
df_outlier <- payment_V3[payment_V3$ratio_average_covered_charges > 0.70*(max(payment_V3$ratio_average_covered_charges) - min(payment_V3$ratio_average_covered_charges[payment_V3$ratio_average_covered_charges> 0])),]
av1 <- payment_V3[,ratio_average_covered_charges,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = ratio_average_covered_charges,colour=ratio_average_covered_charges))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=ratio_average_covered_charges,y=DRGbyState), color='red',size=1.5)  

#Plotting ratio_average_total_payments vs DRG by State
payment_V3 <- as.data.table(payment_V3)
df_outlier <- payment_V3[payment_V3$ratio_average_total_payments > 0.70*(max(payment_V3$ratio_average_total_payments) - min(payment_V3$ratio_average_total_payments[payment_V3$ratio_average_total_payments> 0])),]
av1 <- payment_V3[,ratio_average_total_payments,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = ratio_average_total_payments,colour=ratio_average_total_payments))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=ratio_average_total_payments,y=DRGbyState), color='red',size=1.5) 

#Plotting ratio_average_medicare_payments vs DRG by State
payment_V3 <- as.data.table(payment_V3)
df_outlier <- payment_V3[payment_V3$ratio_average_medicare_payments > 0.70*(max(payment_V3$ratio_average_medicare_payments) - min(payment_V3$ratio_average_medicare_payments[payment_V3$ratio_average_medicare_payments> 0])),]
av1 <- payment_V3[,ratio_average_medicare_payments,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = ratio_average_medicare_payments,colour=ratio_average_medicare_payments))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=ratio_average_medicare_payments,y=DRGbyState), color='red',size=1.5) 


#Replacing all -ve values in features created for mean differences with 0 as we are interested in studying the data 
#that are above the mean representing high charges

payment_corrected <- as.data.frame(payment_V3)
payment_corrected[ , c("ChargePayDiff_State", "ChargePayDiff_Zip", "ChargePayDiff_HRR","TotalPmtDiff_State","TotalPmtDiff_Zip","TotalPmtDiff_HRR","TotalChrgDiff_Zip")][ payment_corrected[ ,  c("ChargePayDiff_State", "ChargePayDiff_Zip", "ChargePayDiff_HRR","TotalPmtDiff_State","TotalPmtDiff_Zip","TotalPmtDiff_HRR","TotalChrgDiff_Zip")] < 0 ] <- 0
tail(payment_corrected)

#Correlation between selected features
#round(cor(payment_corrected[c("ChargePayDiff_State", "ChargePayDiff_Zip", "ChargePayDiff_HRR","TotalPmtDiff_State","TotalPmtDiff_Zip","TotalPmtDiff_HRR","DschrgRatio_State","SDTotalPmtDiff_State","SDTotalPmtDiff_Zip","SDTotalPmtDiff_HRR","SDTotalChrgDiff_State","TotalChrgDiff_Zip","ratio_average_covered_charges","ratio_average_total_payments","ratio_average_medicare_payments")]),3)

#Storing selected features for studying correlation in new data set
payment_cor <- select(payment_corrected,c("ChargePayDiff_State", "ChargePayDiff_Zip", "ChargePayDiff_HRR","TotalPmtDiff_State","TotalPmtDiff_Zip","TotalPmtDiff_HRR","DschrgRatio_State","SDTotalPmtDiff_State","SDTotalPmtDiff_Zip","SDTotalPmtDiff_HRR","SDTotalChrgDiff_State","TotalChrgDiff_Zip","ratio_average_covered_charges","ratio_average_total_payments","ratio_average_medicare_payments"))
str(payment_cor)

#Visualizing the matrix to spot high correlations that indicate pairwise similarity
corMatrix = as.data.frame(cor(payment_cor))
corMatrix$var1 = rownames(corMatrix)
corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'green',high='red',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))

#********************PRINCIPAL COMPONENT ANALYSIS***************************
#Multiple variables measuring the same dimension should be replaced by the underlying factor or a representative variable before
#using Cluster analysis. We could combine correlated variables into a single component based on the results of a principal components analysis.

# We standardize the variables so all of them are assigned equal weightage. Here, we are using the scale() which will by default
#subtract mean and divide by standard deviation. 

#Standardization
df.scaled <- scale(payment_cor, center = TRUE, scale = TRUE)
dim(df.scaled)
#Compute the correlation matrix
res.cor <- cor(df.scaled)

#Calculate the eigenvectors/eigenvalues of the correlation matrix
res.eig <- eigen(res.cor)
res.eig

# Transpose eigeinvectors
eigenvectors.t <- t(res.eig$vectors)

# Transpose the adjusted data
df.scaled.t <- t(df.scaled)

# Compute the new dataset (matrix format)
df.new <- eigenvectors.t %*% df.scaled.t

# Transpose new data and rename columns
df.new <- t(df.new)
str(df.new)
colnames(df.new) <- c("PC1", "PC2", "PC3", "PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15")

head(df.new)

#**************Tracing PCA back to its original variables**************
#Converting scaled original dataset to dataframe
df_orig <- as.data.frame(df.scaled)
str(df_orig)
##Converting PCA dataset to dataframe
df_pca <- as.data.frame(df.new)
str(df_pca)
#Binding PCA dataset with scaled original dataset
df_pca_cor <- cbind(df_orig,df_pca)
str(df_pca_cor)
#Studying correlation between PCA dataset and scaled original dataset to trace PCAs to original variables
corMatrix_pca = as.data.frame(cor(df_pca_cor))
corMatrix_pca$var1 = rownames(corMatrix_pca)
corMatrix_pca %>%
  gather(key=var2,value=r,1:15)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'green',high='red',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))

# PC1	ratio_average_total_payments
# PC2	ChargePayDiff_State
# PC3	TotalChrgDiff_Zip
# PC4	DschrgRatio_State
# PC5	SDTotalPmtDiff_Zip
# PC6	TotalPmtDiff_HRR
# PC7	SDTotalPmtDiff_HRR
# PC8	SDTotalPmtDiff_HRR


#**********************************************************************

#Variances explained by each of the principal components
res.pca <- prcomp(df.new)
summary(res.pca)$importance

#We want to keep only the significant features and drop the insignificant ones so we keep the top 8 Principal components 
#that capture up to 95% of the information

df.new <- as.data.frame(df.new)
df.new_principal <- select(df.new,c(1:8))


#binding only the principal components to the orginal data set
payment_revised <- select(payment_corrected,c(1:18))
payment_revised <- cbind(payment_revised,df.new_principal)
str(payment_revised)

#*************************************K-MEANS CLUSTERING - SELECTING NUMBER OF CLUSTERS********************************

#Subsetting Principal Components into a data set for Clustering
data_cluster <- payment_revised[,c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')]
str(data_cluster)

#We begin with an arbitrary assignment of observations to cluster centroids (here, 5-10) using K-means. And then update Cluster centers 
#to minimize sum of squares from observations to cluster centers. 

set.seed(1000) #Setting Seed for reproducibility
k5 <-kmeans(data_cluster, centers=5,iter.max=1000, nstart = 25,algorithm="Lloyd") 
set.seed(1000) 
k6 <-kmeans(data_cluster, centers=6,iter.max=1000, nstart = 25,algorithm="Lloyd") 
set.seed(1000) 
k7 <-kmeans(data_cluster, centers=7,iter.max=1000, nstart = 25,algorithm="Lloyd") 
set.seed(1000) 
k8 <-kmeans(data_cluster, centers=8,iter.max=1000, nstart = 25,algorithm="Lloyd") 
set.seed(1000) 
k9 <-kmeans(data_cluster, centers=9,iter.max=1000, nstart = 25,algorithm="Lloyd") 
set.seed(1000) 
k10<-kmeans(data_cluster, centers=10,iter.max=1000, nstart = 25,algorithm="Lloyd") 
#Here are the number of observations in the resulting clusters
k5[7]
k6[7]
k7[7]
k8[7]
k9[7]
k10[7]

#We will use the following two data-driven methods that rely on measures of distance of points from the clusters they are assigned to, 
#to determine cluster solutions: Total within sum of squares plot and  Ratio plot

#****Total within sum of squares plot
#Computing total within sum of squares for a number of values of k and plotting a line graph against it to infer number of 
#clusters from the elbow point

within_ss = sapply(4:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25,algorithm="Lloyd")$tot.withinss)
ggplot(data=data.frame(cluster = 4:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1)) 

#Ideal number of clusters is inferred from a sudden change in the line graph what is commonly known as the elbow point. 
#We see the elbow point at cluster = 6 in the above plot.

#****Ratio plot
#Computing ratio of between cluster sum of squares and total sum of squares for a number of values of k and plotting a line graph against it to infer number of 
#clusters from the elbow point

ratio_ss = sapply(4:10,FUN = function(x) {km = kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25,algorithm="Lloyd")
km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 4:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#We see the elbow point at cluster = 6 in the above plot

#The total within sum of square and ratio plots support a six-cluster solution

set.seed(1000)
km = kmeans(x = data_cluster,centers = 6,iter.max=1000,nstart=25,algorithm="Lloyd")

#Size of the clusters
k_segments = km$cluster
table(k_segments)

#*****************Visualizing the Clusters*****************

#To express the clusters on a scatterplot, we flatten the data from 12 dimensions onto 2 by conducting a factor analysis 
#with varimax rotation


temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

c=km[1] #Storing cluster mapping in a variable

#Centroids of clusters
km[2]
#The above table shows that the centroids of clusters 4 and 5 are generally higher compared to those of other clusters.
#Cluster 1 also seem to have centroids higher thatn others next only to clusters 4 and 5.
#From this we can form an opinion that providers falling in clusters 4 and 5 deserve a careful and thorough inspection and the cluster 
# 1 providers need a preliminary level inspection for healthcare abuse.

#Cluster size
km[7]

#The cluster sizes indicate that clusters 4,5 and 1 correspond to 9001 cases totally which is about 6% of the total observations that need inspection

#**************CONCLUSION************

#1. Based on the feedback from the last assignment, new features were added to perform peer comparisons on payments and charges among 
#providers of the same state in addition to comparing individual provider's covered charge to total charge through existing features 
#2. There is a need to perform Principal Component Analysis for creating new features by combining existing features to avoid 
#higher weightage to a specific dimension measured by multiple variables
#3. We arrive at a six-cluster solution using data driven methods like Within sum of squares plot and Ratio plot that rely on measures of 
#distance of points from the clusters they are assigned to
#4. Using the average statistics of the variables of that cluster solution, we can form an opinion that providers falling in 
#clusters 4 and 5 (1.5% of total cases) deserve a careful and thorough inspection. Cluster 1 (4.5% of total cases) need a preliminary 
#level inspection, the findings from which can be further used a closer inspection for healthcare abuse.
#5. The cluster sizes of the 6-cluster solution indicate that clusters 4,5 and 1 correspond to 9001 cases totally which is about 6% of
#the total observations that need inspection for healthcare waste and abuse


