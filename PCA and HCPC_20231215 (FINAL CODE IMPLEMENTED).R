#Starting analysis over with sample of n=281 as of 02/27/2023

#load the packages
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(foreign)
library(missMDA)
library(dplyr)          # for data manipulation
library(tidyr)          # for data shaping
library(gridExtra)


#store as a data frame
LETBI_20230227nocat_DF <- as.data.frame(LETBI_20220415_nocatvars)

############DATA PRE-PROCESSING STEP#############

#to handle missing data perform PCA imputation using 4 components (uses miss MDA package); best practice to include a random seed so results are replicable; 
?imputePCA
Imp_LETBI_202302277_DF<-imputePCA(LETBI_20230227nocat_DF, ncp=4, method = c("EM"), seed=555)
Imp_LETBI_202302277_DF$completeObs

############PCA#############

#FactoMineR: Run INITIAL PCA with 15 components w eigenvalue > 1 (10 have eigevalue); eigenvalue over 1 rule is overly liberal
?FAMD
?PCA
res.PCA_20230227_INIT<-PCA(Imp_LETBI_202302277_DF$completeObs, ncp = 15, scale.unit=TRUE)
res.PCA_20230227_INIT
get_eigenvalue(res.PCA_20230227_INIT)


#FactoMineR:decided based on eigenvalue data and scree plots to go with 3 PCs (accounts for ~45.2% of var)
?PCA
#FactoMineR:
res.PCA_20230227_FIN<-PCA(Imp_LETBI_202302277_DF$completeObs, ncp = 3, scale.unit=TRUE)
res.PCA_20230227_FIN$var$coord
get_eigenvalue(res.PCA_20230227_FIN)

#plotting the quantitative variables 
?fviz_pca_var


##add this code to make the PCA plot a little easier to read with better labels

Imp_LETBI_202302277_DF<-as.data.frame(Imp_LETBI_202302277_DF$completeObs)
write.csv(Imp_LETBI_202302277_DF, "Imp_LETBI_202302277_DF.csv")
res.PCA_20230227_PCAplot<-PCA(Imp_LETBI_202302277_DF.rk, ncp = 3, scale.unit=TRUE)

fviz_pca_var(res.PCA_20230227_PCAplot, repel=TRUE, geom = c("point", "text"), col.var="black", axes = c(1, 2))

fviz_pca_var(res.PCA_20230227_PCAplot, repel=TRUE, geom = c("point", "text"), col.var="black", axes = c(1, 3))

summary(res.PCA_20230227_FIN)





res.PCA_20230227_FIN$var$contrib
res.PCA_20230227_FIN$var$cos2
res.PCA_20230227_FIN$var$coord

#ind contribs
res.PCA_20230227_FIN$ind

#make a data frame of the three PC coordinates, which will be used to cluster data
res.PCA_20230227_FIN
res.PCA_20230227_FINDF<-as.data.frame(res.PCA_20230227_FIN$ind$coord)

############hierachical clustering on principal components#############

#compute the INITIAL hierachical clustering on principal components; #used k-means consolidation algorithm to improve the within group inertia
#Ward=Ward's minimum variance method: It minimizes the total within-cluster variance. 
#Ward cont. At each step the pair of clusters with minimum between-cluster distance are merged.
#euclidean= Euclidean distances are root sum-of-squares of differences

#use 70% of dataset as training set and 30% as test set
set.seed(101)
sample <- sample(c(TRUE, FALSE), nrow(res.PCA_20230227_FINDF), replace=TRUE, prob=c(0.7,0.3))
train  <- res.PCA_20230227_FINDF[sample, ]
test   <- res.PCA_20230227_FINDF[!sample, ]

?HCPC
#run the hierachical cluster analysis in the training set
res.hcpc_20230227_TRAININIT<-HCPC(train, consol=TRUE, method="ward", metric="euclidean", order=TRUE)
res.hcpc_20230227_TRAININIT

res.hcpc_20230227_TRAIN3grp<-HCPC(train, consol=TRUE, method="ward", metric="euclidean", order=TRUE, nb.clust = 3)
res.hcpc_20230227_TRAIN3grp

res.hcpc_20230227_TRAIN3grp$desc.axes

res.hcpc_20230227_TRAIN4grp<-HCPC(train, consol=TRUE, method="ward", metric="euclidean", order=TRUE, nb.clust = 4)
res.hcpc_20230227_TRAIN4grp
res.hcpc_20230227_TRAIN4grp$desc.axes

#the hierarchical cluster tree initially recommended 3 clusters in the training set
fviz_cluster(res.hcpc_20230227_TRAININIT, repel=TRUE, show.clust.cent = TRUE, palette="Dark2",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))

#the hierarchical cluster tree also run 4 cluster in the training set
fviz_cluster(res.hcpc_20230227_TRAIN4grp, repel=TRUE, show.clust.cent = TRUE, palette="Dark2",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))

#KDO has requested a new color pallete

install.packages("viridis")  # Install
library("viridis")           # Load
#1
fviz_cluster(res.hcpc_20230227_TRAIN4grp, repel=TRUE, show.clust.cent = TRUE, palette="YlGnBu",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))

#2
fviz_cluster(res.hcpc_20230227_TRAIN4grp, repel=TRUE, show.clust.cent = TRUE, palette="Set1",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))

#3
fviz_cluster(res.hcpc_20230227_TRAIN4grp, repel=TRUE, show.clust.cent = TRUE, palette="Set2",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))


#4
fviz_cluster(res.hcpc_20230227_TRAIN4grp, repel=TRUE, show.clust.cent = TRUE, palette="Spectral",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))

#5




#use the clValid package for internal cluster validation
library(clValid)
?clValid

#convert to data frame
res.hcpc20230227df <- as.data.frame(res.hcpc_20230227_TRAININIT$data.clust)
#then convert to matrix
res.hcpc20230227mat <-data.matrix(res.hcpc20230227df)

intern_val20230227<-clValid(res.hcpc20230227mat, 3:4,clMethods=c("hierarchical"),validation="internal")
summary(intern_val20230227)

?fviz_nbclust
fviz_nbclust(train, hcut, method = "wss") + geom_vline(xintercept = 4, linetype = 2) + geom_vline(xintercept = 3, linetype = 2)



#validated in the the testing sample

res.hcpc_20230227_Test3GRP<-HCPC(test, consol=TRUE, method="ward", metric="euclidean", nb.clust = 3, order=TRUE)

fviz_cluster(res.hcpc_20230227_Test3GRP, repel=TRUE, show.clust.cent = TRUE, palette="Dark2",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))


res.hcpc_20230227_Test4GRP<-HCPC(test, consol=TRUE, method="ward", metric="euclidean", nb.clust = 4, order=TRUE)

fviz_cluster(res.hcpc_20230227_Test4GRP, repel=TRUE, show.clust.cent = TRUE, palette="Dark2",
             eclipse.type="t", ggtheme=theme_bw(), main="Cluster Assignment by Factor 1 and 2", geom=c("point"), axes=c(1,2))


# decided on a 4  group designation--print out results for training and validation dataset
?HCPC

#training
res.hcpc_20230227_TRAIN4grp_df <- as.data.frame(res.hcpc_20230227_TRAIN4grp$data.clust)
write.csv(res.hcpc_20230227_TRAIN4grp_df, "res.hcpc_20230227_TRAIN4g_df.csv")

#testing
res.hcpc_20230227_TEST4g_df <- as.data.frame(res.hcpc_20230227_Test4GRP$data.clust)
write.csv(res.hcpc_20230227_TEST4g_df, "res.hcpc_20230227_Test4g_df.csv")

#completeobs
write.csv(Imp_LETBI_202302277_DF$completeObs, "res.hcpc_20230227_COMPLETEOBS.csv")

