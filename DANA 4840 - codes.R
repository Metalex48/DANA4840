# 4840 codes ####



## Jan 12 - Euclidean distance#### 

attach(buddymove_holidayiq)
head(buddymove_holidayiq)

df <- buddymove_holidayiq[1:15, 2:7] # choosing first 15 rows. Choosing columns 2 to 7 to avoid text variables.
df.scaled <- scale(df) # Standardize the variables

library(stats)
dist.eucl <- dist(df.scaled, method = "euclidean") # set the distances

library(factoextra)
fviz_dist(dist.eucl) # visualize the matrix










## Jan 17 - K-Means Clustering #### 

attach(buddymove_holidayiq)

df <- buddymove_holidayiq[1:30, 2:7] # choosing first 30 rows and columns 2 to 7 to avoid text variables.
df_scaled <- scale(df) # Scaling the data

library(ggplot2)
library(factoextra)

fviz_nbclust(df_scaled, kmeans, method = "wss") 

# running 3 clusters
km.res <- kmeans(df_scaled, 3, nstart = 25) 
print(km.res)
aggregate(df, by=list(cluster=km.res$cluster), mean) # visualizing the mean values
fviz_cluster(km.res, data = df_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

# running 4 clusters
km.res <- kmeans(df_scaled, 4, nstart = 25)
print(km.res)
aggregate(df, by=list(cluster=km.res$cluster), mean) # visualizing the mean values
fviz_cluster(km.res, data = df_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

# running 5 clusters
km.res <- kmeans(df_scaled, 5, nstart = 25)
print(km.res)
aggregate(df, by=list(cluster=km.res$cluster), mean) # visualizing the mean values
fviz_cluster(km.res, data = df_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "red"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())








## Jan 19 - K-Medoid Clustering ####

attach(buddymove_holidayiq)
head(buddymove_holidayiq)

df <- buddymove_holidayiq[,2:7] # choosing columns 2 to 7 to avoid text variables.
df_scaled <- scale(df) # Scaling the data

library(cluster)
library(factoextra)
fviz_nbclust(df_scaled, pam, method = "silhouette")+
  theme_classic()

# running 3 clusters
pam.res <- pam(df_scaled, 3)
print(pam.res)
fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07", "#E7B800"), # color palette
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

# running 4 clusters
pam.res <- pam(df_scaled, 4)
print(pam.res)
fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07", "#E7B800", "blue"), # color palette
             ellipse.type = "confidence", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_gray())

# running 5 clusters
pam.res <- pam(df_scaled, 5)
print(pam.res)
fviz_cluster(pam.res, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "red"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic())








## Jan 24 - CLARA ####

attach(buddymove_holidayiq)
head(buddymove_holidayiq)

df <- buddymove_holidayiq[,2:7] # choosing columns 2 to 7 to avoid text variables.
df_scaled <- scale(df) # Scaling the data

library(cluster)
library(ggplot2)
library(factoextra)

### Estimating the optimal number of clusters
fviz_nbclust(df_scaled, clara, method = "silhouette")+
  theme_classic()

### Running CLARA algorithm
clara.res <- clara(df_scaled, 2, samples = 50, pamLike = TRUE) # 2 clusters. Use 50 samples.
print(clara.res)

### Adding the row's cluster as an additional column. 
df_wcluster <- cbind(df_scaled, cluster = clara.res$cluster)
head(df_wcluster, n = 4)

### Seeing the Medoids
clara.res$medoids

### Visualizing CLARA clusters
fviz_cluster(clara.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic())










## Jan 26 - Hierarchical Agglomerative ####

#1. View data set
attach(seeds)
head(seeds)

#2. Compute the dissimilarity matrix using euclidean distance.
df_scaled <- scale(seeds, scale = TRUE)
res.dist <- dist(df_scaled, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]

#3. Compute hierarchical clustering using the “average” and “ward.D2” linkage methods using the hclust() function
res.hc1 <- hclust(d = res.dist, method = "average")
res.hc2 <- hclust(d = res.dist, method = "ward.D2")

#4. Plot the dendrograms for the hierarchical clustering.
library("factoextra")
fviz_dend(res.hc1, cex = 0.5)
fviz_dend(res.hc2, cex = 0.5)

#5. Compute cophenetic distance and compute the correlation coefficient using 
# the distance matrix for both methods used. What can you say about the clustering 
# obtained.

# Method = Average: res.hc1
res.coph1 <- cophenetic(res.hc1) # Compute cophenetic distance
cor(res.dist, res.coph1) # Correlation between cophenetic distance and the original distance

# Method = ward.D2: res.hc2
res.coph2 <- cophenetic(res.hc2) # Compute cophenetic distance
cor(res.dist, res.coph2) # Correlation between cophenetic distance and the original distance

#6. At what height do we obtain 3 clusters?

### Method = Average: res.hc1
# Cut tree into 3 groups
grp1 <- cutree(res.hc1, k = 3)
head(grp1, n = 5)
# Number of members in each cluster
table(grp1)
# Visualize the 3 clusters
fviz_dend(res.hc1, k = 3, # Cut in 3 groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
          )

### Method = ward.D2: res.hc2
# Cut tree into 3 groups
grp2 <- cutree(res.hc2, k = 3)
head(grp2, n = 5)
# Number of members in each cluster
table(grp2)
# Visualize the 3 clusters
fviz_dend(res.hc2, k = 3, # Cut in 3 groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
          )

fviz_cluster(list(data = df_scaled, cluster = grp2),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#7. At what height do we obtain 2 clusters?

### Method = Average: res.hc1
# Cut tree into 2 groups
grp3 <- cutree(res.hc1, k = 2)
head(grp3, n = 5)
# Number of members in each cluster
table(grp3)
# Visualize the 3 clusters
fviz_dend(res.hc1, k = 2, # Cut in 2 groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

### Method = ward.D2: res.hc2
# Cut tree into 2 groups
grp4 <- cutree(res.hc2, k = 2)
head(grp4, n = 5)
# Number of members in each cluster
table(grp4)
# Visualize the 3 clusters
fviz_dend(res.hc2, k = 2, # Cut in 2 groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_cluster(list(data = df_scaled, cluster = grp4),
             palette = c("#2E9FDF", "#00AFBB"), 
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())









## Feb 7th - Cluster Validation/Tendency ####

#Test the following data sets to assess whether applying clustering is suitable for the data:
#1. buddymove_holidayiq.csv
#2. seeds.csv
#Use both the Hopkins and VAT approaches


### Buddy Move Holiday ####

df <- scale(buddymove_holidayiq[2:7]) # getting meaningful columns and scaling it.

#### Visual method ####
library("factoextra") # to plot dataset
fviz_pca_ind(prcomp(df), title = "PCA - holidays", 
             habillage = 'none', 
             palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

library(factoextra) # doing K-means analysis on dataset
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

library(factoextra) # plotting dendogram
fviz_dend(hclust(dist(df)), k = 3, k_colors = "jco", 
          as.ggplot = TRUE, show_labels = FALSE)
# I see that the dendogram branches are very high. This means that there is a lot of dissimilarity.
# If there were evident clusters, the branches would split at a lower level.

#### Dissimilarity Matrix or Visual Assessment of cluster Tendency (VAT) algorithm ####
library(factoextra) 
fviz_dist(dist(df), show_labels = FALSE) + labs(title = "Holidays data")

#### Hopkins method ####
library(factoextra)
res <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE)
res$hopkins_stat
# With a value of 0.82, it seems like the data set is indeed clusterable.


### Seeds data set ####

df2 <- scale(seeds) # scaling df

#### Visual method ####
library("factoextra") # to plot dataset
fviz_pca_ind(prcomp(df2), title = "PCA - Seeds", 
             habillage = 'none', 
             palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

library(factoextra) # doing K-means analysis on dataset
km.res2 <- kmeans(df2, 3)
fviz_cluster(list(data = df2, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

library(factoextra) # plotting dendogram
fviz_dend(hclust(dist(df2)), k = 3, k_colors = "jco", 
          as.ggplot = TRUE, show_labels = FALSE)
# I see that the dendogram branches are not high. This means that there is not a lot of dissimilarity.

#### Dissimilarity Matrix or Visual Assessment of cluster Tendency (VAT) algorithm ####
library(factoextra) 
fviz_dist(dist(df2), show_labels = FALSE) + labs(title = "Seeds data")

#### Hopkins method ####
library(factoextra)
res <- get_clust_tendency(df2, n = nrow(df2)-1, graph = FALSE)
res$hopkins_stat
# With a value of 0.75, it seems like the data set is also clusterable.







## Feb 9th - Optimal number of Clusters####

attach(cancer_cervix)
df <- scale(cancer_cervix[1:19], center = TRUE, scale = TRUE)
head(df)
#Use the data set to answer the following questions.

#1. Decide on the optimal number of clusters using the Elbow method.
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+ # xintercept is provided by me to indicate the elbow
  labs(subtitle = "Elbow method")

#2. Decide on the optimal number of clusters using the Average Silhouette method.
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# this method indicates that the number of clusters should be 5.

#3. Decide on the optimal number of clusters using the Gap Statistic.
fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
# this method indicates 7 clusters

#4. Using your answer from Q3, use the eclust function to perform k-means and hierarchical clustering.

### NOTE !!!   !!! NOTE !!! !!!
# the Elbow, Silhouette, and Gap methods indicate 5 or more clusters. However, if we pay close
# attention to all graphs, we see that the drastic changes are close to 2 or 3. We do not always 
# have to strictly follow what they suggest. Also, because the dataset has labels "Yes" and "No", 
# we'll proceed with 2 clusters, to later get the Rand's Index.

result_eclust <- eclust(df, "pam", k = 2, graph = TRUE) # storing the result in result_eclust for further analysis
fviz_dend(result_eclust) # visualize dendrogram. This will not work with PAM or K-means. 
fviz_silhouette(result_eclust) # visualize silhouette
fviz_cluster(result_eclust) # visualize cluster

result_hcut <- hcut(df, k = 2, hc_func = "hclust", graph = TRUE) #another method using HCUT function to plot the following:
fviz_dend(result_hcut) # visualize dendrogram
fviz_silhouette(result_hcut) # visualize silhouette
fviz_cluster(result_hcut) # visualize cluster

#5. Assess the clusters internally using the silhouette coefficient.
# A value close to 1 indicates that the object is well clustered. In the other words, the object i is similar to the other objects in its group.
# A value close to -1 indicates that the object is poorly clustered, and that assigning it to some other cluster would probably improve the overall results.
fviz_silhouette(result_eclust, palette = "jco", ggtheme = theme_classic())
silho_info_pam <- result_eclust$silinfo
silho_info_pam

fviz_silhouette(result_hcut, palette = "jco", ggtheme = theme_classic())
silho_info_hclust <- result_hcut$silinfo
silho_info_hclust

#6. Assess the clusters internally using the Dunn’s index.
# Statistics for k-means clustering
library(fpc)
km_stats_pam <- cluster.stats(dist(df), result_eclust$cluster)
km_stats_hclust <- cluster.stats(dist(df), result_hcut$cluster)

# Dunn index
km_stats_pam$dunn
km_stats_hclust$dunn

#7. Assess the clusters externally using Rand’s Index

# For this, we need to compare the clustering method with the real labels: ca_cervix = Yes or No
label <- ifelse(cancer_cervix$ca_cervix == 'No', 0, 1)
label

library(fpc)
km_stats_pam <- cluster.stats(dist(df), label, result_eclust$cluster)
km_stats_hclust <- cluster.stats(dist(df), label, result_hcut$cluster)

# Rand's Index. The values goes between -1 and 1. We want a value close to 1. 
km_stats_pam$corrected.rand
km_stats_hclust$corrected.rand

# VI index
km_stats_pam$vi
km_stats_hclust$vi





## Feb 28th - Choosing best Cluster Algorithm ####

# Use the cancer_cervix data set to answer the following questions.
attach(cancer_cervix)
df <- scale(cancer_cervix[,-20])
head(df)

#1. Use the clValid function in R to find the best algorithm for clustering the data.
methods <- c("hierarchical","kmeans","diana", "pam", "clara")

### Validation = internal ####
library(clValid)
intern <- clValid(df, nClust = 2:7,
                  clMethods = methods, 
                  validation = "internal")

summary(intern) # The best algorithm would be hierarchical and 2 clusters.

# Read explanation from Help window by typing ClValid. Here is an excerpt:
## The connectivity has a value between 0 and infinity and should be minimized.
## The Dunn Index is the ratio between the smallest distance between observations not in the same cluster to the largest intra-cluster distance. It has a value between 0 and infinity and should be maximized.
## The Silhouette value measures the degree of confidence in a particular clustering assignment and lies in the interval [-1,1], with well-clustered observations having values near 1 and poorly clustered observations having values near -1.


### Validation = stability ####
library(clValid)
stab <- clValid(df, nClust = 2:7,
                  clMethods = methods, 
                  validation = "stability")

summary(stab) # According to the APN and ADM measures, the optimal method would be hierarchical with 2 cluster.
              # According to AD and FOM measures, the optimal method would be DIANA with 7 clusters.

#2. Use the pvclust function in R to obtain the BP and AU p-values for the hierarchical
#clustering. Which clusters are strongly supported by the data?

library(pvclust)
res.pv <- pvclust(df, method.dist="cor", 
                  method.hclust="average", nboot = 10)
# Default plot
plot(res.pv, hang = -1, cex = 0.5)
pvrect(res.pv) # plotting red rectangles around strong clusters
# Values on the dendrogram are AU p-values (Red, left), BP values (green, right), and cluster 
# labels (grey, bottom). Clusters with AU > = 95% are indicated by the rectangles and are 
# considered to be strongly supported by data.
