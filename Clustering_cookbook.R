# Clustering

## Which algorithm to choose?
# K-means clustering can handle larger datasets than hierarchical cluster approaches. 
# Additionally, observations are not permanently committed to a cluster. 
# They are moved when doing so improves the overall solution. 
# However, the use of means implies that all variables must be continuous and the approach can be severely 
# affected by outliers. They also perform poorly in the presence of non-convex (e.g., U-shaped) clusters.

# See also sklearn.cluster documentation for useful table: http://scikit-learn.org/stable/modules/clustering.html

# Also useful: http://cran.r-project.org/web/views/Cluster.html

# ---------------------------------------------------------------------------------------------------------------------
## Load data
data <- mtcars
head(data)
dim(data)

## Prepare data
data <- na.omit(data) # listwise deletion of missing
data <- as.data.frame(scale(data)) # standardize variables

## Exploratory plots
windows()
par(mfrow=c(2,2))
hist(data$mpg,main="Miles per gallon")
plot(data$cyl, data$gear, main = "Gear vs Cylinders")
hist(data$hp, main = "Horse Power")
plot(data$hp, data$wt, main = "Weight Vs Horse Power ")
plot

## Plot the observations according to the first two components
library(cluster)
windows()
clusplot(data,kmeans(data,1)$cluster,lines=2,labels=0)

## Test correlations
library(corrplot) # see also: http://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
M <- cor(data)    # calculate the correlation matrix

windows()
corrplot(M,order="hclust",tl.col="black",addrect=3)

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res <- cor.mtest(data, 0.95)

windows()
corrplot.mixed(M,lower="circle",upper="number", order="hclust",tl.col="black", p.mat = res1[[1]], sig.level = 0.05)

## Determine the features that are highly correlated
highlyCor <- findCorrelation(M, cutoff = 0.75)
# Remove highly correlated features from data
data_filt <- data[,-highlyCor]
colnames(data)[highlyCor]

# Make new corrplots of filtered data
M_filt <- cor(data_filt)

windows()
corrplot(M_filt,order="hclust",tl.col="black")
res <- cor.mtest(data_filt, 0.95)
windows()
corrplot.mixed(M_filt,lower="circle",upper="number", order="hclust",tl.col="black", p.mat = res[[1]], sig.level = 0.05)

# ---------------------------------------------------------------------------------------------------------------------

## K Means
# Determine number of clusters
wss <- (nrow(data_filt)-1)*sum(apply(data_filt,2,var))      # within sum of squares for full data set (so for #clusters = 1)
for (i in 2:15) wss[i] <- sum(kmeans(data_filt, 
                                     centers=i)$withinss)
windows()
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# From this plot, determine the flexion point and form the clusters
cl_Kmeans <- kmeans(data_filt, centers=5)
cl_Kmeans$cluster                     # cluster indices

## K Means using fpc // pamk
library(fpc)
cl_pamK <- pamk(data_filt,krange=2:15)
# optimal numer of clusters:
cl_pamK$nc
cl_pamK$pamobject$clustering          # cluster indices

# ---------------------------------------------------------------------------------------------------------------------

## Hierarchical Clustering

# Using hclust
d <- dist(data_filt, method = "euclidean")  # need to calculate a distance matrix first
clustree <- hclust(d, method="ward.D")      # calculate a dendogram
windows()
plot(clustree)                              # display dendogram
cl_hier5 <- cutree(clustree, k=5)           # cut tree into 5 clusters  # = cluster indexes
rect.hclust(clustree, k=5, border="red")    # draw dendogram with red borders around the 5 clusters 

windows()
plot(clustree)                              # display dendogram
cl_hier12 <- cutree(clustree, k=12)         # cut tree into 12 clusters
rect.hclust(clustree, k=12, border="red")   # draw dendogram with red borders around the 12 clusters 

# using 'pvclust'
library(pvclust)
data_filt_T <- t(data_filt)                 # clusters columns instead of rows, so need to transpose data first!
pvclustree <- pvclust(data_filt_T, method.hclust="ward.D",
               method.dist="euclidean")
plot(pvclustree) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(pvclustree, alpha=.95)
cl_pvclust <- pvpick(pvclustree, alpha=.95)
cl_pvclust$clusters                         # list of observations that fall in each cluster (does not include ALL observations!)
# ---------------------------------------------------------------------------------------------------------------------

## Model-ased clustering
library(mclust)
cl_mclust <- Mclust(data_filt)
plot(cl_mclust) # plot results 
summary(cl_mclust) # display the best model
cl_mclust$classification                    # cluster indices

# ---------------------------------------------------------------------------------------------------------------------

# Plotting cluster results
library(cluster)
windows()
clusplot(data_filt, cl_Kmeans$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# centroid plot (from the fpc package)
windows()
plotcluster(data_filt, cl_Kmeans$cluster)
windows()
plotcluster(data_filt, cl_pamK$pamobject$clustering)

# ---------------------------------------------------------------------------------------------------------------------

## Comparing the accuracy of various clustering results
d <- dist(data_filt, method = "euclidean")  # need to calculate a distance matrix first
cluster.stats(d, cl_Kmeans$cluster, alt.clustering = cl_pamK$pamobject$clustering)

cluster.stats(d, cl_pamK$pamobject$clustering, alt.clustering = cl_Kmeans$cluster )

## Internal evaluation (when there is no labeled data)
# Dunn index  [dunn]   -->  ratio between the minimal inter-cluster distance to maximal intra-cluster distance
#                      --> better model IF high
# Silhouette  [avg.silwidth]   -->  contrasts the average distance to elements in the same cluster with the average distance to elements in other clusters
#                      --> better model IF high
# Davies-Bouldin --> better model IF small

## External evaluation (when there is labeled data to check the clusters)
# Adjusted Rand index --> adjustedRandIndex(clusterPred, clusterTruth)  (from mclust package) (-1: no agreement, 1: perfect agreement)
# F score
# Confusion matrix --> confusionMatrix(clusterPred, clusterTruth)

# ---------------------------------------------------------------------------------------------------------------------

## Determine feature importance
library(randomForest)

data_clust <- data.frame(data_filt, Cluster = as.factor(cl_Kmeans$cluster))
rf <- randomForest(Cluster~., data = data_clust, ntree = 100, importance = TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

windows()
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
print(p)

# ---------------------------------------------------------------------------------------------------------------------

## Repeating the analysis with the full data (not removing highly correleated features)

cl_full_Kmeans <- kmeans(data, centers=5)
data_clust_full <- data.frame(data, Cluster = as.factor(cl_Kmeans$cluster))

rf <- randomForest(Cluster~., data = data_clust_full, ntree = 100, importance = TRUE)

## Confusion matrix for Random Forest model
rf$confusion

## Calculate feature importance
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

windows()
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
print(p)

cluster.stats(d, cl_Kmeans$cluster, alt.clustering = cl_full_Kmeans$cluster)$dunn
cluster.stats(d, cl_full_Kmeans$cluster, alt.clustering = cl_Kmeans$cluster)$dunn  # --> better

cluster.stats(d, cl_Kmeans$cluster, alt.clustering = cl_full_Kmeans$cluster)$avg.silwidth  # --> better
cluster.stats(d, cl_full_Kmeans$cluster, alt.clustering = cl_Kmeans$cluster)$avg.silwidth

# ---------------------------------------------------------------------------------------------------------------------

## Special cases:

# Sparse data: # features >> # observations  
# --> 'sparcl' package
# Sparse K-means --> KMeansSparseCluster
# Sparse hierarchical clustering --> HierarchicalSparseCluster

# features are a mix of numeric and categorical variables 
# --> First, calculate the dissimilarity matrix using daisy (from 'cluster' package). Make sure to set the metric to "gower"
# d <- daisy(data, metric = "gower")
# Then, cluster using hierarchical clustering


