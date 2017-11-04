# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[-1]) # standardize the variables

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  }

wssplot(df)

# Exercise 2:

#   * How many clusters does this method suggest?
# This method seems to suggest that 3 clusters would be optimal. After
# 3 clusters, the observed difference in within-group sum of squares
# is not very substantial and there is not much bend in the line.

#   * Why does this method work? What's the intuition behind it?
# This method is based on the premise that a good cluster is one which 
# contains the smallest within-cluster variation. The within-group sum
# of squares will decrease with each increase in cluster number, and 
# the optimal number of clusters can be determined from where a bend
# appears on the line graph.

#   * Look at the code for wssplot() and figure out how it works
# The wssplot() function first takes in the data, the number of clusters,
# and the seed setting initializer. It then multiplies the number of 
# rows in the data set by the sum of the column variance in the data set
# and assigns this value to the wss variable. Next a loop is run in which
# the seed is set and then the within-cluster sums of squares for the
# number of cluster centers in that loop is calculated and summed and 
# then loaded into the corresponding index of wss. The loop continues
# until values have been calculated for all numbers of cluster centers
# up to the total number indicated in the original input variable. 
# Finally a point and line plot is generated to plot the number of 
# clusters versus the within-groups sum of squares. 


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# This method suggests that 3 clusters is the optimal number to use.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

set.seed(1234) # set a seed 
fit.km <- kmeans(df, 3, nstart=25)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$cluster
# compare to the actual wine types in wine$Type. Would you consider this a good
# clustering?

ct.km <- table(wine$Type, fit.km$cluster)
ct.km

# Based on a comparison of these tables I would conclude that this is a
# good clustering. Most clusters match the actual wine types.

# Exercise 6:
# * Visualize these clusters using function clusplot() from the cluster library
# * Would you consider this a good clustering?

library(cluster)
clusplot(df, fit.km$cluster, main='Visualization of Clusters', color=TRUE)

# I would still consider this to be a good clustering. There is very
# minimal overlap between clusters and the clustered data points seem
# to be of reasonable distance from the corresponding centers.