#' This function Calculates outliers using the DBSCAN algorithm
#' 
#' Given a data.frame, returns a vector indicating whether each observations was deemed an outlier or not
#' @usage dbscan_outlier(df, eps_range = seq(0.5, 2, 0.3), MinPts_range = seq(3, 6, 1), scale = T, skip = c())
#' @param df: data.frame with features over which to run the algorithm
#' @param eps_range: range of eps values to test in order to provide feedback to the user about what the algorithm is finding
#' @param MinPts_range: range of eps values to test in order to provide feedback to the user about what the algorithm is finding
#' @param scale: whether or not to scale the data prior to running the algorithm
#' @param skip: character vector of names of columns to be skipped 
dbscan_outlier <- function(df, eps_range = seq(0.5, 2, 0.3), MinPts_range = seq(3, 6, 1), scale = T, skip = c()){
  
  require(fpc, quietly = T)
  if (length(skip) > 0) df <- df[, setdiff(names(df), skip)]
  
  #Loop through all combination of eps_range and MinPts_range
  for (eps in eps_range){
    
    for (MinPts in MinPts_range){
      
      #run the dbscan
      dbtest <- dbscan(df, eps, MinPts = MinPts, scale = scale)
      
      #Get the predictions
      predictions <- predict(dbtest, df)
      dbscan_outlier <- ifelse(predictions == 0, 1, 0)
      cat(paste("eps =", eps, "MinPts =", MinPts, ":", "number of dbscan outliers =", sum(dbscan_outlier), '\n'))  
      
    }
    
  }
  
  #Let the user decide what cutoffs to use
  eps <- as.numeric(readline("What should the minimum distance (eps) be? "))
  minpts <- as.numeric(readline("What should the minimum number of points be? "))
  
  #run the dbscan
  dbtest <- dbscan(df, eps, MinPts = minpts, scale = scale)
  
  #Get the predictions
  predictions <- predict(dbtest, df)
  dbscan_outlier <- ifelse(predictions == 0, 1, 0)
  dbscan_outlier
  
}

#' Detects outliers using k-means clustering
#' 
#' Given a data.frame, determines which points lie furthest away from their cluster centroids
#' @usage kmeans_outlier (df, cluster_max = 10, percentile = 95, k_scale = T, skip = c())
#' @param df: data.frame over which to run the algorithm
#' @param cluster_max: the maximum number of clusters to test for providing feedback to the user
#' @param percentile: the distance percentile in order to be deemed an outlier?
#' @param scale: whether or not to scale the inputs
#' @param skip: characer vector of which columns the algorithm should skip
kmeans_outlier <- function(df, cluster_max = 10, percentile = 95, k_scale = T, skip = c()){
  
  df <- if (length(skip) > 0) 
    df[, -(which(names(df) %in% skip))]
  else df
  
  #Scale the data if necessary
  df <- if (k_scale) as.data.frame(lapply(df, function(x) scale(x)))
  else df
  
  ss_ratio <- rep(0, cluster_max - 1)
  
  #Check between 2 and 10 to and capture the cohesion
  for (i in 2:cluster_max){
    new_kmeans <- kmeans(df, i)
    ss_ratio[i-1] <- new_kmeans$betweenss / new_kmeans$totss 
  }
  
  plot(y = ss_ratio, x = c(2:cluster_max), xlab = 'Number of clusters', main = "Kmeans cohesion measure (between_SS / total_SS")
  
  #Get the number of clusters to use from the user
  k <- as.integer(readline(prompt = 'Based on the plot, what should \'k\' be? '))
  
  #Use user defined input
  df_kmeans <- kmeans(df, k)
  
  #Get the distance between each point and its cluster center
  euc_dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  j <<- 1
  
  # Get the distances
  dist <- apply(df, MARGIN = 1, FUN = function(x){
    
    cluster <- df_kmeans$cluster[j]
    center <- df_kmeans$centers[cluster,]
    j <<- j + 1 #bc global
    euc_dist(center, x)
  })
  
  # Grab the top whatever percentile
  cutoff <- quantile(dist, probs = seq(0, 1, 0.01))[percentile + 1]
  kmeans_outlier <- ifelse(dist > cutoff, 1, 0)
  kmeans_outlier
  
}

#' Get outliers using Classifier-Adjusted Density Estimation (CADE)
#' 
#' Given a data.frame with a set of observations, determines which observations are likely to be outliers using CADE.
#' @usage cade_outlier(df, prop = 1, skip = c(), range = seq(0.3, 0.7, 0.1), ...)
#' @param df: input data.frame
#' @param prop: proportion of fake to real data
#' @param skip: which columns the algorithm should skip
#' @param range: range of cutoffs to try with cade and push to the user 
cade_outlier <- function(df, prop = 1, skip = c(), range = seq(0.3, 0.7, 0.1), ...){
  
  require(CADE, quietly = T)
  
  df <- df[complete.cases(df), ]
  ids <- df[, skip]
  real <- if (length(skip) > 0) 
    df[, -(which(names(df) %in% skip))]
  else df
  fake <- as.data.frame(lapply(real, function(x) uniform(x, 
                                                         length(x) * prop)))
  real$y <- 0
  fake$y <- 1
  data <- rbind(real, fake)
  tree <- randomForest::randomForest(as.factor(y) ~ ., data = data, 
                                     importance = TRUE)
  vars <- names(attr(tree$terms, "dataClasses")[-1])
  prop <- tree$importance[, 1]/sum(tree$importance[, 1])
  df$prob <- predict(tree, newdata = df, type = "prob")[, 2]
  df$prob <- df$prob/(1 - df$prob)
  
  for (cutoff in range){
    
    df$outlier <- ifelse(df$prob > cutoff, 1, 0)
    cat(paste("Number of outliers for cutoff of ", cutoff, "-->", length(df$outlier[df$outlier == 1]), '\n'))
    
  }
  
  new_cutoff <- as.numeric(readline(prompt = 'What cutoff would you like to use? '))
  return(ifelse(df$prob > new_cutoff, 1, 0))
  
}

#' Master function to three outlier detection methods at once and ensemble the results
#' 
#' Given a data.frame of observations, determines outliers using dbscan, kmeans, and CADE and returns a dataframe indicating whether each observation was an outlier for that method as well as a sum of the number of the outlier detection methods that voted that observation as an outlier.
#' @usage outlier_ensemble (input_df, dbscan_eps_range = seq(0.5, 2, 0.3), dbscan_MinPts_range = seq(3, 6, 1), dbscan_scale = T, kmeans_cluster_max = 10, kmeans_percentile = 95, kmeans_scale = T, cade_prop = 1, all_skip = c(), cade_range = seq(0.3, 0.7, 0.1))
#' @param all: see documentation on the three outlier detection functions included in this package
outlier_ensemble <- function(input_df, dbscan_eps_range = seq(0.5, 2, 0.3), dbscan_MinPts_range = seq(3, 6, 1), dbscan_scale = T, kmeans_cluster_max = 10, kmeans_percentile = 95, kmeans_scale = T, cade_prop = 1, all_skip = c(), cade_range = seq(0.3, 0.7, 0.1)){
  
  #Run the outlier methods
  dbscan_outlier <- dbscan_outlier(df = input_df, eps_range = dbscan_eps_range, MinPts_range = dbscan_MinPts_range, scale = dbscan_scale, skip = all_skip)
  
  #run kmeans
  kmeans_outlier <- kmeans_outlier(df = input_df, cluster_max = kmeans_cluster_max, percentile = kmeans_percentile, k_scale = kmeans_scale, skip = all_skip)
  
  #run cade outlier
  cade_outlier <- cade_outlier(df = input_df, prop = cade_prop, skip = all_skip, range = cade_range)
  
  #Add them to the data frame
  input_df$dbscan_outlier <- dbscan_outlier
  input_df$kmeans_outlier <- kmeans_outlier
  input_df$cade_outlier <- cade_outlier
  
  #Sum all the outliers
  input_df$outlier_sum <- input_df$dbscan_outlier + input_df$kmeans_outlier + input_df$cade_outlier
  
  #Return the updated df
  return(input_df)
}