MY.track_within_ss <- function(data, max.k)
{
  # first column is k and second column is total within cluster ss
  output <- matrix(0, ncol = 2, nrow = max.k)
  output[,1] <- 1:max.k
  
  for (i in 1:max.k) {
    
    km_results <- kmeans(data, centers = i)
    
    output[i,2] <- km_results$tot.withinss
  }
  
  return(output)
}