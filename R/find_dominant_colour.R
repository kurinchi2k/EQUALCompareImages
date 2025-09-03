find_dominant_colour <- function(image) {
  image_as_integer <- as.integer(image[[1]])
  image_df <- data.frame(red = c(image_as_integer[,,1]), green = c(image_as_integer[,,2]), blue = c(image_as_integer[,,3]))
  kmeans_cluster_means <- kmeans(image_df, 1)$centers
  output <- rgb(kmeans_cluster_means/255)
  return(output)
}
