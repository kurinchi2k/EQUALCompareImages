pairwise_comparison <- function(image_1, image_2, include_dimension_insensitive_measures = FALSE, remove_dominant_colour = FALSE, extra_parameters = "") {
  if (missing(extra_parameters)) {extra_parameters = ""}
  if (remove_dominant_colour == TRUE) {
    image_1 <- image_flatten(image_1)
    image_2 <- image_flatten(image_2)
    image_1 <- image_transparent(image_1, color = find_dominant_colour(image_1), fuzz = 10)
    image_2 <- image_transparent(image_2, color = find_dominant_colour(image_2), fuzz = 10)
  }
  if (include_dimension_insensitive_measures == TRUE) {
    perceptual_hash <- attributes(image_compare(image_2, image_1, metric = "PHASH"))[[2]]
  } else {
    perceptual_hash <- NA
  }
  # One can one only compare the areas that are common to both images
  image_1_dimensions <- image_info(image_1)
  image_2_dimensions <- image_info(image_2)
  min_height <- min(image_1_dimensions$height, image_2_dimensions$height)
  min_width <- min(image_1_dimensions$width, image_2_dimensions$width)
  image_1 <- image_crop(image_1, geometry = geometry_area(width = min_width, height = min_height))
  image_2 <- image_crop(image_2, geometry = geometry_area(width = min_width, height = min_height))
  absolute_error <- attributes(image_compare(image_2, image_1, metric = "AE"))[[2]]
  pixels_compared <- min_height*min_width
  absolute_error_percent <- absolute_error/pixels_compared
  if (is.data.frame(extra_parameters)) {
    if(nrow(extra_parameters) > 0) {
      extra_parameters_values <- lapply(1:nrow(extra_parameters), function(x) {
        attributes(image_compare(image_2, image_1, metric = extra_parameters$metric_name[x]))[[2]]
      })
      names(extra_parameters_values) <- extra_parameters$display_name
      output <- cbind.data.frame(pixels_compared = pixels_compared, 
                                 absolute_error = absolute_error, 
                                 absolute_error_percent = absolute_error_percent,
                                 perceptual_hash = perceptual_hash,          
                                 extra_parameters_values
      )
    } else{
      output <- cbind.data.frame(pixels_compared = pixels_compared, 
                                 absolute_error = absolute_error, 
                                 absolute_error_percent = absolute_error_percent,
                                 perceptual_hash = perceptual_hash
      )  
    }
  } else {
    output <- cbind.data.frame(pixels_compared = pixels_compared, 
                               absolute_error = absolute_error, 
                               absolute_error_percent = absolute_error_percent,
                               perceptual_hash = perceptual_hash
    )
  }
  return(output)
}
