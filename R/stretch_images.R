stretch_images <- function(image_1, image_2) {
  image_1_dimensions <- image_info(image_1)
  image_2_dimensions <- image_info(image_2)
  max_height <- max(image_1_dimensions$height, image_2_dimensions$height)
  max_width <- max(image_1_dimensions$width, image_2_dimensions$width)
  image_1 <- image_resize(image_1, paste0(max_width, "x", max_height, "!"))
  image_2 <- image_resize(image_2, paste0(max_width, "x", max_height, "!"))
  output <- list(image_1 = image_1, image_2 = image_2)
  return(output)
}
