compare_similarities_two_images <- function(rv) {
  image_1 <- image_read(rv$file_upload_image_1$datapath)
  image_2 <-image_read(rv$file_upload_image_2$datapath)
  stretch_images <- (rv$stretch_images == "Yes")
  number_of_resizes <- rv$number_of_resizes_between
  extra_parameters_requested <- rv$extra_parameters_between
  if (TRUE %in% (extra_parameters_requested == "")) {
    extra_parameters == ""
  } else {
    extra_parameters <- {cbind.data.frame(
      display_name = c("fuzz", "mean_absolute_error_normalized", "mean_error_per_pixel",
                       "normalized_cross_correlation", "peak_absolute_error",
                       "peak_signal_to_noise_ratio", "root_mean_squared_error"),
      metric_name = c("Fuzz", "MAE", "MEPP", "NCC", "PAE", "PSNR", "RMSE")
    )}
    extra_parameters <- extra_parameters[extra_parameters$display_name %in% extra_parameters_requested,]
  }
  try(notify_info(paste0("comparing images...\n"), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  resizes <- seq(from = 0, to = 1, length.out = (number_of_resizes + 2))
  resizes <- resizes[! (resizes %in% c(0,1))]
  comparisons <- list()
  if (stretch_images == TRUE) {
    stretched_images <- stretch_images(image_1 = image_1, image_2 = image_2)
    image_1 <- stretched_images$image_1
    image_2 <- stretched_images$image_2
  }
  # Direct comparison
  try(notify_info(paste0("\nComparing images without reduction in size..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  comparisons[[1]] <- cbind.data.frame(change = if (stretch_images == TRUE) {"Stretched image"} else {"No alteration to image"}, 
                                       pairwise_comparison(image_1, image_2, include_dimension_insensitive_measures = TRUE,
                                                           extra_parameters = extra_parameters))
  # Rotations
  try(notify_info(paste0("\nComparing images without reduction in size (after rotation)..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  comparisons[2:4] <- lapply(1:3, function(rotation){
    degrees_rotation <- rotation * 90
    rotated_image <- image_rotate(image_2, degrees_rotation)
    output <- cbind.data.frame(change = paste0(degrees_rotation, " degrees rotation"), 
                               pairwise_comparison(image_1, rotated_image,
                                                   extra_parameters = extra_parameters))
    return(output)
  })
  # Horizontal mirror image and rotation
  try(notify_info(paste0("\nComparing images without reduction in size (horizontal mirror images and rotation)..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  horizontal_mirror_image_2 <- image_flip(image_2)
  comparisons[[5]] <- cbind.data.frame(change = "horizontal mirror image", 
                                       pairwise_comparison(image_1, horizontal_mirror_image_2,
                                                           extra_parameters = extra_parameters))
  comparisons[6:8] <- lapply(1:3, function(rotation){
    degrees_rotation <- rotation * 90
    rotated_image <- image_rotate(horizontal_mirror_image_2, degrees_rotation)
    output <- cbind.data.frame(change = paste0("horizontal mirror image; ", degrees_rotation, " degrees rotation"), 
                               pairwise_comparison(image_1, rotated_image, extra_parameters = extra_parameters))
    return(output)
  })
  # Vertical mirror image and rotation
  try(notify_info(paste0("\nComparing images without reduction in size (Vertical mirror images and rotation)..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  vertical_mirror_image_2 <- image_flop(image_2)
  comparisons[[9]] <- cbind.data.frame(change = "vertical mirror image", 
                                       pairwise_comparison(image_1, vertical_mirror_image_2,
                                                           extra_parameters = extra_parameters))
  comparisons[10:12] <- lapply(1:3, function(rotation){
    degrees_rotation <- rotation * 90
    rotated_image <- image_rotate(vertical_mirror_image_2, degrees_rotation)
    output <- cbind.data.frame(change = paste0("vertical mirror image; ", degrees_rotation, " degrees rotation"), 
                               pairwise_comparison(image_1, rotated_image,
                                                   extra_parameters = extra_parameters))
    return(output)
  })
  comparisons <- do.call(rbind.data.frame,comparisons)
  image_2_dimensions <- image_info(image_2)
  if (number_of_resizes > 0) {
    resized_comparisons <- lapply(1:length(resizes), function(resize) {
      scaling_factor <- resizes[resize]
      image_2_resized <- image_resize(image_2, geometry = geometry_size_pixels(width = scaling_factor * image_2_dimensions$width, preserve_aspect = TRUE))
      comparisons_resized <- list()
      # Direct comparison
      try(notify_info(paste0("\nComparing images after resizing to ", round(scaling_factor * 100, 1) ,"%..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
      comparisons_resized[[1]] <- cbind.data.frame(change = paste0("resize ", scaling_factor), 
                                                   pairwise_comparison(image_1, image_2_resized,
                                                                       extra_parameters = extra_parameters))
      # Rotations
      try(notify_info(paste0("\nComparing images after resizing to ", round(scaling_factor * 100, 1) ,"% (after rotation)..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
      comparisons_resized[2:4] <- lapply(1:3, function(rotation){
        degrees_rotation <- rotation * 90
        rotated_image <- image_rotate(image_2_resized, degrees_rotation)
        output <- cbind.data.frame(change = paste0("resize ", scaling_factor, "; ", degrees_rotation, " degrees rotation"), 
                                   pairwise_comparison(image_1, rotated_image,
                                                       extra_parameters = extra_parameters))
        return(output)
      })
      # Horizontal mirror image and rotation
      try(notify_info(paste0("\nComparing images after resizing to ", round(scaling_factor * 100, 1) ,"% (horizontal mirror images and rotation)..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
      horizontal_mirror_image_2 <- image_flip(image_2_resized)
      comparisons_resized[[5]] <- cbind.data.frame(change = paste0("resize ", scaling_factor, "; ", "horizontal mirror image"), pairwise_comparison(image_1, horizontal_mirror_image_2,
                                                                                                                                                    extra_parameters = extra_parameters))
      comparisons_resized[6:8] <- lapply(1:3, function(rotation){
        degrees_rotation <- rotation * 90
        rotated_image <- image_rotate(horizontal_mirror_image_2, degrees_rotation)
        output <- cbind.data.frame(change = paste0("resize ", scaling_factor, " ", "horizontal mirror image; ", degrees_rotation, " degrees rotation"), 
                                   pairwise_comparison(image_1, rotated_image,
                                                       extra_parameters = extra_parameters))
        return(output)
      })
      # Vertical mirror image and rotation
      try(notify_info(paste0("\nComparing images after resizing to ", round(scaling_factor * 100, 1) ,"% (vertical mirror images and rotation)..."), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
      vertical_mirror_image_2 <- image_flop(image_2_resized)
      comparisons_resized[[9]] <- cbind.data.frame(change = paste0("resize ", scaling_factor, "; ", "vertical mirror image"), 
                                                   pairwise_comparison(image_1, vertical_mirror_image_2,
                                                                       extra_parameters = extra_parameters))
      comparisons_resized[10:12] <- lapply(1:3, function(rotation){
        degrees_rotation <- rotation * 90
        rotated_image <- image_rotate(vertical_mirror_image_2, degrees_rotation)
        output <- cbind.data.frame(change = paste0("resize ", scaling_factor, "; ", "vertical mirror image; ", degrees_rotation, " degrees rotation"), 
                                   pairwise_comparison(image_1, rotated_image,
                                                       extra_parameters = extra_parameters))
        return(output)
      })
      comparisons_resized <- do.call(rbind.data.frame,comparisons_resized)
      return(comparisons_resized)
    })
    resized_comparisons <- do.call(rbind.data.frame, resized_comparisons)
  } else {
    resized_comparisons <- NULL
  }
  results <- rbind.data.frame(comparisons, resized_comparisons)
  write.csv(results, paste0(tempdir(), "/comparison_results.csv"), row.names = FALSE, na = "")
  return(results)
}
