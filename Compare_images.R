# Options ####
# Version 0.1 Pre-release version
options(scipen=9999)
# Expand memory and improve options
options(shiny.maxRequestSize = 30*1024^2)
# Functions ####
find_dominant_colour <- function(image) {
  image_as_integer <- as.integer(image[[1]])
  image_df <- data.frame(red = c(image_as_integer[,,1]), green = c(image_as_integer[,,2]), blue = c(image_as_integer[,,3]))
  kmeans_cluster_means <- kmeans(image_df, 1)$centers
  output <- rgb(kmeans_cluster_means/255)
  return(output)
}
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
results_heatmaps <- function(data, column) {
  heatmap_accuracy_only_validation <- ggplot(data = heatmap_only_validation_data,
                                             aes(x = prediction_type, y = model, fill = accuracy)) +
    geom_tile(color = "white",lwd = 0.5, linetype = 1) +
    coord_fixed() +
    theme(axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=1),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_gradient(low = "white", high = "darkblue") + xlab("Prediction type") + ylab("Model") +
    geom_text(aes(label = round(accuracy,2)), color = "white", size = 4)
  ggsave(filename = paste0(plots_folder, "/heatmaps/accuracy_only_validation.png"), height = 7, width = 7, units = "in")
  
}
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
compare_similarities_same_image <- function(rv) {
  image = image_read(rv$file_upload_image$datapath)
  number_of_splits = rv$number_of_splits
  number_of_resizes = rv$number_of_resizes_within
  extra_parameters_requested <- rv$extra_parameters_within
  if (TRUE %in% (extra_parameters_requested == "")) {
    extra_parameters == ""
  } else {
    extra_parameters <- {cbind.data.frame(
      display_name = c('fuzz', 'mean_absolute_error_normalized', 'mean_error_per_pixel',
                       'normalized_cross_correlation', 'peak_absolute_error',
                       'peak_signal_to_noise_ratio', 'root_mean_squared_error'),
      metric_name = c('Fuzz', 'MAE', 'MEPP', 'NCC', 'PAE', 'PSNR', 'RMSE')
    )}
    extra_parameters <- extra_parameters[extra_parameters$display_name %in% extra_parameters_requested,]
  }
  storage_folder = tempfile()
  image_dimensions <- image_info(image)
  x_off <- seq(from = 1, to = image_dimensions$width, length.out = number_of_splits)
  y_off <- seq(from = 1, to = image_dimensions$height, length.out = number_of_splits)
  try(notify_info(paste0("splitting images...\n"), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  split_images <- lapply(1:(number_of_splits-1), function(x) {
    output <- lapply(1:(number_of_splits-1), function(y) {
      image_crop(image = image, geometry = geometry_area(height = image_dimensions$height%/% number_of_splits, width = image_dimensions$width%/% number_of_splits, x_off = x_off[x], y_off = y_off[y]))
    })
    do.call(c, output)
  })
  split_images <- do.call(c, split_images)
  if(dir.exists(storage_folder)){unlink(storage_folder, recursive = TRUE)}
  dir.create(storage_folder, recursive = TRUE)
  dir.create(paste0(storage_folder, "/split_images"), recursive = TRUE)
  dir.create(paste0(storage_folder, "/heatmaps"), recursive = TRUE)
  dir.create(paste0(storage_folder, "/data_for_heatmaps"), recursive = TRUE)
  try(notify_info(paste0("saving images...\n"), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  silence <- sapply(1:length(split_images), function(x) {
    image_write(split_images[x], path = paste0(storage_folder, "/split_images/split_image_",x,".jpeg"),
                format = "jpeg")
  })
  try(notify_info(paste0("comparing images...\n"), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
  resizes <- seq(from = 0, to = 1, length.out = (number_of_resizes + 2))
  resizes <- resizes[! (resizes %in% c(0,1))]
  pairwise_comparisons <- lapply(1:length(split_images), function(x) {
    image_1 <- split_images[x]
    each_pairwise_comparison <- lapply(x:length(split_images), function(z){
      image_2 <- split_images[z]
      image_2_dimensions <- image_info(image_2)
      try(notify_info(paste0("comparing image ", x, " with image ", z, " of ", (number_of_splits-1)^2, " images\n"), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
      comparisons <- list()
      # direct comparison
      comparisons[[1]] <- cbind.data.frame(image_1 = x, image_2 = z, change = "without modification", pairwise_comparison(image_1, image_2, include_dimension_insensitive_measures = TRUE, extra_parameters = extra_parameters))
      # rotations
      comparisons[2:4] <- lapply(1:3, function(rotation){
        degrees_rotation <- rotation * 90
        rotated_image <- image_rotate(image_2, degrees_rotation)
        output <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0(degrees_rotation, " degrees rotation"), pairwise_comparison(image_1, rotated_image, extra_parameters = extra_parameters))
        return(output)
      })
      # mirror image
      horizontal_mirror_image_2 <- image_flip(image_2)
      comparisons[[5]] <- cbind.data.frame(image_1 = x, image_2 = z, change = "horizontal mirror image", pairwise_comparison(image_1, horizontal_mirror_image_2, extra_parameters = extra_parameters))
      comparisons[6:8] <- lapply(1:3, function(rotation){
        degrees_rotation <- rotation * 90
        rotated_image <- image_rotate(horizontal_mirror_image_2, degrees_rotation)
        output <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("horizontal mirror image; ", degrees_rotation, " degrees rotation"), pairwise_comparison(image_1, rotated_image, extra_parameters = extra_parameters))
        return(output)
      })
      vertical_mirror_image_2 <- image_flop(image_2)
      comparisons[[9]] <- cbind.data.frame(image_1 = x, image_2 = z, change = "vertical mirror image", pairwise_comparison(image_1, vertical_mirror_image_2, extra_parameters = extra_parameters))
      comparisons[10:12] <- lapply(1:3, function(rotation){
        degrees_rotation <- rotation * 90
        rotated_image <- image_rotate(vertical_mirror_image_2, degrees_rotation)
        output <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("vertical mirror image; ", degrees_rotation, " degrees rotation"), pairwise_comparison(image_1, rotated_image, extra_parameters = extra_parameters))
        return(output)
      })
      comparisons <- do.call(rbind.data.frame,comparisons)
      if (number_of_resizes > 0) {
        resized_comparisons <- lapply(1:length(resizes), function(resize) {
          scaling_factor <- resizes[resize]
          image_2_resized <- image_resize(image_2, geometry = geometry_size_pixels(width = scaling_factor * image_2_dimensions$width, preserve_aspect = TRUE))
          comparisons_resized <- list()
          # direct comparison
          comparisons_resized[[1]] <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("resize ", scaling_factor), pairwise_comparison(image_1, image_2_resized, extra_parameters = extra_parameters))
          # rotations
          comparisons_resized[2:4] <- lapply(1:3, function(rotation){
            degrees_rotation <- rotation * 90
            rotated_image <- image_rotate(image_2_resized, degrees_rotation)
            output <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("resize ", scaling_factor, "; ", degrees_rotation, " degrees rotation"), pairwise_comparison(image_1, rotated_image, extra_parameters = extra_parameters))
            return(output)
          })
          # mirror image
          horizontal_mirror_image_2 <- image_flip(image_2_resized)
          comparisons_resized[[5]] <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("resize ", scaling_factor, "; ", "horizontal mirror image"), pairwise_comparison(image_1, horizontal_mirror_image_2, extra_parameters = extra_parameters))
          comparisons_resized[6:8] <- lapply(1:3, function(rotation){
            degrees_rotation <- rotation * 90
            rotated_image <- image_rotate(horizontal_mirror_image_2, degrees_rotation)
            output <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("resize ", scaling_factor, " ", "horizontal mirror image; ", degrees_rotation, " degrees rotation"), pairwise_comparison(image_1, rotated_image, extra_parameters = extra_parameters))
            return(output)
          })
          vertical_mirror_image_2 <- image_flop(image_2_resized)
          comparisons_resized[[9]] <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("resize ", scaling_factor, "; ", "vertical mirror image"), pairwise_comparison(image_1, vertical_mirror_image_2, extra_parameters = extra_parameters))
          comparisons_resized[10:12] <- lapply(1:3, function(rotation){
            degrees_rotation <- rotation * 90
            rotated_image <- image_rotate(vertical_mirror_image_2, degrees_rotation)
            output <- cbind.data.frame(image_1 = x, image_2 = z, change = paste0("resize ", scaling_factor, "; ", "vertical mirror image; ", degrees_rotation, " degrees rotation"), pairwise_comparison(image_1, rotated_image, extra_parameters = extra_parameters))
            return(output)
          })
          comparisons_resized <- do.call(rbind.data.frame,comparisons_resized)
          return(comparisons_resized)
        })
        resized_comparisons <- do.call(rbind.data.frame, resized_comparisons)
      } else {
        resized_comparisons <- NULL
      }
      output <- rbind.data.frame(comparisons, resized_comparisons)
      return(output)
    })
    output <- do.call(rbind.data.frame, each_pairwise_comparison)
  })
  results <- do.call(rbind.data.frame, pairwise_comparisons)
  write.csv(results, paste0(storage_folder, "/comparison_results.csv"), row.names = FALSE, na = "")
  try(notify_info(paste0("Preparing for heatmaps...")),silent = TRUE)
  heatmaps <- results
  heatmaps <- heatmaps[! is.na(heatmaps$perceptual_hash),]
  heatmap_parameters <- colnames(heatmaps)[6:ncol(heatmaps)]
  options(repr.plot.width = 7, repr.plot.height = 7*image_dimensions$height/image_dimensions$width)
  heatmaps_all_images <- lapply(1:length(split_images),function(x) {
    try(notify_info(paste0("Creating heatmaps for ", x, " of ", (number_of_splits-1)^2, " images\n"), timeout = 30000, config_notify(showOnlyTheLastOne = TRUE)),silent = TRUE)
    heatmap_each_image <- heatmaps[((heatmaps$image_1 == x) | (heatmaps$image_2 == x)),]
    heatmap_each_image$row_number = rep(1:(number_of_splits-1), (number_of_splits-1))
    heatmap_each_image$column_number = unlist(lapply(1:(number_of_splits-1), function(z) {rep(z, (number_of_splits-1))}))
    write.csv(heatmap_each_image, paste0(storage_folder, "/data_for_heatmaps/section_", x, ".csv"),
              row.names = FALSE, na = "")
    each_parameter <- lapply(1:length(heatmap_parameters), function(z) {
      placeholder <- {ggplot() +
          geom_tile(
            data = heatmap_each_image, 
            aes(x = row_number, y = rev(column_number), fill = eval(parse(text = heatmap_parameters[z]))), color = alpha("white", 0.6),lwd = 0.1, linetype = 1) +
          scale_fill_gradient(low = alpha("darkblue", 0.8), high = alpha("white", 0.8)) +
          theme(axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                legend.position="none",
                panel.background = element_rect(fill='transparent', color = NA),
                plot.background = element_rect(fill='transparent', color = NA),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                plot.margin = margin(0,0,0,0),
                axis.ticks.length = unit(0, "pt")
          ) +
          geom_label(data = heatmap_each_image, aes(x = row_number, y = rev(column_number), label = round(heatmap_each_image[,heatmap_parameters[z]],2)), color = "maroon", size = 4, fill = "pink")
      }
      ggsave(paste0(tempdir(),"/placeholder.png"), width = image_dimensions$width,
             height = image_dimensions$height, units = "px")
      placeholder <- knitr::plot_crop(paste0(tempdir(),"/placeholder.png"))
      merged_image <-  ggdraw() + draw_image(rv$file_upload_image$datapath) + 
        draw_image(paste0(tempdir(),"/placeholder.png")) 
      ggsave2(filename = paste0(storage_folder,'/heatmaps/section_', x, "_", heatmap_parameters[z],".png"),
              width = 7, height = 7*image_dimensions$height/image_dimensions$width, units = "in")
    })
    if (x == 1) {
      # Create a template
      placeholder <- {ggplot() +
          geom_tile(
            data = heatmap_each_image, 
            aes(x = row_number, y = rev(column_number), fill = 1:nrow(heatmap_each_image)), color = alpha("white", 0.6),lwd = 0.1, linetype = 1) +
          scale_fill_gradient(low = alpha("darkblue", 0.8), high = alpha("white", 0.8)) +
          theme(axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                legend.position="none",
                panel.background = element_rect(fill='transparent', color = NA),
                plot.background = element_rect(fill='transparent', color = NA),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                plot.margin = margin(0,0,0,0),
                axis.ticks.length = unit(0, "pt")
          ) +
          geom_label(data = heatmap_each_image, aes(x = row_number, y = rev(column_number), label = paste0("section_", 1:nrow(heatmap_each_image))), color = "maroon", size = 4, fill = "pink")
      }
      ggsave(paste0(tempdir(),"/placeholder.png"), width = image_dimensions$width,
             height = image_dimensions$height, units = "px")
      placeholder <- knitr::plot_crop(paste0(tempdir(),"/placeholder.png"))
      merged_image <-  ggdraw() + draw_image(rv$file_upload_image$datapath) + 
        draw_image(paste0(tempdir(),"/placeholder.png")) 
      ggsave2(filename = paste0(storage_folder,"/section_legend.png"),
              width = 7, height = 7*image_dimensions$height/image_dimensions$width, units = "in")
      
    }
  })
  options(repr.plot.width = 7, repr.plot.height = 7*image_dimensions$height/image_dimensions$width)
  if (file.exists(paste0(tempdir(), "/comparison_results.zip"))) {unlink(paste0(tempdir(), "/comparison_results.zip"), recursive = TRUE)}
  zip::zip(paste0(tempdir(), "/comparison_results.zip"), 
           list.files(storage_folder, full.names = TRUE), 
           mode = "cherry-pick")
  # To avoid "No visible binding for global variable"
  row_number <- column_number <- NULL
  return(results)
}
# Load packages ####
library("shiny")
library("shinyjs")
library("shinybusy")
library("magick")
library("ggplot2")
library("cowplot")
library("knitr")
library("zip")
# Interface creation ####
# Some functions and list to create the user interface
{
  ui_short_forms <- {cbind.data.frame(short_name = c("text", "numeric", "slider", "select", "checkbox", "radio", "file", "action", "date", "html", "plot", "image", "download"),
                                      long_name = c("textInput", "numericInput", "sliderInput", "selectInput", "checkboxGroupInput",
                                                    "radioButtons", "fileInput", "actionButton", "dateInput", "HTML", "plotOutput", "plotOutput", "downloadButton"),
                                      render_name = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "renderPlot", "renderImage", "downloadHandler")
  )}
  ui_short_forms$update_name <- paste0("update", str_to_title(ui_short_forms$short_name), str_remove(ui_short_forms$long_name, ui_short_forms$short_name))
  create_UI <- function(ui_text, ui_short_forms) {
    ui_text$order <- 1:nrow(ui_text)
    ui_text$additional_options <- NA
    ui_text$submit_text <- NA
    ui_text$rv_text <- paste0(ui_text$variables, " = NA")
    ui_text$additional_options[! is.na(ui_text$additional_parameters)] <- paste0(
      ", ", paste0(str_replace_all(ui_text$additional_parameters[! is.na(ui_text$additional_parameters)], ";", ",")))
    ui_text$additional_options[is.na(ui_text$additional_parameters)] <- ''
    # Input
    ui_text_input <- ui_text[ui_text$input_type %in% c("text", "numeric", "slider", "select", "checkbox", "radio", "file", "action", "date"),]
    ui_text_input$submit_text[ui_text_input$input_type == "text"] <-
      paste0("(input$", ui_text_input$variables[ui_text_input$input_type == "text"], " != '')")
    ui_text_input$submit_text[ui_text_input$input_type %in% c("numeric", "slider")] <-
      paste0("(! is.na(input$", ui_text_input$variables[ui_text_input$input_type  %in% c("numeric", "slider")], "))")
    ui_text_input$submit_text[ui_text_input$input_type %in% c("select", "checkbox", "radio")] <-
      paste0("(! TRUE %in% (input$", ui_text_input$variables[ui_text_input$input_type  %in% c("select", "checkbox", "radio")], " == ''))")
    ui_text_input$submit_text[ui_text_input$input_type %in% c("file", "action")] <-
      paste0("(! is.null(input$", ui_text_input$variables[ui_text_input$input_type  %in% c("file", "action")], "))")
    ui_text_input$submit_text[ui_text_input$input_type == "action"] <-
      paste0("(input$", ui_text_input$variables[ui_text_input$input_type  == "action"], " > 0)")
    ui_text_input$ui <- paste0("output$", ui_text_input$variables, "_UI <- renderUI(",
                               ui_short_forms$long_name[match(ui_text_input$input_type, ui_short_forms$short_name)], "('", ui_text_input$variables, "', ",
                               "'", ui_text_input$description, "'", ui_text_input$additional_options,"))")
    submit_conditions <- ui_text_input$submit_text[ui_text_input$mandatory == "yes"]
    capture_values <- paste0("if (length(input$",ui_text_input$variables,") > 0) {rv$", ui_text_input$variables, " <- input$", ui_text_input$variables, "}")
    # Output
    ui_text_output <- ui_text[! (ui_text$input_type %in% c("text", "numeric", "slider", "select", "checkbox", "radio", "file", "action", "date")),]
    if (nrow(ui_text_output) > 0) {
      ui_text_output$ui <- paste0("output$", ui_text_output$variables, "_UI <- renderUI(",
                                  ui_short_forms$long_name[match(ui_text_output$input_type, ui_short_forms$short_name)],
                                  unlist(lapply(1:nrow(ui_text_output), function(x) {
                                    if (ui_text_output$input_type[x] == "html") {
                                      paste0("(rv$", ui_text_output$variables[x],"))")
                                    } else if (ui_text_output$input_type[x] %in% c("plot", "image")) {
                                      paste0("('", ui_text_output$variables[x], "'", ui_text_output$additional_options[x],"))")
                                    } else if (ui_text_output$input_type[x] == "download"){
                                      paste0("('", ui_text_output$variables[x], "','", ui_text_output$description[x],"'))")
                                    }
                                  }))
      )
      ui_text_output$rv_text <- paste0(ui_text_output$variables, " = NULL")
      ui_text_output$rv_text[((ui_text_output$input_type == "html") & (! is.na(ui_text_output$description)))] <-
        paste0(ui_text_output$variables[((ui_text_output$input_type == "html") & (! is.na(ui_text_output$description)))], " = '", ui_text_output$description[((ui_text_output$input_type == "html") & (is.na(ui_text_output$description)))], "'")
      ui_text_extra_output <- ui_text_output[ui_text_output$input_type %in% c("plot", "image", "download"),]
      if (nrow(ui_text_extra_output) > 0) {
        ui_text_extra_output$order <- ui_text_extra_output$order + 0.5
        ui_text_extra_output$ui <- paste0(
          "if (! is.null(", paste0("rv$", ui_text_extra_output$variables), ")) {",
          "output$", ui_text_extra_output$variables, " <- ",
          ui_short_forms$render_name[
            match(ui_text_extra_output$input_type, ui_short_forms$short_name)],
          "(",
          unlist(lapply(1:nrow(ui_text_extra_output), function(x) {
            if (ui_text_extra_output$input_type[x] == "html") {
              output <- NA
            } else if (ui_text_extra_output$input_type[x] == "plot") {
              output <- paste0("rv$", ui_text_extra_output$variables[x])
              split_information <- unlist(str_split(ui_text_extra_output$additional_parameters_2[x], "; " ))
              if (length(split_information) > 0) {
                output <- paste0(output, ", ", paste0(split_information[2:length(split_information)], collapse = ", "))
              }
            } else if (ui_text_extra_output$input_type[x] == "image") {
              output <- paste0("list(src = rv$", ui_text_extra_output$variables[x])
              split_information <- unlist(str_split(ui_text_extra_output$additional_parameters_2[x], "; " ))
              if (length(split_information) > 0) {
                output <- paste0(output, ", ", paste0(split_information[2:length(split_information)], collapse = ", "))
              }
              output <- paste0(output, "), deleteFile = FALSE")
            } else if (ui_text_extra_output$input_type[x] == "download") {
              output <- paste0(
                "filename = '",
                paste0(str_remove(ui_text_extra_output$variables[x], "download_"), ".",
                       ui_text_extra_output$additional_parameters_2[x]),
                "', content = function(file) {file.copy(rv$",ui_text_extra_output$variables[x],
                ", file)}"
              )
            }
            return(output)
          })),
          ")}"
        )
        ui_text_output <- rbind.data.frame(ui_text_output, ui_text_extra_output)
      }
    }
    ui_text_merged <- rbind.data.frame(ui_text_input, ui_text_output)
    ui_text_merged <- ui_text_merged[order(ui_text_merged$order),]
    output <- list(ui = ui_text_merged$ui,
                   submit_conditions = submit_conditions,
                   capture_values = capture_values,
                   rv_text = unique(ui_text_merged$rv_text)
    )
  }
}
# UI text and create UI
{
  ui_text <- {cbind.data.frame(
    variables = c(
      "restart", "html_message", 
      "within_between_images", "submit_within_between_images", "reset_within_between_images", 
      "file_upload_image_1", "file_upload_image_2", "stretch_images", "number_of_resizes_between", "extra_parameters_between",
      "submit_between", "reset_between", "download_results_between",
      "file_upload_image", "number_of_splits", "number_of_resizes_within", "extra_parameters_within",
      "submit_within", "reset_within", "download_results_within"
    ),
    description = c("Perform one more comparison",
                    "Message", 
                    "Select whether you want to compare between two images or compare sections within a single image", 
                    "Submit your choice", 
                    "Reset your choice", 
                    "Upload the first image", 
                    "Upload the second image", 
                    "Do you want the images to be stretched to the maximum width and height of the two images being compared", 
                    "How many image reductions/dimunitions do you want to perform?", 
                    "Select any additional parameters that must be reported", 
                    "Submit your choices", 
                    "Reset your choices", 
                    "Download the comparison results",
                    "Upload the image", 
                    "How many sections do you want the width/height to be divided?", 
                    "How many image reductions/dimunitions do you want to perform?", 
                    "Select any additional parameters that must be reported", 
                    "Submit your choices", 
                    "Reset your choices", 
                    "Download the comparison results"
    ),
    mandatory = c("no", "no", 
                  "yes", "no", "no", 
                  "no", "no", "no", "no", "no", 
                  "no", "no", "no", 
                  "no", "no", "no", "no", 
                  "no", "no", "no"
    ),
    input_type = c("action", "html", 
                   "select", "action", "action", 
                   "file", "file", "select", "slider", "checkbox",
                   "action", "action", "download",
                   "file", "slider", "slider", "checkbox",
                   "action", "action","download"
    ),
    additional_parameters = c(NA, NA,
                              "choices = c('', 'Between images', 'Sections within an image'); width = '90%'", 
                              NA, NA,
                              "accept = 'image/*'; width = '90%'",
                              "accept = 'image/*'; width = '90%'",
                              "choices = c('', 'Yes', 'No'); width = '90%'", 
                              "min = 0, max = 10, value = 1; width = '90%'",
                              "choices = c('fuzz', 'mean_absolute_error_normalized', 'mean_error_per_pixel',
                                            'normalized_cross_correlation', 'peak_absolute_error',
                                            'peak_signal_to_noise_ratio', 'root_mean_squared_error'); inline = TRUE",
                              NA, NA, NA,
                              "accept = 'image/*'; width = '90%'",
                              "min = 2, max = 20, value = 5; width = '90%'",
                              "min = 0, max = 10, value = 1; width = '90%'",
                              "choices = c('fuzz', 'mean_absolute_error_normalized', 'mean_error_per_pixel',
                                            'normalized_cross_correlation', 'peak_absolute_error',
                                            'peak_signal_to_noise_ratio', 'root_mean_squared_error'); inline = TRUE",
                              NA, NA, NA),
    additional_parameters_2 = c(NA, NA, 
                                NA, NA, NA, 
                                NA, NA, NA, NA, NA, NA, NA, "csv", 
                                NA, NA, NA, NA, NA, NA,"zip")
  )}
  main_panel_display_fields <- paste0("uiOutput(outputId = '", ui_text$variables, "_UI')")
  main_panel_display <- paste0("fluidRow(", paste0("eval(parse(text = main_panel_display_fields[",1:length(main_panel_display_fields),"]))", collapse = ",\n"), ")")
  start_submit_text <- create_UI(ui_text, ui_short_forms)
  start_text <- start_submit_text[[1]]
  check_text <- paste0("(", paste0(start_submit_text[[2]], collapse = " & "), ")")
  capture_text <- paste0(start_submit_text[[3]], collapse = "\n")
  required_reactive_values <- paste0("rv <- reactiveValues(",paste0(start_submit_text[[4]], collapse = ", "), ")")
  all_ui_null <- paste0("output$", ui_text$variables, "_UI <- NULL")
  all_rv_reset <- paste0("rv$", start_submit_text[[4]])
}
# Instructions
Instructions <- {paste0(
  "<h2>Instructions</h2>",
  "<h3>General comments</h3>",
  "<ol start = 1>",
  "<li>This program has been created for comparing between images or sections of images.</li>",
  "<li>This program is undergoing testing. Therefore, the user must use this software at their own risk.</li>",
  "<li>This program must not be used for any unlawful purposes.</li>",
  "<li>This calculates the differences between images (or sections within images) using different measures.</li>",
  "<li>As default, absolute error and perceptual hash are reported. Additional measures can be selected as necessary.</li>",
  "<li>At present, there are no cut-off points of interpretation. Any values provided below are based on limited observations rather than systematic observations.</li>",
  "<li>If all the measures point to higher similarity, it is likely that the images are similar and the converse is also true.</li>",
  "<li>The brief meaning of different measures are provided below. Please see the references section at the end to learn more details about these measures.</li>",
  "<ul>",
  "<li><em>Absolute error:</em> This calculates pixel-by-pixel difference. A lower value of absolute error indicates greater similarity between images. ",
  "The absolute error is also reported as percentage similarity with higher values indicating gerater similarity between images.",
  "The main comparison is comparison between the images/sections of images without modifications; comparisons are also performed after modifying the images such as rotations, mirror images, resized images.</li>",
  "<li><em>Perceptual hash algorithm:</em> This calculates perceptual hash, the broad similarity rather than pixel-by-pixel difference. A lower value of difference in perceptual hash indicates greater similarity between images.",
  "This value is only calculated for the comparison between image/sections of images without modifications.</li>", 
  "<li><em>Fuzz:</em> The mean colour distance between between two corresponding points of the images.</li>",
  "<li><em>Mean absolute error (normalized):</em> This calculates the average of the difference in each of red, blue, green channels between two correspondings points of the images.</li>",
  "<li><em>Mean error per pixel:</em> This calculates the normalized mean error between two corresponding points of the images.</li>",
  "<li><em>Normalized cross correlation:</em> A measure of the strength of association of colour values between two images.</li>",
  "<li><em>Peak absolute error:</em> This calculates the peak absolute error between the corresponding points of two images.</li>",
  "<li><em>Peak Signal to noise ratio (PSNR):</em> A measure of distortion between two images.</li>",
  "<li><em>Root mean error squared:</em> Square root of the average of the channel error squared.</li>",
  "</ul>",
  "<li>For all the measures, the higher values indicate greater dissimilarity. There is no uniform threshold for interpretation. We recommend mean absolute error < 25% and/or perceptual hash < 25 for further scrutiny.",
  "<li>We have not reviewed the information on other parameters to suggest any threshold.",
  "</ol>",
  "<h3>Initial screen</h3>",
  "<p>In the initial screen, choose whether you want to compare between two images or between sections of the same image and click on submit.</p>",
  "<h3>Compare between two images</h3>",
  "<ol start = 1>",
  "<li>Only image formats (any) are accepted as uploads for the two images.</li>",
  "<li>Select whether the images must be stretched to maximum size before comparison. ",
  "If 'Yes' is chosen, the images are to be stretched to the maximum width and height among the images. ",
  "This means the images may be stretched. If 'No' is chosen, only the sections of the images corresponding to the minimum width and height among the images are compared.</li>",
  "<li>Select the number of resizes. This means that second image is shrunk by a certain percentage before the images are compared. ",
  "The percentage by which the image is shrunk by is determined by the number of resizes you choose. For example, if you choose 3 as the number of resizes, ",
  "the images are shrunk by 33% and 67%. If you chose 4 as the number of resizes, the images are shrunk by 25%, 50%, and 75%, and so on.</li>",
  "<li>As default, absolute error and perceptual hash are reported. Additional measures can be selected as necessary.</li>",
  "<li>After the selections, submit your choices. You can then download the results available as csv file.</li>",
  "</ol>",
  "<h3>Compare sections within the same image</h3>",
  "<ol start = 1>",
  "<li>Only image formats (any) are accepted as upload for the image.</li>",
  "<li>Select the number of splits. This divides the image into squares or rectangles determined by the number of splits. ",
  "For example,if you choose 5 splits, the image is divided into 5x5 = 25 parts and each of the 25 parts are compared with each other. ",
  "If you chose 6 splits, the image is split into 6x6 = 36 parts and each of the 36 parts are compared with each other.</li>",
  "<li>Select the number of resizes. Please see section on comparing between images to understand what this means. ",
  "<li>As default, absolute error and perceptual hash are reported. Additional measures can be selected as necessary.</li>",
  "<li>As in the previous section, after the selections, submit your choices. You can then download the results available as zip file.</li>",
  "<li>The zipped file contains a csv file which provides the results in tabular format. The results are also available as heatmaps. 
  The split images are available for reference, so it is clear which aspects are being compared.</li>",
  "</ol>",
  "<h3>References</h3>",
  "<ol start = 1>",
  "<li><a href = 'https://imagemagick.org/script/command-line-options.php#metric' target = 'blank'>General reference for all errors</a>.</li>",
  "<li><a href = 'https://www.sciencedirect.com/topics/engineering/absolute-error' target = 'blank'>A set of references for absolute errors</a>.</li>",
  "<li><a href = 'https://www.ofcom.org.uk/online-safety/safety-technology/overview-of-perceptual-hashing-technology' target = 'blank'>Perceptual hash</a>.</li>",
  "<li><a href = 'https://pmc.ncbi.nlm.nih.gov/articles/PMC6147431/' target = 'blank'>Normalized cross correlation</a>.</li>",
  "</ol>"
)}
# User interface ####
ui <- {fluidPage(
  # Some parameters for web page
  shinyjs::useShinyjs(),
  add_busy_spinner(spin = "fading-circle"),
  {tags$head(
    tags$style(HTML('
        body {background-color: aliceblue;color: black;}
        p {text-align: left; margin-top: 0px; margin-bottom: 0px;line-height: 1.6;font-family:Sans-Serif}
        h1 {text-align: center;font-family:arial bold;}
        h2 {text-align: left; margin-top: 6px;font-family:arial bold; color: maroon; font-weight: bold; font-size: 28px}
        h3 {text-align: left; margin-top: 6px;font-family:arial bold; color: black; font-weight: bold; font-size: 20px}
        h4 {text-align: left; margin-top: 6px;font-family:arial bold; color: green; background-color: yellow; font-weight: bold; font-size: 28px}
        h5 {text-align: left; margin-top: 6px;font-family:arial bold; color: maroon; background-color: yellow; font-weight: bold; font-size: 28px}
#submit_within_between_images{background-color:darkgreen; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#submit_between{background-color:darkgreen; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#submit_within{background-color:darkgreen; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#reset_within_between_images{background-color:maroon; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#reset_between{background-color:maroon; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#reset_within{background-color:maroon; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#restart{background-color:purple; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#download_results_between{background-color:darkblue; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#download_results_within{background-color:darkblue; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
')))
  },
  # Title panel
  {fluidRow(headerPanel(div(
    column(width = 12, HTML('<h1 style = "color:white; background-color:darkblue; font-family:arial bold;"><b>Compare images</b></h1>')),
  )))},
  # Side bar
  sidebarLayout(
    # Side panel for instructions ####
    sidebarPanel(HTML(Instructions)),
    # Main panel for input and output ####
    mainPanel(
      # Name ####
      fluidRow(
        column(width = 12, HTML('<h2 style = "color:#254636; background-color: aliceblue; text-align: left; font-family:arial bold;"><b>Evidence-Based Healthcare: Best Information for Best Practice</b></h2>')),
        column(width = 12, HTML('<h3 style = "color:#254636; background-color: aliceblue; text-align: left; font-family:arial bold;"><b>Developed by: </b><a href="https://profiles.ucl.ac.uk/11524-kurinchi-gurusamy" target="_blank"><i>Professor Kurinchi Gurusamy, University College London</i></a></h3>')),
        column(width = 12, HTML('<h2 style = "color:#254636; background-color: aliceblue; text-align: left; font-family:arial bold;"><b>EQUity through biomedicAL research (EQUAL) group</b></h2>')),
      ),
      # Main information ####
      eval(parse(text = main_panel_display)),
    ),
  ),
)
}
# Server ####
server <- function(input, output, session) {
  eval(parse(text = required_reactive_values))
  eval(parse(text = start_text[2:5]))
  observeEvent(input$restart, {
    if (! is.null(input$restart)) {
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      unlink(list.files(tempdir(), full.names = TRUE))
      eval(parse(text = start_text[2:5]))
    }
  })
  observeEvent(input$submit_within_between_images, {
    if (! is.null(input$submit_within_between_images)) {
      rv$html_message = ''
      rv$submit_within_between_images <- ((! TRUE %in% (input$submit_within_between_images == '')))
      if (rv$submit_within_between_images) {
        eval(parse(text = capture_text))
        eval(parse(text = all_ui_null))
        if (rv$within_between_images == "Between images") {
          rv$html_message = ''
          eval(parse(text = start_text[c(1,2,6:12)]))
        } else {
          rv$html_message = ''
          eval(parse(text = start_text[c(1,2,15:20)]))
        }
      } else {
        rv$html_message = "<h5>Choosing whether you want to compare between images or sections within an image is mandatory. Please enter this information before submission.</h5>"
      }
    }
  })
  observeEvent(input$reset_within_between_images, {
    if (! is.null(input$reset_within_between_images)) {
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[2:5]))
    }
  })
  observeEvent(input$submit_between, {
    if (! is.null(input$submit_between)) {
      eval(parse(text = paste0('disable("', ui_text$variables[1:12],'")')))
      eval(parse(text = capture_text))
      results <- compare_similarities_two_images(rv)
      rv$html_message = ''
      rv$download_results_between <- paste0(tempdir(), "/comparison_results.csv")
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[c(1,2,13:14)]))
    }
  })
  observeEvent(input$reset_between, {
    if (! is.null(input$reset_between)) {
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[c(1,2,6:12)]))
    }
  })
  observeEvent(input$submit_within, {
    if (! is.null(input$submit_within)) {
      eval(parse(text = capture_text))
      eval(parse(text = paste0('disable("', ui_text$variables[c(1,2, 15:20)],'")')))
      results <- compare_similarities_same_image(rv)
      rv$html_message = ''
      rv$download_results_within <- paste0(tempdir(), "/comparison_results.zip")
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[c(1,2,21:22)]))
    }
  })
  observeEvent(input$reset_within, {
    if (! is.null(input$reset_within)) {
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[c(1,2,15:20)]))
    }
  })
}
# Run the application ####
shinyApp(ui = ui, server = server)
