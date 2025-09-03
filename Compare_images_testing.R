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
library("stringr")
library("magick")
library("ggplot2")
library("cowplot")
library("knitr")
library("zip")
# Run the tests ####
run_tests <- function(source_folder = "cleveland_art_museum_images", 
                                 number_of_images = 30,
                                 number_of_runs = 3) {
  # Create a list to simulate the uploads of shiny app ####
  rv <- {list(
    file_upload_image_1 = cbind.data.frame(datapath = ""), 
    file_upload_image_2 = cbind.data.frame(datapath = ""), 
    stretch_images = "No", 
    number_of_resizes_between = 1,
    extra_parameters_between = c("fuzz", "peak_signal_to_noise_ratio"),
    file_upload_image = cbind.data.frame(datapath = ""), 
    number_of_splits = 3, 
    number_of_resizes_within = 1,
    extra_parameters_within = c("fuzz")
  )}
  # Additional functions for running the tests ####
  sample_images <- function(source_folder, number_of_images = 30) {
    files_in_folder <- list.files(source_folder, full.names = TRUE)
    sample_of_images <- sample(files_in_folder, min(length(files_in_folder), number_of_images), replace = FALSE)
    if (dir.exists(paste0(tempdir(), "/sample_images_folder"))) {unlink(paste0(tempdir(), "/sample_images_folder"), recursive = TRUE)}
    dir.create(paste0(tempdir(), "/sample_images_folder"))
    placeholder <- lapply(1:length(sample_of_images), function(x) {
      file.copy(from = sample_of_images[x], to = paste0(tempdir(), "/sample_images_folder/",
                                                        basename(sample_of_images[x])))
    })
    output <- paste0(tempdir(), "/sample_images_folder")
  }
  image_folder <- sample_images(source_folder = source_folder, number_of_images = 3)
  compare_between <- function(image_folder, rv) {
    images_path <- list.files(image_folder, full.names = TRUE)
    cat(paste0("\nComparing between two images (", length(images_path), " images)..."))
    comparison_results <- lapply(1:length(images_path), function(x) {
      output <- lapply(x:length(images_path), function(y) {
        cat(paste0("\nComparing image ", x, " with image ", y, "..."))
        rv$file_upload_image_1$datapath <- images_path[x]
        rv$file_upload_image_2$datapath <- images_path[y]
        cbind.data.frame(
          image_1 = basename(images_path[x]),
          image_1 = basename(images_path[y]),
          compare_similarities_two_images(rv)
        )
      })  
      output <- do.call(rbind.data.frame, output)
    })
    comparison_results <- do.call(rbind.data.frame, comparison_results)
  }
  compare_within <- function(image_folder, rv) {
    images_path <- list.files(image_folder, full.names = TRUE)
    cat(paste0("\nComparing within images (", length(images_path), " images)..."))
    comparison_results <- lapply(1:length(images_path), function(x) {
        cat(paste0("\nImage ", x, "..."))
        rv$file_upload_image$datapath <- images_path[x]
        cbind.data.frame(
          image = basename(images_path[x]),
          compare_similarities_same_image(rv)  
        )
    })
    comparison_results <- do.call(rbind.data.frame, comparison_results)
  }
  
  # Run the tests ####
  seeds <- sample(1:1000000, number_of_runs, replace = FALSE)
  lapply(1:number_of_runs, function (run_number) {
    cat(paste0("\nRun ", run_number, "..."))
    set.seed(seeds[run_number])
    cat(paste0("\nRunning tests on comparing between two images..."))
    results_between <- compare_between(image_folder, rv)
    results_within <- compare_within(image_folder, rv)
    # Export all the test results to results folder ####
    cat(paste0("\nExporting detailed study results..."))
    if (dir.exists(paste0("test_results_run_", run_number)) == TRUE) {unlink(paste0("test_results_run_", run_number), recursive = TRUE)}
    dir.create(paste0("test_results_run_", run_number))
    results_files <- c("results_between", "results_within")
    placeholder <- lapply(results_files, function(x) {
      write.csv(eval(parse(text = x)), paste0("test_results_run_", run_number, "/", x, ".csv"), row.names = FALSE, na = "")
    })
    # Summary of results of no alteration ####
    summary_results_no_alteration_between <- summary(results_between[results_between$change == "No alteration to image",6:ncol(results_between)])
    summary_results_no_alteration_within <- summary(results_within[results_within$change == "without modification",6:ncol(results_within)])
    summary_files <- c("summary_results_no_alteration_between", "summary_results_no_alteration_within")
    placeholder <- lapply(summary_files, function(x) {
      write.csv(eval(parse(text = x)), paste0("test_results_run_", run_number, "/", x, ".csv"), row.names = FALSE, na = "")
    })
    output <- list(summary_results_no_alteration_between = summary_results_no_alteration_between,
                   summary_results_no_alteration_within = summary_results_no_alteration_within)
    return(output)
  })
}
results <- run_tests()