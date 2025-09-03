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
