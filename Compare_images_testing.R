# Options ####
# Version 0.1 Pre-release version
options(scipen=9999)
# Expand memory and improve options
options(shiny.maxRequestSize = 30*1024^2)
# Load packages ####
library(EQUALCompareImages)
library(stringr)
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