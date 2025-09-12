# Options ####
# Version 0.1 Pre-release version
options(scipen=9999)
# Expand memory and improve options
options(shiny.maxRequestSize = 30*1024^2)
# Load packages ####
library(shiny)
library(shinyjs)
library(shinybusy)
library(stringr)
library(EQUALCompareImages)
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
