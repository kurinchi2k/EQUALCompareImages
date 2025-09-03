\name{compare_similarities_same_image}
\alias{compare_similarities_same_image}
\title{
Compare similarities within sections of the same image
}
\description{
Divides the images into a number of sections (determined by the user) and uses
\pkg{magick} to compare the similarities between the different sections of the
images.It computes the absolute error and the perceptual hash of each 
pairwise comparison. Measures such as fuzz, root mean squared error
are also available as optional measures.}
\usage{
compare_similarities_same_image(rv)
}
\arguments{
\item{rv}{a list provided by user input containing the path to the image, the 
number of sections into which the width and height must be split, and the number 
of resizes.}
}
\value{
A csv file containing the results
}
\references{
\url{https://sites.google.com/view/equal-group/home}}
\author{Kurinchi Gurusamy}
\note{This is part of a suite of functions required to compare images.}
\seealso{
\code{\link[=pairwise_comparison]{pairwise_comparison()}}
\code{\link[=compare_similarities_two_images]{compare_similarities_two_images()}}
}
\concept{EQUALCompareImages}
\examples{
# Create a plot
filename <- tempfile(fileext = ".png")
png(filename, width = 300, height = 450, units = "px")
plot.new()
hist(rnorm(100))
recordPlot()
dev.off()

# Create a list to simulate the uploads of shiny app
rv <- {list(
  file_upload_image_1 = cbind.data.frame(datapath = ""), 
  file_upload_image_2 = cbind.data.frame(datapath = ""), 
  stretch_images = "No", 
  number_of_resizes_between = 1,
  extra_parameters_between = c("fuzz"),
  file_upload_image = cbind.data.frame(datapath = ""), 
  number_of_splits = 3, 
  number_of_resizes_within = 1,
  extra_parameters_within = c("fuzz")
)}
# Allocate the file paths
rv$file_upload_image$datapath <- filename
# Perform the function
library(magick)
library("ggplot2")
library("cowplot")
library("knitr")
library("zip")

# Not run for CRAN checks because of time it takes to run
if (interactive()) {
results <- compare_similarities_same_image(rv)
}
}