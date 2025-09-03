\name{compare_similarities_two_images}
\alias{compare_similarities_two_images}
\title{
Compare similarities between two images
}
\description{
Uses \pkg{magick} to compare the similarities between the different sections of 
the images.It computes the absolute error and the perceptual hash of each 
pairwise comparison. Measures such as fuzz, root mean squared error
are also available as optional measures.
}
\usage{
compare_similarities_two_images(rv)
}
\arguments{
\item{rv}{a list provided by user input containing the paths to the two images 
being compared, whether the images must be streteched to the maximum width and 
height of the two images, the number of sections into which the width and height 
must be split, and the number of resizes.}
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
\code{\link[=compare_similarities_same_image]{compare_similarities_same_image()}}
}
\concept{EQUALCompareImages}
\examples{
# Create a plot and save this as file
filename_1 <- tempfile(fileext = ".png")
png(filename_1, width = 300, height = 450, units = "px")
plot.new()
hist(rnorm(100))
recordPlot()
dev.off()
# One more plot for comparison
filename_2 <- tempfile(fileext = ".png")
png(filename_2, width = 300, height = 450, units = "px")
plot.new()
boxplot(rnorm(100))
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
rv$file_upload_image_1$datapath <- filename_1
rv$file_upload_image_2$datapath <- filename_2

# Perform the function
library(magick)
results <- compare_similarities_two_images(rv)
}
