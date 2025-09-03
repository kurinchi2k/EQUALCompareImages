\name{pairwise_comparison}
\alias{pairwise_comparison}
\title{
Compare a pair of images
}
\description{
This is the main function of this package and supports both the
\code{\link[=compare_similarities_two_images]{compare_similarities_two_images()}} 
function and the
\code{\link[=compare_similarities_same_image]{compare_similarities_same_image()}} 
function.
}
\usage{
pairwise_comparison(image_1, image_2, 
include_dimension_insensitive_measures = FALSE, remove_dominant_colour = FALSE,
extra_parameters = "")
}
\arguments{
\item{image_1}{The first image of the pair of images for which the comparison
must be made.}
\item{image_2}{The second image of the pair of images for which the comparison 
must be made.}
\item{include_dimension_insensitive_measures}{Logical (default value is FALSE) 
indicating whether dimension insensitive measures such as perceptual hash must 
be calculated. These are necessary only for the comparison of images without any
modification as these measures are expected to find similarities between the 
images even if they are rotated or resized.}
\item{remove_dominant_colour}{Logical (default value is TRUE) 
indicating whether the dominant colour must be removed. When indicated as TRUE, 
this is performed only for the comparison of images without any modification 
as some of the algorithms to find similarities between the images even if they 
are rotated or resized rely on the background colour.}
\item{extra_parameters}{By default, only absolute error and perceptual hash are 
calculated and reported. Measures such as fuzz, root mean squared 
error can also be calculated optionally.}
}
\value{
a data frame containing the pixels_compared, absolute_error, perceptual_hash, and
any optional measures.
}
\references{
\url{https://sites.google.com/view/equal-group/home}}
\author{Kurinchi Gurusamy}
\note{This is part of a suite of functions required to compare images.}
\seealso{
\code{\link[=compare_similarities_two_images]{compare_similarities_two_images()}}
\code{\link[=compare_similarities_same_image]{compare_similarities_same_image()}}
\code{\link[=find_dominant_colour]{find_dominant_colour()}}
}
\concept{EQUALCompareImages}
\examples{
# Create a plot and save this as file
filename_1 <- tempfile(fileext = ".png")
png(filename_1)
plot.new()
hist(rnorm(100))
recordPlot()
dev.off()
# One more plot for comparison
filename_2 <- tempfile(fileext = ".png")
png(filename_2)
plot.new()
boxplot(rnorm(100))
recordPlot()
dev.off()

# Read images
library(magick)
image_1 <- image_read(filename_1)
image_2 <- image_read(filename_2)

# Perform the function
results <- pairwise_comparison(image_1 = image_1, image_2 = image_2, 
                               include_dimension_insensitive_measures = TRUE)
}
