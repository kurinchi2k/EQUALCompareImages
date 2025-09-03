\name{stretch_images}
\alias{stretch_images}
\title{
Stretch images to the maximum width and height of two images
}
\description{
This stretches the images to the maximum width and height of two images.
}
\usage{
stretch_images(image_1, image_2)
}
\arguments{
\item{image_1}{The first image of the pair of images for which the comparison
must be made.}
\item{image_2}{The second image of the pair of images for which the comparison 
must be made.}
}
\value{
\item{image_1}{The first image stretched to the maximum width and height of the 
two images being compared}
\item{image_2}{The second image stretched to the maximum width and height of the 
two images being compared}
}
\references{
\url{https://sites.google.com/view/equal-group/home}}
\author{Kurinchi Gurusamy}
\note{This is part of a suite of functions required to compare images.}
\seealso{
\code{\link[=compare_similarities_two_images]{compare_similarities_two_images()}}
}
\concept{EQUALCompareImages}
\examples{
# Create a plot and save this as file - width less than height
filename_1 <- tempfile(fileext = ".png")
png(filename_1, width = 300, height = 450, units = "px")
plot.new()
hist(rnorm(100))
recordPlot()
dev.off()
# One more plot for comparison - width more than height
filename_2 <- tempfile(fileext = ".png")
png(filename_2, width = 450, height = 300, units = "px")
plot.new()
boxplot(rnorm(100))
recordPlot()
dev.off()

# Read images
library(magick)
image_1 <- image_read(filename_1)
image_2 <- image_read(filename_2)

# Perform the function
results <- stretch_images(image_1 = image_1, image_2 = image_2)
# Plots are stretched to the maximum height and maximum width among the two images
plot(results[[1]])
plot(results[[2]])
}
