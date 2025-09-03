\name{find_dominant_colour}
\alias{find_dominant_colour}
\title{
Find dominant colour in an image
}
\description{
This function finds the dominant colour in an image. This is used when the option
"remove_dominant_colour" argument in 
\code{\link[=pairwise_comparison]{pairwise_comparison()}} is TRUE.
}
\usage{
find_dominant_colour(image)
}
\arguments{
\item{image}{The image in which the dominant colour must be determined}
}
\value{
Dominant colour
}
\references{
\url{https://sites.google.com/view/equal-group/home}}
\author{Kurinchi Gurusamy}
\note{This is part of a suite of functions required to compare images.}
\seealso{
\code{\link[=pairwise_comparison]{pairwise_comparison()}}
}
\concept{EQUALCompareImages}
\examples{
# Create a plot and save this as file
filename <- tempfile(fileext = ".png")
png(filename)
plot.new()
hist(rnorm(100))
recordPlot()
dev.off()
# Read image
library(magick)
image <- image_read(filename)
# Perform the function
results <- find_dominant_colour(image)
}
