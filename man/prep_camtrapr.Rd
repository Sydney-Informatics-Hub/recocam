% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recocam_prep_camtrapr.R
\name{prep_camtrapr}
\alias{prep_camtrapr}
\title{Takes a data frame of information extracted from image EXIF data and processes it into a format usable with the camtrapR package.}
\usage{
prep_camtrapr(df_fixed, number_of_species = 2)
}
\arguments{
\item{df_fixed}{A data frame of data extracted from images using gather_images.}

\item{number_of_species}{An integer specifying how many species could be tagged in a single image. This integer wil be used to help specifiy the number of columns that we want the function to work on.}
}
\value{
df_camtrapr, a data frame that can be used for all sorts of analysis with the camtrapR r package.
}
\description{
To make this function work, you have to do a couple things. First, you need to manually tag your images. These tags must use a specific tagging scheme. Your tags should be numbered. More importantly, you need to have species records recorded like this: one tag for each species in the image (i.e. species_1, species_2), and one tag for the number of that species (i.e. no_of_species_1, no_of_species_2).
}
