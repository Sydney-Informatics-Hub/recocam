

# recocam


**recocam** provides robust tools for extracting and processing information and annotations that have been stored in image EXIF data. This package was built to support ecologists who have to deal with large numbers of images from camera traps and to prepare them for analysis with the camtrapr packahe, but it can work with all sorts of images as long as they contain annotation that have been stored in the keywords EXIF attribute field. This package was developed by the Sydney Informatics Hub, a Core Research Facility of the University of Sydney.

This package is designed to be computationally efficient and can process multiple subfolders full of images, but be aware that processing time goes up as more images are included. Current processing speed for the main importing function gather_images() is around 100 images a second.

This package allows users to generate a data frame of information contained in image EXIF data, and can adapt to different tagging schemes as long as they follow a structure (see below). The other functions in the package are designed to enable users to use their image data with **camtrapr**'s data exploration tools.

## Installation

Before you can install and use **recocam**, you must install one program (**ExifTool**, a free and open source program for extracting image metadata) and a few R packages (notably **exifr** to enable efficient use of **ExifTool**, with **reshape2** and **Tidyverse** providing some important functions).

### ExifTool

The method for installing **ExifTool** on your computer varies depending on if you are using MacOS/Linux/Unix or Windows. You can find out more about **ExifTool** and download the software [here](https://exiftool.org/).

On MacOS/Linux/Unix, simply follow the steps provided on **ExifTool**'s website. For Windows, you will have take a couple extra steps to ensure that R is able to utilize **ExifTool**. Once you have downloaded the stand-alone executable, unzip the file and rename the .exe file to "exiftool.exe". Then place this file in your primary windows directory (C:/Windows in most cases). Once you have done this, R should be able to find **ExifTool**.

To make sure that you have correctly installed **ExifTool**:
```{r exif tool stuff}
#Check if your system is able to find ExifTool
Sys.which("exiftool")

#This is what you would see on MacOS. On Windows, you should see "C:\\WINDOWS\\exiftool.exe"
```
### recocam

To install **recocam**, you can download it from the Sydney Informatics Hub's github. 

``` {r installing packages, eval=FALSE}
library(devtools)
install_github("https://github.com/Sydney-Informatics-Hub/recocam")
```
If you have any questions about this package, run into any issues, or simply want to see what is going on with development of **recocam**, check out the repository's issues page.

### Citing recocam

If you use recocam for a publication, presentation, or any form professional application, please cite recocam as well as the Sydney Informatics Hub, a Core Research Facility of the University of Sydney.
