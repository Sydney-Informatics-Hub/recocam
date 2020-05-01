

# recocam

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/informatics/recocam.svg?branch=master)](https://travis-ci.org/informatics/recocam)
<!-- badges: end -->

**recocam** provides robust tools for processing and extracting information from collections of camera trap images, and is optimized to prepare manually tagged JPG images for analysis with the **camtrapR** package.

This package is designed to be computationally efficient and can process multiple subfolders full of images, but be aware that processing time goes up as more images are included.

This package produces three main outputs: a species record table, camera trap information table, and a rejected image table.

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

To install **recocam**, you must connect with the the University of Sydney's enterprise Github page.

``` {r installing packages, eval=FALSE}
#something like this, placeholder now
library(devtools)
install_github("https://github.com/Sydney-Informatics-Hub/recocam")
```
### Citing recocam

If you use recocam for a publication, presentation, or any form professional application, please cite recocam as well as the Sydney Informatics Hub, a Core Research Facility of the University of Sydney
