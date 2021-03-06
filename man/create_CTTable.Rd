% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recocam_various_helper_functions.R
\name{create_CTTable}
\alias{create_CTTable}
\title{Generates a data.frame that summarizes the operational days of each camera used in the trip.
This function is hard coded to use a particular set of columns. You must use the default arguments for gather_images at this time.}
\usage{
create_CTTable(data_from_images, df_stations, trip_id, ct_columns)
}
\arguments{
\item{data_from_images}{data.frame with key attributes - trip, site, camera_no, site_code, moon_phase_number, species, count.}

\item{df_stations}{Path string to the  to camera stations csv file that has 5 attributes, SiteCode, lat, long, Site,
Camera_Trap, Notes, Easting, Northing.}

\item{trip_id}{Character string representing the trip. E.g. Jun.10, Jun.11, Sep.14, Apr.15.
The value is the trip for which the camera efforts are to be calculated.}

\item{ct_columns}{a list of columns to extract data from.}
}
\value{
camtrapR CTTable data.frame based on the user's images
}
\description{
Generates a data.frame that summarizes the operational days of each camera used in the trip.
This function is hard coded to use a particular set of columns. You must use the default arguments for gather_images at this time.
}
