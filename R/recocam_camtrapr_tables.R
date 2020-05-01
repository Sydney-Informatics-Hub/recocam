#' Generate a data frame that is compatible with the camtrapR recordTable data structure.
#'
#' @param data_from_images dataframe of image data created using the gather_images function
#' @param delta_time_compared_to Character string having values of lastIndependentRecord (default) or lastRecord. This is used to compare the time difference to previous records of same species in a station.
#' @param camera_independence logical. option to control if cameras are independent from each other. Default is set to true.
#' @param remove_non_independent_records logical. option to control including or removing records that have been assessed to be non independent. Default is set to true.
#' @param min_delta_time integer. integer value specifying independence of records, if set to 0, all images are independent and no temporal filtering is needed. Default is set to 3.
#' @return Generates a dataframe with species temporal information for the all camera traps and stations that are present in the trip.
#' @export
make_recordtable <- function(data_from_images,
                                     delta_time_compared_to="lastIndependentRecord",
                                     camera_independence = TRUE,
                                     remove_non_independent_records = TRUE,
                                     min_delta_time = 3
)
{

  images_with_species <- data_from_images %>%
                              dplyr::filter(species != "NA"  & process_image == TRUE) %>%
                              dplyr::distinct() %>%
                              dplyr::mutate(date_time_original = lubridate::ymd_hms(date_time_original)
                                     ) %>%
                              dplyr::arrange(date_time_original)

    record_table <- make_recordtable_helper(images_with_species,
                                                       delta_time_compared_to,
                                                       camera_independence,
                                                       remove_non_independent_records,
                                                       min_delta_time)

  return(record_table)
}
#' Generates a dataframe that summarizes the operational days of each camera used in the trip.
#'
#' @param data_from_images dataframe with the extracted exif data with key attributes, Trip, DateTimeOriginal, Camera.No, Species, Count.
#' @param camera_stations_file_path Path string to the  to camera stations csv file that has 5 attributes, SiteCode, lat, long,
#' Site, Camera_Trap, Notes, Easting, Northing.
#' @param trip Character string representing the trip. E.g. Jun.10, Jun.11, Sep.14, Apr.15. The value is the trip for which the camera efforts are to be calculated.
#' @param ct_columns A list of columns to target in the data.frame of extracted camera trap data.
#' @return Returns a dataframe compatabile with CTtable of camtrapR package.
#' @importFrom readr read_csv
#' @export
make_effortstable <- function(data_from_images,
                                     camera_stations_file_path,
                                     trip,
                              ct_columns = c("trip",
                                             "treatment",
                                             "site_code",
                                             "site",
                                             "camera_no",
                                             "date_time_original",
                                             "species",
                                             "comments")

)
{
  df_stations <- readr::read_csv(camera_stations_file_path,
                                 col_names = TRUE,
                                 col_types = cols()
                                 ) %>% janitor::clean_names()
# Call helper function to process the camera stations file
  df_efforts <- create_CTTable(data_from_images,
                               df_stations,
                               trip,
                               ct_columns) %>%
    drop_na()
  return(df_efforts)
}
