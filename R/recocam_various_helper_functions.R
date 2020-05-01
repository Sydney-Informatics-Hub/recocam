#' Construct a recordTable, data structure that is compatible with camtrapR for only species image.
#'
#' @param df_with_species_images R dataframe ofonly images that have species in them, with key attributes - trip, site, camera_no, site_code, moon_phase_number, species, count.
#' @param delta_time_compared_to Character string having values of lastIndependentRecord (default) or lastRecord.
#' This is used to compare the time difference to previous records of same species in a station.
#' @param camera_independence Logical value with default set to TRUE.
#' @param remove_non_indpendent_records Logical value with default set to TRUE.
#' @param min_delta_time Integer value specifying independence of records, if set to 0,
#' all images are independent and no temporal filtering is needed. Default is set to 3.
#'
#' @return R Data frame which has all the attributes of cameratrapR recordTable data structure.
make_recordtable_helper <- function(df_with_species_images,
                                delta_time_compared_to,
                                camera_independence,
                                remove_non_indpendent_records,
                                min_delta_time
)
{
  record_table_species <- assess_tempora_independence(intable = df_with_species_images,
                                                      deltaTimeComparedTo = delta_time_compared_to,
                                                      columnOfInterest = "species",
                                                      stationCol = "site_code",
                                                      cameraCol = "camera_no",
                                                      camerasIndependent = camera_independence,
                                                      minDeltaTime = min_delta_time,
                                                      removeNonIndependentRecords = remove_non_indpendent_records)

  return(record_table_species)
}

#' Generates a data.frame that summarizes the operational days of each camera used in the trip.
#' This function is hard coded to use a particular set of columns. You must use the default arguments for gather_images at this time.
#'
#' @param data_from_images data.frame with key attributes - trip, site, camera_no, site_code, moon_phase_number, species, count.
#' @param df_stations Path string to the  to camera stations csv file that has 5 attributes, SiteCode, lat, long, Site,
#' Camera_Trap, Notes, Easting, Northing.
#' @param trip_id Character string representing the trip. E.g. Jun.10, Jun.11, Sep.14, Apr.15.
#' The value is the trip for which the camera efforts are to be calculated.
#' @param ct_columns a list of columns to extract data from.
#' @return camtrapR CTTable data.frame based on the user's images
#' @importFrom dplyr select
#'
create_CTTable <- function(data_from_images,
                           df_stations,
                           trip_id,
                           ct_columns
)
{
  df_stations <- df_stations

  data_from_images$date_time_original <- as.character.POSIXt(data_from_images$date_time_original)

  df_non_species_images <- data_from_images %>%
    dplyr::filter(!is.na(comments))

  df_non_species_images <- df_non_species_images %>%
    dplyr::select(all_of(ct_columns)) %>%
    dplyr::distinct(date_time_original,
             species,
             site_code,
             camera_no,
             .keep_all = TRUE)

  df_last_photo_taken <- df_non_species_images %>%
    dplyr::filter(comments == "last photo taken")

  df_efforts_calculation <- df_last_photo_taken %>%
    dplyr::left_join(df_non_species_images,
              by = c("trip", "treatment", "site_code", "site", "camera_no")) %>%
    dplyr::filter(comments.x == "last photo taken" &
             comments.y == "first photo taken") %>%
    dplyr::rename(last_photo_taken = date_time_original.x) %>%
    dplyr::rename(first_photo_taken = date_time_original.y) %>%
    dplyr::select (-c(species.x, species.y)) %>%
    tidyr::separate(last_photo_taken,
             c("last_photo_taken_date", "last_photo_taken_time"), sep = " ") %>%
    tidyr::separate(first_photo_taken,
             c("first_photo_taken_date", "first_photo_taken_time"), sep = " ")

  df_stations_effort <- df_efforts_calculation %>%
    dplyr::left_join(df_stations, by = "site_code") %>%
    dplyr::filter(trip == trip_id) %>%
    janitor::clean_names() %>%
    dplyr::select (-c(comments_x, comments_y)) %>%
    dplyr::rename(site = site_x)

  return(df_stations_effort)
}
#'
# This function is extracted and modified from the camtrapR package and is used to check for temporal and camera independence when making a  make a recordTable, which is a data.frame that is compatible with camtrapR package to analyze species movement.


assess_tempora_independence <- function(intable,
                                        deltaTimeComparedTo,
                                        columnOfInterest,
                                        cameraCol,
                                        camerasIndependent,
                                        stationCol,
                                        minDeltaTime,
                                        removeNonIndependentRecords = TRUE
)
{

  # check if Exif date tags were read correctly
  if (any(is.na(intable$date_time_original))) {
    which.tmp <- which(is.na(intable$date_time_original))
    if(length(which.tmp) == nrow(intable)) stop("Could not read any Exif DateTimeOriginal tag at station: ",
                                                paste(unique(intable[which.tmp, stationCol])),
                                                " Consider checking for corrupted Exif metadata.")
    warning(paste("Could not read Exif DateTimeOriginal tag of",
                  length(which.tmp),
                  "image(s) at station",
                  paste(unique(intable[which.tmp, stationCol]), collapse = ", "),
                  ". Will omit them. Consider checking for corrupted Exif metadata. \n",
                  paste(file.path(intable[which.tmp, "directory"],
                                  intable[which.tmp, "file_name"]),
                        collapse = "\n")),
            call. = FALSE, immediate. = TRUE)
    intable <- intable[-which.tmp ,]
    rm(which.tmp)
  }

  # prepare to add time difference between observations columns
  intable <- data.frame(intable,
                        delta.time.secs  = NA,
                        delta.time.mins  = NA,
                        delta.time.hours = NA,
                        delta.time.days  = NA)

  # introduce column specifying independence of records
  if (minDeltaTime == 0) {
    intable$independent <- TRUE    # all independent if no temporal filtering
  } else {
    intable$independent <- NA
  }


  for (xy in 1:nrow(intable)) {     # for every record

    # set independent = TRUE if it is the 1st/only  record of a species / individual

    if (camerasIndependent == TRUE) {
      if(intable$date_time_original[xy]  == min(intable$date_time_original[which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                                                                 intable[, stationCol]       == intable[xy, stationCol] &
                                                                                 intable[, cameraCol]        == intable[xy, cameraCol]) ])){    # cameras at same station assessed independently
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    } else {
      if(intable$date_time_original[xy]  == min(intable$date_time_original[which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                                                                 intable[, stationCol]       == intable[xy, stationCol]) ])){
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    }

    if(is.na(intable$delta.time.secs[xy])) {   # if not the 1st/only record, calculate time difference to previous records of same species at this station

      if(deltaTimeComparedTo == "lastIndependentRecord"){

        if (camerasIndependent == TRUE) {
          which_time2 <- which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                 intable[, stationCol] == intable[xy, stationCol] &
                                 intable[, cameraCol] == intable[xy, cameraCol] &
                                 intable$independent == TRUE &
                                 intable$date_time_original <  intable$date_time_original[xy])
        } else {
          which_time2 <- which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                 intable[, stationCol] == intable[xy, stationCol] &
                                 intable$independent == TRUE &
                                 intable$date_time_original <  intable$date_time_original[xy])
        }
      }  else {
        if (camerasIndependent  == TRUE) {
          which_time2 <- which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                 intable[, stationCol] == intable[xy, stationCol] &
                                 intable[, cameraCol] == intable[xy, cameraCol] &
                                 intable$date_time_original <intable$date_time_original[xy])
        } else {
          which_time2 <- which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                 intable[, stationCol] == intable[xy, stationCol] &
                                 intable$date_time_original <intable$date_time_original[xy])
        }
      }

      # time difference to last (independent) record
      diff_tmp <- min(na.omit(difftime(time1 = intable$date_time_original[xy],
                                       time2 = intable$date_time_original[which_time2],
                                       units = "secs")))

      # save delta time in seconds
      intable$delta.time.secs[xy] <-  diff_tmp
      if(intable$delta.time.secs[xy] >= (minDeltaTime * 60) | intable$delta.time.secs[xy] == 0) {
        intable$independent[xy] <- TRUE
      } else {
        intable$independent[xy] <- FALSE
      }
    }
  }

  if (removeNonIndependentRecords) {
    # keep only independent records
    outtable <- intable[intable$delta.time.secs >= (minDeltaTime * 60) |
                          intable$delta.time.secs == 0,]
  } else {
    outtable <- intable
  }

  return(outtable)
}

#' Returns the Exifs attribute name from the index in the researcher tagged input document.
#' This function may be deprecated; consider checking?
#'
#' @param keyword_id Attribute index in the researcher tagged input document
#' @param keywords_tags data.frame of the exifs tag index to attribute mapping present in the image.
#'
#' @return Returns the attribute keyword from the index.
fix_attribute_names <- function(keyword_id,
                                keywords_tags
)
{
  attribute_mapping <- keywords_tags %>%
    filter(index == keyword_id)
  attribute_name <- as.character(attribute_mapping[1,2])

  if (as.integer(keyword_id) > dim(keywords_tags)[1]) {
    attribute_name <- paste("invalid_attribute", "_", keyword_id, sep = "")
  }

  return(attribute_name)
}
