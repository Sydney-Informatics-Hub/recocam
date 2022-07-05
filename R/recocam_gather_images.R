#' Extracts and processes image EXIF data into a nice data frame.
#'
#' Better than the original gather_images function in every single way. This function reads exif data from images and creates a data frame from these values. This function can extract any information that you want and put it together into a data frame. Note that the tool we use to read EXIF data (exifr) treats everything as a character string. This function entrusts changing variable types to you and your discression :) If you want to analyze this data with camtrapR, you can then use the prep_camtrapr function on the output of gather_images.
#' @param images_path Path to a directory/folder that contains the desired images.
#' @param tags_path Path to a csv file that contains the information needed to map tags information contained in the keywords EXIF attribute.
#' @param exif_targets A list of EXIF attributes to extract from image files. If not specified, default is "FileName", "DateTimeOriginal","FileSize", "Keywords". These four defaults attributes will be included in any image taken according to CIPA standards, and only advanced users should consider supplying custom EXIF attribute targets. Currently, the only non-optional keywords are FileName and Keywords. However, if you want to use further functions in this package to prepare images for use with camtrapr, you need to use the default attributes to target. Be warned, custom attributes may cause problems.
#' @param tag_sep A regular experession for a character or string of characters to use as a seperator for splitting tag numbers from tag names. This defaults to a period and a space.
#' @return Generates df_fixed, a data.frame with columns for each of the image tags in your tagging scheme.
#' @export

gather_images <- function(images_path, # path to folder(s) containing images
                          tags_path, # path to image tagging scheme csv
                          exif_targets=c("FileName",
                                         "DateTimeOriginal",
                                         "FileSize",
                                         "Keywords"
                          ), # default exif attributes to extract. Custom inputs can work :)
                          tag_sep = "\\.[:space:]"
)

{
  # Read user specified attribute tags
  keyword_tags <- readr::read_csv(tags_path,
                                   col_names = TRUE,
                                   col_types = readr::cols()
                                  ) %>%
    janitor::clean_names() %>% # clean the names of the columns
    dplyr::mutate(exif_attribute = janitor::make_clean_names(exif_attribute)) # clean the contents of the columns

  message("Reading your images")
  message("Depending on how many images you have, this may take some time...")

  # Extract data from images using a function from the exifr package.
  # This is currently the fastest way to read data from images, but it is still a performance bottleneck.
  df <- exifr::read_exif(images_path, # pointer to the image folder(s) path
                  tags = exif_targets, # specifies the EXIF attributes to extract
                  recursive = TRUE, # makes it so that the function reads images in sub folders
                  args = NULL,
                  quiet = TRUE
  )


  message("Processing your images")

  # check if the exif attributes that you specified have been extracted
  if (all(exif_targets %in% as.vector(colnames(df)) )) {
    df["process_image"] <- TRUE
  } else {
    df["process_image"] <- FALSE

  }
  # within the image data dataframe, split the nested list in the keywords EXIF attribute column into a nested dataframe with 2 columns: tag number and tag value
  df_split <- df %>% janitor::clean_names() %>%
    dplyr::mutate(keywords = purrr::map(keywords, ~ reshape2::colsplit(.,
                                                                       tag_sep,
                                                                       names = c('number', 'value')
                                                                       )))
  # now we pivot the keywords nested lists wider, and unnest them wider
  message("Cleaning up data")
  df_wider <- df_split %>%
    dplyr::mutate(
      keywords = purrr::map(keywords, # transform and pivot the keyword nested lists into wide data frames
                            ~tidyr::pivot_wider(.,names_from = number, values_from = value))
      ) %>% tidyr::unnest_wider(keywords) %>% # unnest the keywords nested data frames
    dplyr::mutate_all(as.character) # turn all columns into characters. Trust me this makes things better in the long run.
  # pivot the keyword tags list wider so we can do stuff with it
  keyword_tags_clean <- keyword_tags %>%
    dplyr::rename(number = index) %>%
    mutate(number = gsub("\\)","",number))
  # then use this pivoted keyword tag list to generate a dataframe with column names that are the number of the keyword tag, and values that are the keyword tags
  keyword_numbers <- keyword_tags_clean %>%
    tidyr::pivot_wider(names_from = number, values_from = exif_attribute)
  # join the two data frames so that there are empty columns for missing values.
  # This is wrapped in suppressMessages to prevent full_join() from telling the user what it is joining things by
  message("Almost done!")
  df_joined <- suppressMessages(dplyr::full_join(df_wider, keyword_numbers))
  # Create two character vectors to specify the columns in the dataframe to target and the new names to give them
  tag_numbers <- colnames(keyword_tags_clean %>%  tidyr::pivot_wider(names_from = number, values_from = exif_attribute))
  tag_names <- colnames(keyword_tags_clean %>% tidyr::pivot_wider(names_from = exif_attribute, values_from = number))
  # now reorganize and rename the keyword tag columns based on the number of the keywords using the two character vectors that we just created
  df_fixed <- df_joined %>%
    dplyr::select(all_of(tag_numbers), everything()) %>%
    dplyr::rename_at(dplyr::vars(all_of(tag_numbers)), ~ tag_names)

  # TODO: Find a better solution so we don't have to do stupid things like this.
  df_fixed <- head(df_fixed, -1)

  # count how many images have been processed
  processed_count <- nrow(df_fixed)
  # print a message
  message(processed_count," ", "images have been successfully wrangled!")

  return(df_fixed)
}
