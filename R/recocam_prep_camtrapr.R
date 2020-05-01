#' Takes a data frame of information extracted from image EXIF data and processes it into a format usable with the camtrapR package.
#'
#' To make this function work, you have to do a couple things. First, you need to manually tag your images. These tags must use a specific tagging scheme. Your tags should be numbered. More importantly, you need to have species records recorded like this: one tag for each species in the image (i.e. species_1, species_2), and one tag for the number of that species (i.e. no_of_species_1, no_of_species_2).
#' @param df_fixed A data frame of data extracted from images using gather_images.
#' @param number_of_species An integer specifying how many species could be tagged in a single image. This integer wil be used to help specifiy the number of columns that we want the function to work on.
#' @return df_camtrapr, a data frame that can be used for all sorts of analysis with the camtrapR r package.
#' @export

prep_camtrapr <- function(df_fixed,
                          number_of_species = 2
){

  # move filename and all columns with names that contain "species" to the start of the data frame.
  # this way we can now use numbers to refer to the specific number of columns that we want to operate on.
  species_data <- df_fixed %>% dplyr::select(file_name, all_of(contains("species")), everything())

  # this function defines the column number of the last column that contains the word "species"
  last_species_column <- 3+(1*number_of_species)

  # create a specification for how to pivot species/count columns into a unified species/count column
  species_spec <- species_data %>% tidyr::build_longer_spec(
    cols = 2:last_species_column,
    names_to = c(".value", "species_id"),
    names_pattern = "(.*)_(\\d+)$"
  )
  # pivot species/count pairs into one unified pair
  species_pivoted <- tidyr::pivot_longer_spec(species_data, species_spec)

  # finally rename the column containing counts
  df_total <- species_pivoted %>%
    dplyr::rename(count = no_of_species) %>%
    dplyr::mutate_all(as.character) # also make everything characters again to avoid potential issues!

message("Your data is now ready for analysis with camtrapr!")

  return(df_total)
}
