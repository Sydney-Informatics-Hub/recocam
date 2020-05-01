# additional vignette data

library(recocam)

# prep another season's data ----
keywords <- system.file("testdata/keywords.csv", package="recocam")

system.time(season2_raw <- gather_images("/Users/hlyd4326/Desktop/PIPE1009/2013-04\ photos", keywords))

season1_raw <- gather_images("/Users/hlyd4326/Desktop/PIPE1009/2010-06\ photos", keywords)

season1_clean <- prep_camtrapr(season1_raw)

season2_clean <- prep_camtrapr(season2_raw)

season1_records <- make_recordtable(season1_clean)
season1_efforts <- make_effortstable(season1_clean, camsites, trip = "Jun.10")

season2_records <- make_recordtable(season2_clean)

camsites <- system.file("extdata/CamSitesDemo.csv", package="recocam")

season2_efforts <- make_effortstable(season2_clean, camsites, trip = "Apr.13")

# testing with a new batch of images, because something went wrong :( ----
# this got an error? Error: Can't join on '1' x '1' because of incompatible types (character / list)
season3_raw <- gather_images("/Users/hlyd4326/Desktop/PIPE1009/2014-04\ photos", keywords)

season3_superraw <- read_exif("/Users/hlyd4326/Desktop/PIPE1009/2014-04\ photos",
                              tags=c("FileName",
                              "DateTimeOriginal",
                              "FileSize",
                              "Keywords"),
                              recursive = TRUE,
                              args = NULL,
                              quiet = TRUE)

df <- season3_superraw

tags_path <- system.file("testdata/keywords.csv", package="recocam")

keyword_tags <- readr::read_csv(tags_path,
                                col_names = TRUE,
                                col_types = readr::cols()
) %>%
  janitor::clean_names() %>% # clean the names of the columns
  dplyr::mutate(exif_attribute = janitor::make_clean_names(exif_attribute))


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
    keywords = purrr::map(keywords,
                          ~tidyr::pivot_wider(.,names_from = number, values_from = value))
  ) %>%
  tidyr::unnest_wider(keywords) %>% dplyr::mutate_all(as.character)
# pivot the keyword tags list wider so we can do stuff with it
keyword_tags_clean <- keyword_tags %>%
  dplyr::rename(number = index)
# then use this pivoted keyword tag list to generate a dataframe with column names that are the number of the keyword tag, and values that are the keyword tags
keyword_numbers <- keyword_tags_clean %>%
  tidyr::pivot_wider(names_from = number, values_from = exif_attribute)
# join the two data frames so that there are empty columns for missing values.
# This is wrapped in suppressMessages to prevent left_join() from telling the user what it is joining things by
message("Almost done!")
df_joined <- suppressMessages(dplyr::full_join(df_wider, keyword_numbers))
# Create two character vectors to specify the columns in the dataframe to target and the new names to give them
tag_numbers <- colnames(keyword_tags_clean %>% tidyr::pivot_wider(names_from = number, values_from = exif_attribute))
tag_names <- colnames(keyword_tags_clean %>% tidyr::pivot_wider(names_from = exif_attribute, values_from = number))
# now reorganize and rename the keyword tag columns based on the number of the keywords using the two character vectors that we just created
df_fixed <- df_joined %>%
  dplyr::select(all_of(tag_numbers), everything()) %>%
  dplyr::rename_at(dplyr::vars(all_of(tag_numbers)), ~ tag_names)


# clean up season 1's data so it doesn't have Tony's tag test columns ----
season1_clean <- season1_clean %>%
  select(!12:13)


# save data for vignette ----
# saving Rds versions
saveRDS(season1_raw, "./inst/extdata/vignette_raw1.Rds")
saveRDS(season1_clean, "./inst/extdata/vignette_clean1.Rds")
saveRDS(season1_records, "./inst/extdata/vignette_records1.Rds")
saveRDS(season1_efforts, "./inst/extdata/vignette_efforts1.Rds")

saveRDS(season2_clean, "./inst/extdata/vignette_clean2.Rds")
saveRDS(season2_records, "./inst/extdata/vignette_records2.Rds")
saveRDS(season2_efforts, "./inst/extdata/vignette_efforts2.Rds")


# saving csv versions
write_csv(season2_clean, "./inst/extdata/vignette_clean2.csv")
write_csv(season2_records, "./inst/extdata/vignette_records2.csv")
write_csv(season2_efforts, "./inst/extdata/vignette_efforts2.csv")

season2_clean <- readr::read_csv("./inst/extdata/vignette_season2.csv")
season2_records <- readr::read_csv("./inst/extdata/vignette_records2.csv")
season2_efforts <- readr::read_csv("./inst/extdata/vignette_efforts2.csv")

# vignette to show how to bind rows ----

merged_efforts <- dplyr::bind_rows(season1_efforts, season2_efforts)

season1_clean <- readRDS(system.file("/extdata/vignette_clean1.Rds", package= "recocam"))
season2_clean <- readRDS(system.file("/extdata/vignette_clean2.Rds", package= "recocam"))

merged_images_clean <- dplyr::bind_rows(season1_clean, season2_clean)

merged_records <- dplyr::bind_rows(season1_records, season2_records)


