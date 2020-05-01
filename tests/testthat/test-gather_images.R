context("Data import")

# test full functionality of gather images
# this test takes a bit of time to run because it is actually reading images

test_that("exif data is extracted and processed", {

  # load expected results
  expected <- readr::read_csv(system.file("extdata/expected.csv", package="exifpro2camtrapr"))

  # load stuff for testing
  testy <- system.file("testdata/testimages", package="exifpro2camtrapr")
  testtargets <- system.file("extdata/keywords.csv", package="exifpro2camtrapr")


  gather_test <- gather_images(testy, testtargets)

  expected <- ncol(expected) - 3 # subtracting 3 here, because the current function works differently than the original version of this function.
  actual <- ncol(gather_test)
  print("Expected is...")
  print(expected)
  print("Actual is...")
  print(actual)
  expect_equal(expected, actual)

})


# test that the FileName EXIF attribute column is split into two

test_that("FileName EXIF attribute is split into two columns", {
  # Make some dummy data
  df_test <- data.frame(a = c("chicken.JPG", "dog.jpg"))
  names(df_test) <- c("FileName")

  df_test <- df_test %>%
    separate(FileName,
             c("File", "Extension"),
             sep = "[.]") %>%
    mutate(Extension = tolower(Extension)) %>%
    janitor::clean_names() %>%
    filter(extension == "jpg")

  expected <- ncol(data.frame(a =c(1), b =c(1)))
  actual <- ncol(df_test)
  print("Expected is...")
  print(expected)
  print("Actual is...")
  print(actual)
  expect_equal(expected, actual)
})
