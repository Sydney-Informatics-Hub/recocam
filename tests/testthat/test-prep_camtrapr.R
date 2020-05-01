# test is multiple species in a single image will be split into multiple rows
test_that("Multiple species in a single image wil be split into seperate rows", {

  expected <- readr::read_csv(system.file("extdata/expected.csv", package="recocam"))

  # load stuff for testing
  testy <- system.file("testdata/testimages", package="recocam")
  testtargets <- system.file("extdata/keywords.csv", package="recocam")

  gather_test <- recocam::gather_images(testy, testtargets)

  # now actual testing
  test <- recocam::prep_camtrapr(gather_test)
  # We know that if correctly processed, the test file should have 6 rows
  expected <- 6
  # how many rows does it have?
  actual <- nrow(test)
  print(expected)
  print(actual)
  # if the function works, the number of rows should be 6
  expect_equal(expected, actual)
})
