# Helper board function
board_labkey_test <- function() {
  # uses LABKEY_API_KEY env var

  board_labkey(
    board_alias = "pins-test", # becomes 'labkey-pins-test in cache
    base_url = "https://learn.labkey.com/",
    folder = "LabKey_Board/",
    subdir = "pins"
  )
}

test_that("Write a pin on a labkey board", {

  board <- board_labkey_test()

  # this is returning messages even though pins.quiet is TRUE
  resp <- board %>% pin_write(mtcars, "mtcars", type = "rds")
  expect_equal(resp, "mtcars")
})

test_that("Write pin with same hash", {
  withr::local_options(pins.quiet = FALSE)

  board <- board_labkey_test()

  suppressMessages(
    expect_message(board %>% pin_write(mtcars, "mtcars", type = "rds"),
                   regexp = "The hash of pin \"mtcars\" has not changed")
  )
})

test_that("Write pin with new hash", {
  withr::local_options(pins.quiet = FALSE)

  board <- board_labkey_test()

  suppressMessages(
    expect_message(board %>% pin_write(mtcars[1:10,], "mtcars", type = "rds"),
                   regexp = "Creating new version")
  )

  # get pin versions to check
  pin_versions <- board %>% pin_versions(name = "mtcars")
  expect_s3_class(pin_versions, "data.frame")
  expect_equal(colnames(pin_versions), c("version", "created", "hash"))
  expect_equal(nrow(pin_versions), 2)
  expect_equal(pin_versions$hash, c("76dea", "0439b"))
})

test_that("Read latest pin", {
  board <- board_labkey_test()

  latest_pin <- board %>% pin_read(name = "mtcars")

  expect_equal(latest_pin, mtcars[1:10,])
})

test_that("Read latest pin by hash", {
  board <- board_labkey_test()

  latest_pin <- board %>% pin_read(name = "mtcars", hash = "0439b")

  expect_equal(latest_pin, mtcars[1:10,])
})

test_that("Read pin by version", {
  board <- board_labkey_test()

  pin_versions <- board %>% pin_versions(name = "mtcars")

  first_pin <- board %>% pin_read(name = "mtcars", version = pin_versions$version[1])
  expect_equal(first_pin, mtcars)
})

test_that("Read latest pin by version", {
  board <- board_labkey_test()

  pin_versions <- board %>% pin_versions(name = "mtcars")

  latest_pin <- board %>% pin_read(name = "mtcars", version = pin_versions$version[2])
  expect_equal(latest_pin, mtcars[1:10,])
})

test_that("List available pins", {
  board <- board_labkey_test()

  pins_avail <- board %>% pin_list()

  expect_equal(pins_avail, "mtcars")
})

test_that("Try to delete pin that doesn't exits", {
  board <- board_labkey_test()

  expect_error(board %>% pin_delete(names = "foobar"),
               regexp = "Can't find pin called")
})

test_that("Delete pin version", {
  board <- board_labkey_test()

  pin_versions <- board %>% pin_versions(name = "mtcars")
  version_to_delete <- pin_versions$version[1]

  # note pin delete does not delete from the cache
  board %>% pin_version_delete(name = "mtcars", version = version_to_delete)

  pin_versions_after <- board %>% pin_versions(name = "mtcars")

  expect_false(version_to_delete %in% pin_versions_after$version)
})

test_that("Delete pin", {
  board <- board_labkey_test()

  # note pin delete does not delete from the cache
  board %>% pin_delete(names = "mtcars")

  pins_avail <- board %>% pin_list()

  expect_false("mtcars" %in% pins_avail)
})
