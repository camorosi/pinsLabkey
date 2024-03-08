test_that("Create a labkey board", {
  # TODO may need api key here
  board <- board_labkey(
    board_alias = "pins-test",
    base_url = "https://learn.labkey.com/",
    folder = "LabKey_Board/",
    subdir = "pins"
  )
  expect_s3_class(board, "pins_board_labkey")
  expect_equal(names(board), c("board", "api", "cache", "versioned", "name",
                              "base_url", "folder", "subdir", "api_key"))
})

test_that("Create a labkey board with bad credentials", {
  # TODO issues with labkey remembering credentials
  Rlabkey::labkey.setDefaults() # should unset defaults

  expect_error(
    board_labkey(
      board_alias = "pins-test",
      base_url = "https://learn.labkey.com/",
      folder = "LabKey_Board/",
      subdir = "new_folder_that_doesnt_exist",
      api_key = "123456"
      ),
    regexp = "HTTP request was unsuccessful. Status code = 401"
  )
})