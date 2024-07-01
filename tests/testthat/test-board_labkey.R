test_that("Create a labkey board", {
  # uses LABKEY_API_KEY env var
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
  # Rlabkey v3.2.1 and higher fixes issues of remembered credentials
  # Previously needed to reset httr handle
  # httr::handle_reset(Rlabkey::labkey.getBaseUrl())
  # Rlabkey::labkey.setDefaults() # should unset defaults

  suppressWarnings(
    expect_error(
      board_labkey(
        board_alias = "pins-test",
        base_url = "https://learn.labkey.com/",
        folder = "LabKey_Board/",
        subdir = "new_test_folder",
        api_key = "123456"
        ),
    regexp = "HTTP request was unsuccessful. Status code = 401"
    )
  )
})
