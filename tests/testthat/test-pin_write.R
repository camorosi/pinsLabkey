test_that("Write a pin on a labkey board", {
  withr::local_envvar(c("LABKEY_API_KEY" = Sys.getenv("LABKEY_API_KEY")))

  board <- board_labkey(
    board_alias = "pins-test", # becomes 'labkey-pins-test in cache
    base_url = "https://learn.labkey.com/",
    folder = "LabKey_Board/",
    subdir = "pins"
  )

  resp <- board %>% pin_write(mtcars, "mtcars")
  expect_equal(resp, "mtcars") # TODO capture messages as well?

})
