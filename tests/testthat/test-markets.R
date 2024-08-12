test_that("markets() brings a tibble", {
  expect_type(markets(), "list")
  expect_s3_class(markets(), c("tbl_df", "tbl", "data.frame"))
})
#> Test passed

test_that("markets() has one or more rows", {
  expect_true(nrow(markets())>=1)
})
#> Test passed

test_that("markets(some market id) returns a tibble with one row", {
  l <- markets()$id
  for(i in l){
    expect_true(nrow(markets(i))==1)
  }
})
#> Test passed
