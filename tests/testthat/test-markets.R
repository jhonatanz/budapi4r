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

test_that("volume() brings a tibble", {
  expect_type(volume("BTC-CLP"), "list")
  expect_s3_class(volume("BTC-CLP"), c("tbl_df", "tbl", "data.frame"))
})
#> Test passed

test_that("volume() has one row", {
  expect_true(nrow(volume("BTC-CLP"))==1)
})
#> Test passed

test_that("ticker() brings a tibble", {
  expect_type(ticker("BTC-CLP"), "list")
  expect_s3_class(ticker("BTC-CLP"), c("tbl_df", "tbl", "data.frame"))
})
#> Test passed

test_that("ticker() has one row", {
  expect_true(nrow(ticker("BTC-CLP"))==1)
})
#> Test passed

test_that("tickers() brings a tibble", {
  expect_type(tickers(), "list")
  expect_s3_class(tickers(), c("tbl_df", "tbl", "data.frame"))
})
#> Test passed

test_that("tickers() has one or more rows", {
  expect_true(nrow(tickers())>=1)
})
#> Test passed

test_that("orders_book(some market) brings a tibble", {
  expect_type(orders_book("BTC-CLP"), "list")
  expect_s3_class(orders_book("BTC-CLP"), c("tbl_df", "tbl", "data.frame"))
})
#> Test passed

test_that("orders_book(some market) has one or more rows", {
  expect_true(nrow(orders_book("BTC-CLP"))>=1)
})
#> Test passed
