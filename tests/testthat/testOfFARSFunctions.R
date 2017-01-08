context("Testing FARS functions")

test_that("make_filename", {
  expect_equal(make_filename("2015"), "accident_2015.csv.bz2")
})

test_that("fars_read", {
  expect_error(fars_read("sillyFile"), "file 'sillyFile' does not exist")
})


