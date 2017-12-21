test_that("make_filename is makeing proper name", {
  expect_equal(make_filename("2014"), "accident_2014.csv.bz2")
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
})
