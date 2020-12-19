library(testthat)
library(testthis)
library(popMean)
#context("popmean_function")

test_that("Empty dataset", {

  dat <- data.frame()

  expect_error(
    popmean(data = dat, intervention = interventionType, years = 2020:2022)
  )

})

test_that("Try to deal with an unknow intervention type", {

  tdat <- read_testdata("dat.rds")

  interventionType = "AAA"

  expect_error(
    popmean(data = tdat, intervention = interventionType, years = 2020:2022)
  )

})

test_that("Unexpected dataset format",{

  tdat <- read_testdata("dat.rds")

  expect_setequal(
    colnames(tdat),
    c("zone", "population", "year", "none", "IRS", "ITN", "IRS.ITN")
  )
})


