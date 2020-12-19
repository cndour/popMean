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

test_that("Try to deal with an unknown intervention type", {

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

test_that("popmean works as expected", {
  dt = data.frame(zone = c("z1","z2"), population =c(50,50), year = 2020, IRS = c(.01,.25))
  tabmean = popmean(data = dt, intervention = "IRS", years = 2020)
  expect_equal(tabmean$mean, 0.13)
})

