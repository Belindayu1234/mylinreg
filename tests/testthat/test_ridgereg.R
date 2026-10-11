library(testthat)
library(MASS)

test_that("ridgereg compare lm lambda=0", {
  data(iris)
  mod1 <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, iris, lambda=0)
  mod2 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, iris)
  expect_equal(unname(coef(mod1)), unname(coef(mod2)), tolerance = 1e-4)
})

test_that("predict works with / without newdata", {
  data(iris)
  m <- ridgereg(Petal.Length~Sepal.Length, iris, lambda=1)
  pred1 <- predict(m)
  expect_equal(pred1, m$fitted_values)
  pred2 <- predict(m, newdata = iris[1:10,])
  expect_length(pred2,10)
})

test_that("coef return named coefficients", {
  data(iris)
  m <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, iris, lambda=2)
  expect_named(coef(m), c("(Intercept)","Sepal.Length","Sepal.Width"))
})

test_that("Compare with MASS::lm.ridge approximate match",{
  data(iris)
  form <- Petal.Length ~ Sepal.Length + Sepal.Width
  m_mass <- MASS::lm.ridge(form, data = iris, lambda = 5)
  m_my <- ridgereg(form, data = iris, lambda = 5)
  # lm.ridge使用SVD实现，允许微小数值差异
  expect_equal(unname(coef(m_my)), unname(coef(m_mass)), tolerance = 1e-2)
})

test_that("large lambda shrink coefficients toward zero",{
  data(iris)
  m_small <- ridgereg(Petal.Length ~ Sepal.Length+Sepal.Width, iris, lambda = 0.1)
  m_large <- ridgereg(Petal.Length ~ Sepal.Length+Sepal.Width, iris, lambda = 10000)
  # large lambda非截距系数趋近0
  expect_true(abs(coef(m_large)[2]) < abs(coef(m_small)[2]))
  expect_true(abs(coef(m_large)[3]) < abs(coef(m_small)[3]))
})
