library(testthat)
library(MASS)
test_that("ridgereg compare lm lambda=0", {
  data(iris)
  mod1 <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, iris, lambda=0)
  mod2 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, iris)
  expect_equal(coef(mod1), unname(coef(mod2)), tolerance = 1e-4)
})

test_that("predict works with newdata", {
  data(iris)
  m <- ridgereg(Petal.Length~Sepal.Length, iris, lambda=1)
  pred1 <- predict(m)
  pred2 <- predict(m, newdata = iris[1:10,])
  expect_length(pred2,10)
})
