test_that("ridgereg rejects erroneous input", {
  expect_error(ridgereg(Petal.Length ~ Sepal.Width, data = iris, lambda = -1))
  expect_error(ridgereg(Petal.Length ~ Sepal.Width, data = iris, lambda = "a"))
  expect_error(ridgereg(Petal.Length ~ Sepal.Width, data = as.matrix(iris)))
})

test_that("ridgereg returns a ridgereg object with named coefficients", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris,
                  lambda = 1)

  expect_s3_class(mod, "ridgereg")
  expect_named(coef(mod), c("(Intercept)", "Sepal.Width", "Sepal.Length"))
  expect_length(fitted(mod), nrow(iris))
  expect_length(resid(mod), nrow(iris))
})

test_that("lambda = 0 gives the ordinary least squares fit", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris,
                  lambda = 0)
  ols <- lm(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  expect_equal(unname(coef(mod)), unname(coef(ols)), tolerance = 1e-10)
  expect_equal(unname(fitted(mod)), unname(fitted(ols)), tolerance = 1e-10)
})

test_that("coefficients are similar to MASS::lm.ridge", {
  for (lambda in c(1, 10, 100)) {
    mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris,
                    lambda = lambda)
    mass <- MASS::lm.ridge(Petal.Length ~ Sepal.Width + Sepal.Length,
                           data = iris, lambda = lambda)

    expect_equal(unname(coef(mod)), unname(as.numeric(coef(mass))),
                 tolerance = 0.01)
  }
})

test_that("predict works with and without newdata", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris,
                  lambda = 1)

  # without newdata: the fitted values
  expect_equal(predict(mod), mod$fitted_values)

  # with newdata: the manual computation from the coefficients
  newdata <- iris[1:5, ]
  manual <- coef(mod)["(Intercept)"] +
    coef(mod)["Sepal.Width"] * newdata$Sepal.Width +
    coef(mod)["Sepal.Length"] * newdata$Sepal.Length
  expect_equal(unname(predict(mod, newdata = newdata)), unname(manual),
               tolerance = 1e-12)
})

test_that("the penalty shrinks the coefficients towards zero", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris,
                  lambda = 1e6)

  expect_true(all(abs(coef(mod)[-1]) < 1e-3))
  expect_equal(unname(coef(mod)["(Intercept)"]), mean(iris$Petal.Length),
               tolerance = 1e-3)
})

test_that("visualize_airport_delays returns ggplot object", {
  skip_if_not_installed(c("nycflights13","dplyr","ggplot2"))
  p <- visualize_airport_delays()
  expect_s3_class(p, "ggplot")
})
