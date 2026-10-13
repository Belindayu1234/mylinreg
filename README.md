# mylinreg

[![R‑CMD‑Check](https://github.com/Belindayu1234/mylinreg/actions/workflows/R-package.yml/badge.svg)](https://github.com/Belindayu1234/mylinreg/actions/workflows/R-package.yml)

[![R-CMD-check.yaml](https://github.com/wenli420/lab4advr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wenli420/lab4advr/actions/workflows/R-CMD-check.yaml)

The goal of `mylinreg` is to implement simple linear regression (`linreg()`) and ridge regression (`ridgereg()`) from scratch in base‑R, without relying on high‑level modelling functions.

## Installation

You can install the development version of mylinreg from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Belindayu1234/mylinreg")
```

## Basic Example: Linear Regression (Lab4)

```r
library(mylinreg)

# fit our homemade linear regression model
mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

# print coefficients
print(mod)

# get fitted values
head(fitted(mod))

# residuals
head(resid(mod))

# diagnostic ggplot2 plots
plot(mod)

# predict for new observations
predict(mod, newdata = iris[1:5, ])
```

## Bonus Example: Ridge Regression

```r
library(mylinreg)
data("BostonHousing", package = "mlbench")

# fit ridge regression with a fixed penalty λ
ridge_mod <- ridgereg(medv ~ ., data = BostonHousing, lambda = 1)

# coefficients and prediction
coef(ridge_mod)
predict(ridge_mod, newdata = head(BostonHousing))
```

## Package Structure

- `linreg()`: Ordinary least‑squares linear regression (Lab 4 assignment)
- `ridgereg()`: L2‑penalized ridge regression (Bonus lab assignment)
- S3‑methods: `print()`, `coef()`, `fitted()`, `resid()`, `predict()`, `plot()`, `summary()`
- Vignettes:
  - `introduction.Rmd`: demonstration for linreg
  - `ridgereg.Rmd`: full ridge regression case‑study with caret cross‑validation
