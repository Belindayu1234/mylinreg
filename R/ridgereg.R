#' @title Ridge Regression
#' @description Fits a ridge regression model with penalty \code{lambda}. The
#'   covariates are standardised before estimation, the intercept is not
#'   penalised, and the coefficients are transformed back to the original
#'   scale. The penalised problem is solved with a QR decomposition of the
#'   row-augmented design matrix, which is the (*) exercise of the bonus lab.
#' @param formula A model formula, for example \code{y ~ x1 + x2}.
#' @param data A data frame containing the variables in \code{formula}.
#' @param lambda A single non-negative number, the ridge penalty.
#' @return An object of class \code{ridgereg}: a list with the coefficients,
#'   the fitted values, the penalty and the means and standard deviations that
#'   were used to standardise the covariates.
#' @examples
#' data(iris)
#' ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris, lambda = 1)
#' @importFrom stats coef fitted predict resid
#' @export
ridgereg <- function(formula, data, lambda = 0) {
  if (!inherits(formula, "formula")) stop("'formula' must be a formula")
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (!is.numeric(lambda) || length(lambda) != 1 || !is.finite(lambda) ||
      lambda < 0) {
    stop("'lambda' must be a single non-negative number")
  }

  # Model frame: drops NA rows and keeps the factor levels.
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit,
                           drop.unused.levels = FALSE)
  chr <- vapply(mf, is.character, logical(1))
  mf[chr] <- lapply(mf[chr], factor)          # stable dummy coding
  y <- stats::model.response(mf)
  if (!is.numeric(y)) stop("'formula' must have a numeric response")
  X <- stats::model.matrix(formula, mf)

  has_intercept <- attr(stats::terms(mf), "intercept") == 1
  Xp <- if (has_intercept) X[, -1, drop = FALSE] else X
  if (ncol(Xp) == 0) stop("the model needs at least one covariate")

  # Standardise the covariates; a constant column gets sds = 1.
  y_mean <- if (has_intercept) mean(y) else 0
  means <- if (has_intercept) colMeans(Xp) else rep(0, ncol(Xp))
  means <- stats::setNames(as.numeric(means), colnames(Xp))
  centred <- sweep(Xp, 2, means)
  sds <- apply(centred, 2, stats::sd)
  sds[!is.finite(sds) | sds == 0] <- 1
  sds <- stats::setNames(as.numeric(sds), colnames(Xp))
  Xs <- sweep(centred, 2, sds, "/")

  # The intercept column is added but not penalised.
  if (has_intercept) {
    Xs <- cbind("(Intercept)" = 1, Xs)
    penalty <- diag(ncol(Xs))
    penalty[1, 1] <- 0
  } else {
    penalty <- diag(ncol(Xs))
  }

  # Ridge as least squares on the augmented matrix [Xs; sqrt(lambda) * P].
  augmented <- rbind(Xs, sqrt(lambda) * penalty)
  beta_std <- qr.coef(qr(augmented), c(y - y_mean, rep(0, ncol(Xs))))
  beta_std[is.na(beta_std)] <- 0

  # Back to the original scale.
  if (has_intercept) {
    slopes <- beta_std[-1] / sds
    coefficients <- c("(Intercept)" = y_mean - sum(slopes * means), slopes)
    beta_std <- stats::setNames(as.numeric(beta_std[-1]), colnames(Xp))
  } else {
    coefficients <- stats::setNames(as.numeric(beta_std / sds), colnames(Xp))
    beta_std <- stats::setNames(as.numeric(beta_std), colnames(Xp))
  }

  fitted_values <- drop(X %*% coefficients)
  xlevels <- lapply(mf[vapply(mf, is.factor, logical(1))], levels)

  structure(
    list(
      call = match.call(),
      coefficients = coefficients,
      coefficients_standardized = beta_std,
      fitted_values = fitted_values,
      residuals = y - fitted_values,
      formula = formula,
      terms = stats::terms(mf),
      xlevels = xlevels,
      data = data,
      lambda = lambda,
      means = means,
      sds = sds,
      y_mean = y_mean,
      has_intercept = has_intercept,
      n = nrow(X),
      p = ncol(Xp)
    ),
    class = "ridgereg"
  )
}

#' @title Print method for ridgereg
#' @param x A \code{ridgereg} object.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.ridgereg <- function(x, ...) {
  cat("\nCall:\n", paste(deparse(x$call, width.cutoff = 500L), collapse = "\n"),
      "\n\nCoefficients:\n", sep = "")
  print(x$coefficients)
  invisible(x)
}

#' @title Coefficients of a ridgereg object
#' @param object A \code{ridgereg} object.
#' @param ... Ignored.
#' @return A named vector of coefficients.
#' @export
coef.ridgereg <- function(object, ...) {
  object$coefficients
}

#' @title Fitted values of a ridgereg object
#' @param object A \code{ridgereg} object.
#' @param ... Ignored.
#' @return A numeric vector of fitted values.
#' @export
fitted.ridgereg <- function(object, ...) {
  object$fitted_values
}

#' @title Residuals of a ridgereg object
#' @param object A \code{ridgereg} object.
#' @param ... Ignored.
#' @return A numeric vector of residuals.
#' @export
resid.ridgereg <- function(object, ...) {
  object$residuals
}

#' @title Predict method for ridgereg
#' @description Returns the fitted values when \code{newdata} is missing,
#'   otherwise the predictions for \code{newdata}. The design matrix is built
#'   from the stored terms, so the response is not needed in \code{newdata};
#'   columns are matched by name and rows with missing values give \code{NA}.
#' @param object A \code{ridgereg} object.
#' @param newdata Optional data frame (or matrix) with new covariate values.
#' @param ... Ignored.
#' @return A numeric vector of predictions.
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted_values)
  newdata <- as.data.frame(newdata)

  vars <- all.vars(stats::delete.response(object$terms))
  keep <- names(object$coefficients)
  complete <- stats::complete.cases(newdata[, vars, drop = FALSE])
  out <- rep(NA_real_, nrow(newdata))
  names(out) <- rownames(newdata)

  if (any(complete)) {
    X <- stats::model.matrix(stats::delete.response(object$terms),
                             newdata[complete, , drop = FALSE],
                             xlev = object$xlevels)
    # Dummy columns that do not occur in newdata are zero.
    missing_cols <- setdiff(keep, colnames(X))
    if (length(missing_cols) > 0) {
      X <- cbind(X, matrix(0, nrow(X), length(missing_cols),
                           dimnames = list(NULL, missing_cols)))
    }
    out[complete] <- drop(X[, keep, drop = FALSE] %*% object$coefficients)
  }
  out
}
