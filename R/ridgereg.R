#' Ridge Regression
#'
#' @param formula formula
#' @param data data.frame
#' @param lambda numeric penalty parameter >= 0
#' @return ridgereg S3 object
#' @export
ridgereg <- function(formula, data, lambda){

  mf <- stats::model.frame(formula, data)
  y <- stats::model.response(mf)
  X <- stats::model.matrix(formula, data)

  # Extract predictor columns excluding intercept
  X_no_int <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  means <- colMeans(X_no_int)
  sds <- apply(X_no_int, 2, stats::sd)
  X_scaled <- scale(X_no_int, center = means, scale = sds)

  # Construct scaled design matrix with intercept column
  X_mat <- cbind(1, X_scaled)
  p <- ncol(X_mat)

  # Penalty matrix, do NOT penalize intercept term
  I_mat <- diag(p)
  I_mat[1, 1] <- 0

  # Compute ridge regression coefficients on scaled predictors
  beta_ridge <- solve(t(X_mat) %*% X_mat + lambda * I_mat) %*% t(X_mat) %*% y

  # Rescale coefficients back to original data scale
  beta_out <- numeric(length(beta_ridge))
  names(beta_out) <- colnames(X)
  beta_out[-1] <- (beta_ridge[-1] / sds)
  beta_out[1]  <- beta_ridge[1] - sum(beta_out[-1] * means)

  # Fitted values on original response scale
  y_hat_original <- as.vector(X %*% beta_out)

  res <- list(
    coefficients = beta_out,
    fitted_values = y_hat_original,
    formula = formula,
    data = data,
    lambda = lambda,
    means = means,
    sds = sds
  )
  class(res) <- "ridgereg"
  return(res)
}

#' Print method for ridgereg
#' @param x ridgereg object
#' @param ... ignore
#' @export
print.ridgereg <- function(x, ...){
  cat("Call:\n")
  cat(paste0("ridgereg(formula = ", deparse(x$formula), ", lambda = ", x$lambda, ")\n"))
  cat("\nCoefficients:\n")
  print(stats::setNames(x$coefficients, names(x$coefficients)))
}

#' Extract coefficients from ridgereg object
#' @param object ridgereg
#' @param ... ignore
#' @export
coef.ridgereg <- function(object, ...){
  object$coefficients
}

#' Predict method for ridgereg
#' @param object ridgereg
#' @param newdata optional new data frame
#' @param ... ignore
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...){
  if(is.null(newdata)){
    return(object$fitted_values)
  }
  Xnew <- stats::model.matrix(object$formula, data = newdata)
  pred <- Xnew %*% stats::coef(object)
  as.vector(pred)
}
