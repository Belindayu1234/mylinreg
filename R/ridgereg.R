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

  # 标准化自变量，不标准化截距列
  X_no_int <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  means <- colMeans(X_no_int)
  sds <- apply(X_no_int, 2, stats::sd)
  X_scaled <- scale(X_no_int, center = means, scale = sds)

  X_mat <- cbind(1, X_scaled)
  p <- ncol(X_mat)

  I_mat <- diag(p)
  I_mat[1,1] <- 0 # intercept NOT penalized

  # ridge beta
  beta_ridge <- solve(t(X_mat) %*% X_mat + lambda * I_mat) %*% t(X_mat) %*% y
  y_hat <- X_mat %*% beta_ridge

  # rescale coefficients back to original scale
  beta_out <- numeric(length(beta_ridge))
  names(beta_out) <- colnames(X)
  #beta_out[1] <- beta_ridge[1] - sum(beta_ridge[-1] * means / sds)
  #beta_out[-1] <- beta_ridge[-1] / sds
  beta_out[-1] <- (beta_ridge[-1] / sds)
  beta_out[1]  <- beta_ridge[1] - sum( beta_out[-1] * means )


  res <- list(
    coefficients = as.vector(beta_out),
    fitted_values = as.vector(y_hat),
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
  cat(paste0("ridgereg(formula = ", deparse(x$formula), ", lambda = ",x$lambda,")\n"))
  cat("\nCoefficients:\n")
  print(stats::setNames(x$coefficients, names(x$coefficients)))
}

#' coef for ridgereg
#' @param object ridgereg
#' @param ... ignore
#' @export
coef.ridgereg <- function(object, ...){
  #stats::setNames(object$coefficients, names(object$coefficients))
  object$coefficients
}

#' predict for ridgereg support newdata
#' @param object ridgereg
#' @param newdata optional new data frame
#' @param ... ignore
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...){
  if(is.null(newdata)){
    return(object$fitted_values)
  }
  Xnew <- stats::model.matrix(object$formula, newdata)
  pred <- Xnew %*% stats::coef(object)
  as.vector(pred)
}
