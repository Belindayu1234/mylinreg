#' Linear Regression using Ordinary Least Squares
#'
#' Fits a multiple linear regression model using matrix algebra.
#' Returns an S3 object of class linreg.
#'
#' @param formula an object of class formula
#' @param data a data frame
#'
#' @return An S3 object of class \code{linreg} containing regression quantities.
#' @export
#'
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' print(mod)
linreg <- function(formula, data){

  # 1. build model matrix X and response y
  X <- stats::model.matrix(formula, data = data)
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]

  n <- nrow(X)
  p <- ncol(X)
  df <- n - p

  # OLS beta hat: (X^T X)^(-1) X^T y
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  beta_hat <- solve(XtX) %*% Xty

  # fitted values
  y_hat <- X %*% beta_hat
  # residuals
  e_hat <- y - y_hat

  # residual variance sigma^2
  sigma2_hat <- as.numeric( (t(e_hat) %*% e_hat) / df )

  # variancecovariance matrix for coefficients
  var_beta <- sigma2_hat * solve(XtX)

  # tvalues
  t_vals <- as.vector(beta_hat / sqrt(diag(var_beta)))

  # pvalues, twotailed tdistribution
  p_vals <- 2 * stats::pt(-abs(t_vals), df = df)

  # build S3 object
  result <- list(
    call = match.call(),
    beta = as.vector(beta_hat),
    beta_names = colnames(X),
    fitted = as.vector(y_hat),
    residuals = as.vector(e_hat),
    df = df,
    sigma2 = sigma2_hat,
    var_beta = var_beta,
    t_values = t_vals,
    p_values = p_vals,
    formula = formula,
    data = data
  )
  class(result) <- "linreg"
  return(result)
}


#' Print method for linreg S3 class
#'
#' @param x object of class linreg
#' @param ... further arguments
#' @export
print.linreg <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  coef_vec <- stats::setNames(x$beta, x$beta_names)
  print(coef_vec)
}

#' coef method for linreg
#'
#' @param object object of class linreg
#' @param ... further arguments
#' @export
coef.linreg <- function(object, ...){
  stats::setNames(object$beta, object$beta_names)
}

#' residuals method for linreg
#'
#' @param object object of class linreg
#' @param ... further arguments
#' @export
residuals.linreg <- function(object, ...){
  object$residuals
}

#' pred Generic for linreg objects
#' @param object object
#' @param ... other arguments
#' @export
pred <- function(object, ...) UseMethod("pred")

#' pred method for linreg (coursespecific, not predict())
#'
#' @param object object of class linreg
#' @param ... further arguments
#' @export
pred.linreg <- function(object, ...){
  object$fitted
}

#' Summary method for linreg
#'
#' @param object object of class linreg
#' @param ... further arguments
#' @export
summary.linreg <- function(object, ...){
  se_beta <- sqrt(diag(object$var_beta))
  coef_table <- data.frame(
    Estimate = object$beta,
    StdErr = se_beta,
    t_value = object$t_values,
    p_value = object$p_values
  )
  rownames(coef_table) <- object$beta_names

  out <- list(
    coefficients = coef_table,
    sigma_hat = sqrt(object$sigma2),
    df = object$df,
    call = object$call
  )
  class(out) <- "summary.linreg"
  print(out)
  return(invisible(out))
}

#' Print summary.linreg
#' @param x summary.linreg object
#' @param ... further args
#' @export
print.summary.linreg <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
  cat("\nResidual standard error: ", x$sigma_hat, " on ", x$df, " degrees of freedom\n", sep="")
}


#' Plot method for linreg: two ggplot figures
#'
#' Produces Residuals vs Fitted and Scale Location plots
#' @param x linreg object
#' @param ... further arguments
#' @export
plot.linreg <- function(x, ...){
  fitted <- residual <- std_resid <- NULL
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plot.linreg")
  }
  df_plot <- data.frame(
    fitted = x$fitted,
    residual = x$residuals,
    std_resid = x$residuals / sqrt(x$sigma2)
  )

  p1 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = residual)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::ggtitle("Residuals vs Fitted") +
    ggplot2::xlab("Fitted values") +
    ggplot2::ylab("Residuals")

  p2 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = sqrt(abs(std_resid)))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::ggtitle("ScaleLocation") +
    ggplot2::xlab("Fitted values") +
    ggplot2::ylab(expression(sqrt("|Standardized residuals|")))

  print(p1)
  readline(prompt="Press enter to see next plot...")
  print(p2)

  #' @export
  print.summary.linreg <- function(x, ...){
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(format(x$coefficients, digits=5))
    cat("\nResidual standard error: ", round(x$sigma_hat,5), " on ", x$df, " degrees of freedom\n", sep="")
  }
}
