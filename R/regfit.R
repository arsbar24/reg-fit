#' Fits a polynomial of specified degree to the data with a regularization term
#'
#' @param y Dependent variable
#' @param x Independent variable(s) in array form
#' @param degree Degree of polynomial to fit
#' @param lambda Weight of regularization term (may be selected using crossval)
#'
#' @return List with the coefficients for each term in the polynomial and the error.
#'
#' @details
#' Finds the polynomial of specified degree that minimizes
#'
#' \code{({y}-p({x}))^2+lambda*sum(coeff^2)}
#'
#'
#' Where \code{p} is the polynomial and \code{coeff} is its coefficients.
#'
#' This function is useful when concerns about overfitting are present. \code{lambda} may be chosen by the \code{crossval} function.
#'
#' @examples
#' reg.lm((1:10)^3, 1:10, degree = 3, lambda = 1)
#' @export


reg.lm <- function(y, x, degree = 1, lambda = 1){
	if(as.integer(degree) != degree){
		stop("Degree of polynomial must be an integer")
	}
	if(is.vector(y) == FALSE || is.vector(x) == FALSE){
		stop("Data must be in vector form")
	}
	if(length(y) != length(x)){
		stop("Dependent and independent observations must be same length")
	}
	if(is.numeric(lambda) == FALSE){
		stop("lambda must be numeric.")
	}
	x <- as.matrix(x)
	Y <- as.matrix(y)
	X <- NULL
	for(i in 0:degree){
		X <- cbind(X,x^i) # build polynomial
	}
	# minimize error
	fit <- solve(t(X) %*% X + lambda*diag(ncol(X))) %*% t(X) %*% Y
	error <- sum((Y - X %*% fit)^2) + lambda * sum(fit^2)
	list(coefficients = fit, error = error)
}
