#' Uses cross-validation to find a good weighting of regularization
#'
#' @param y Dependent variable
#' @param x Independent variable(s) in array form
#' @param partitions Number of partitions to divide the data into (n for n-fold validation)
#' @param degree Degree of polynomial to fit
#' @param lambda Vector of lambda weightings to try
#'
#' @return A list of the the best \code{lambda} value, and its fit (includes coefficients and error).
#'
#' @details
#' Splits the data into the number of partitions specified then attempts a fit on each partition, and calculates the error of the fit on the remaining data.
#'
#' @examples
#' crossval((1:10)^3, 1:10, partitions = 20, degree = 3, lambda = c(0.001, 0.01, 0.1, 1, 10))
#' @export


crossval <- function(y, x, partitions = 5, degree = 1, lambda = c(0.1, 1, 10)){
	if(as.integer(partitions) != partitions){
		stop("Number of partitions must be an integer")
	}
	if(is.vector(y) == FALSE || is.vector(x) == FALSE){
		stop("Data must be in vector form")
	}
	if(length(y) != length(x)){
		stop("Dependent and independent observations must be same length")
	}
	if(is.numeric(lambda) == FALSE||lambda<0){
		stop("lambda must be numeric value >= 0.")
	}
	if(length(lambda) == 1){
		stop("Not much point doing this if you're just testing a single lambda value.")
	}
	N <- length(x)
	n <- floor(N/partitions) # length of each partition
	error <- rep(0, length(lambda)) # initialize
	for(i in 1:partitions){
		indices <- ((i-1)*n+1):(i*n) # define partition
		X <- x[indices]
		Y <- y[indices]
		Xtest <- NULL
		for(d in 0:degree){
			Xtest <- cbind(Xtest, x[-indices]^d) # define polynomial for test data
		}
		Ytest <- y[-indices]
		for(j in 1:length(lambda)){
			# calculate fits and error for different lambda values
			fit <- reg.lm(Y, X, degree = degree, lambda = lambda[j])
			fit <- fit$coefficients
			error[j] <- error[j] +
				sum((Ytest - Xtest %*% fit)^2)
		}
	}
	j <- which.min(error) # finds index with lowest error
	best.fit <- reg.lm(y,x, degree = degree, lambda = lambda[j])
	list(lambda = lambda[j], coefficients = best.fit$coefficients, error = best.fit$error)
}
