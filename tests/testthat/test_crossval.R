context("Cross validation")

test_that("Is it independent of order of lambda?", {
	x <- c(1, 5.4, 2.1)
	y <- c(0.2, -3, 7.1)
	lambda1 <- c(1, 0.1, 10)
	lambda2 <- c(0.1, 10, 1)
	cv1 <- crossval(y,x,lambda = lambda1)
	cv2 <- crossval(y,x,lambda = lambda1)
	expect_identical(cv1$lambda, cv2$lambda)
})

test_that("Does the error from crossval match reg.lm error?", {
	x <- c(1, 5.4, 2.1)
	y <- c(0.2, -3, 7.1)
	cv <- crossval(y,x)
	error.cv <- cv$error
	reg <- reg.lm(y, x, lambda = cv$lambda)
	error.reg <- reg$error
	expect_identical(error.reg, error.cv)
})

test_that("Does crossval give you lambda = 0 when data outside model?", {
	x <- 1:10
	y <- (1:10)^2
	cv <- crossval(y, x, degree = 1, lambda = c(0, 10^(-6),10^(-2),1))
	expect_false(cv$lambda==0)
})

test_that("Does crossval give you lambda = 0 when data inside model?", {
	x <- 1:30
	y <- (1:30)^2
	cv <- crossval(y, x, degree = 2, lambda = c(0, 10^(-6),10^(-2),1))
	expect_identical(cv$lambda, 0)
})

