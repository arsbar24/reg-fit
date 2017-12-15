context("Comparing reg.lm with lm")

test_that("Does regularized match linear model when lambda = 0", {
	x <- c(1, 5.4, 2.1)
	y <- c(0.2, -3, 7.1)
	reg <- reg.lm(y,x,lambda = 0)
	lm <- lm(y~x)
	expect_equal(as.numeric(lm$coefficients), as.numeric(reg$coefficients))
})

test_that("Are they the same when lambda != 0", {
	x <- c(1, 5.4, 2.1)
	y <- c(0.2, -3, 7.1)
	reg <- reg.lm(y,x,lambda = 0.1)
	lm <- lm(y~x)
	expect_false(as.numeric(lm$coefficients)[1]==as.numeric(reg$coefficients)[1])
	expect_false(as.numeric(lm$coefficients)[2]==as.numeric(reg$coefficients)[2])
})

test_that("Does fit have positive slope when data positively correlated?", {
	x <- 1:5
	y <- (1:5)^3
	reg <- reg.lm(y,x,lambda = 1)
	expect_true(reg$coefficients[2]>0)
})

