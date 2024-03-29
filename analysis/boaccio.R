x <- c(0.6114, 1.497, 3.4051)
x <- log(x)
qnorm(0.95)
(se1 <- (x[3] - x[2]) / qnorm(0.95))
(se2 <- (x[2] - x[1]) / qnorm(0.95))
y <- rnorm(1e6, mean = x[2], sd = mean(c(se1, se2)))
quantile(exp(y), probs = c(0.05, 0.95))
plot(density(exp(y)))
