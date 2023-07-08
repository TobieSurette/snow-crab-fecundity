# Load fecundity data here:
data <- read.csv(, header = TRUE, stringsAsFactors = FALSE)

# Extract predictor and response variables:
x <- data$carapace.width
y <- data$fecundity

# Define initial parameter values:
theta <- c(log.alpha = -3, beta = 3, log.sigma = -1)

# Define log-likelihood function for an allometric model:
loglike <- function(theta, x, y){
   sigma <- exp(theta["log.sigma"])
   mu <- theta["log.alpha"] + theta["beta"] * log(x)
   v <- dnorm(log(y), mu, sigma, log = TRUE)

   return(-sum(v))
}

loglike(theta, x, y)  # Should return some number.

# Perform fitting:
res <- optim(theta, loglike, x = data$carapace.width, data$fecundity, control = list(trace = 3))

# Update parameter vector:
theta <- res$par

# Plot results:
plot(log(data$carapace.width), data$fecundity, pch = 21, bg = "grey")
x <- seq(-10, 10, len = 1000)
mu <- theta["log.alpha"] + theta["beta"] * log(x)
lines(x, mu, col = "red", lwd = 2)
