
# Model parameters:
P.loss <- c(0.6424, 0.3576)
P.mat <- c(0.3532, 0.6468)
P.out <- c(0.9153, 0.0847)
a.loss <- 4.706
b.loss <- 1.496
beta <- 2.869
log.alpha <- c(-1.023, -1.383)
sigma.obs <- 0.1071
sigma.out <- 0.3159


# Plot model density distribution:
xx <- 70
yy <- seq(9, 11.75, len = 1000)
d1 <- P.mat[1] * dnorm(yy, mean = beta * log(xx) + log.alpha[1], sd = sigma.obs)
d1.out <- P.mat[1] * dnorm(yy, mean = beta * log(xx) + log.alpha[1], sd = sigma.obs + sigma.out)
d2 <- P.mat[2] * dnorm(yy, mean = beta * log(xx) + log.alpha[2], sd = sigma.obs)
d2.out <- P.mat[2] * dnorm(yy, mean = beta * log(xx) + log.alpha[2], sd = sigma.obs + sigma.out)

d1 <- P.out[1] * d1 + P.out[2] * d1.out
d2 <- P.out[1] * d2 + P.out[2] * d2.out


plot(c(10000, 110000), c(0, 2.5), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
lines(exp(yy), d1 + d2, lwd = 2, col = fade("grey30"))
lines(exp(yy), d1, col = fade("olivedrab2", 0.80))
polygon(exp(yy), c(0, d1[2:(length(d1)-1)], 0), col = fade("olivedrab2"), border = NA)
lines(exp(yy), d2, col = fade("indianred1", 0.80))
polygon(exp(yy), c(0, d2[2:(length(d2)-1)], 0), col = fade("indianred1"), border = NA)
lines(rep(exp(yy)[which.max(d1)],2), c(0, max(d1)), col = "olivedrab2", lwd = 2)
lines(rep(exp(yy)[which.max(d2)],2), c(0, max(d2)), col = "indianred1", lwd = 2)
xscale <- seq(10000, 150000, by = 10000)
axis(1, at = xscale[seq(1, length(xscale), by = 2)], labels = xscale[seq(1, length(xscale), by = 2)] / 1000)
axis(1, at = xscale[seq(2, length(xscale), by = 2)], labels = xscale[seq(2, length(xscale), by = 2)] / 1000)
mtext("Probability density", 2, 2.75, cex = 1.25, font = 2, col = "grey30") 
mtext("Fecundity (thousand eggs)", 1, 2.75, cex = 1.25, font = 2, col = "grey30")  
box(col = "grey50")

log.y <- seq(-1, 1, len = 1000)
yp = -0.1434
m = 9.04E-4
p <- 1 - (1 / (1 + exp(-(log.y - yp)/m)))
plot(c(-0.75, 0.5), c(0,1), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
lines(log.y, p, col = "red", lwd = 2)
vline(yp, lwd = 1, lty = "dashed", col = "red")
mtext()
box(col = "grey50")

