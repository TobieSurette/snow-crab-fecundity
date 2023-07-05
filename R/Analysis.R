library(gulf.utils)
library(gulf.graphics)
library(rjags)

# Read biological data:
x <- read.csv("data/biological.csv", header = TRUE, stringsAsFactors = FALSE)

index <- which((log(x$carapace.width) <= 4.2) & (log(x$carapace.width) > 4.1))
gbarplot(table(round(log(x$gonad.weight[index])*10)/10), width = 0.1)

# Define initial parameter vector:
theta = c(intercept.primiparous = -6,
          intercept.multiparous = -5.5,
          slope.primiparous = 3,
          slope.multiparous = 3,
          log.sigma = -1,
          log.sigma.outlier = 1,
          logit.proportion = 0,
          logit.outlier = -4)

# Define log-likelihood function for regression mixture:
loglike <- function(theta, x, y){
   # Define mixture proportions:
   p.primiparous <- exp(theta[["logit.proportion"]]) / (1 + exp(theta[["logit.proportion"]]))
   p.multiparous <- 1 / (1 + exp(theta[["logit.proportion"]]))
   p.outlier <- exp(theta[["logit.outlier"]]) / (1 + exp(theta[["logit.outlier"]]))   
   
   # Regression means:
   mu.primiparous <- theta["intercept.primiparous"] + theta["slope.primiparous"] * x
   mu.multiparous <- theta["intercept.multiparous"] + theta["slope.multiparous"] * x
   
   # Regression error:
   sigma <- theta[["log.sigma"]]
   sigma.outlier <- theta[["log.sigma.outlier"]]
   
   (p.primiparous * dnorm(y, mu.primiparous, sigma) + 
    p.multiparous * dnorm(y, mu.multiparous, sigma)) 
   
   
   dnorm(y, mu.multiparous, sigma)
   
   
}
   
# Fecundity plot:
plot(log(x$carapace.width), log(x$egg.total.number), 
     cex = 0.25, pch = 21, bg = "grey", col = "grey60",
     xlim = c(3.7, 4.5), ylim = c(9.0, 12), 
     xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
mtext("ln(carapace width)", 1, 2.5, cex = 1.25)
mtext("ln(fecundity)", 2, 2.5, cex = 1.25)

# Fecundity plot:
plot(log(x$carapace.width), log(x$gonad.weight), 
     cex = 0.35, pch = 21, bg = "grey", col = "grey60",
     xlim = c(3.8, 4.5), ylim = c(-0.5, 3), 
     xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
mtext("ln(carapace width)", 1, 2.5, cex = 1.25)
mtext("ln(gonad weight)", 2, 2.5, cex = 1.25)
months <- 5:10
for (i in 1:length(months)){
   index <- x$month == months[i]
   points(log(x$carapace.width)[index], log(x$gonad.weight)[index], 
          pch = 21, cex = 1, bg = rainbow(length(months))[i])
}

plot(log(jitter(x$egg.sample.number, amount = 10)), 
     log(x$egg.sample.weight), cex = 0.25)

index <- which(x$egg.sample.number == 1000)
index <- x$year == 2013
plot(log(x$carapace.width), log(x$egg.total.number), 
     pch = 21, cex = 0.25, ylim = c(9, 12), xlim = c(3.8, 4.5))
points(log(x$carapace.width[index]), log(x$egg.total.number[index]), 
       pch = 21, bg = "red", cex = 0.75)
points(log(x$carapace.width[index]), log(n[index]), 
       pch = 21, bg = "green", cex = 0.75)


index <- which((x$year == 1998)  & (x$gear != "cage"))
plot(log(x$carapace.width), log(x$egg.total.weight), 
     pch = 21, cex = 0.25, ylim = c(-1, 2), xlim = c(3.8, 4.5))
points(log(x$carapace.width[index]), log(x$egg.total.weight[index]), 
       pch = 21, bg = "green", cex = 0.75)

points(log(x$carapace.width[index]), log(n[index]), 
       pch = 21, bg = "green", cex = 0.75)

ew <- x$egg.sample.weight / x$egg.sample.number
n <- x$egg.total.weight / ew

plot(x$egg.total.number, pch = 21, cex = 0.25)

index <- x$year == 2017
points((1:nrow(x))[index], x$egg.total.number[index], pch = 21, bg = "red", cex = 0.45)
   
points(1:nrow(x), n, pch = 21, bg = "red", cex = 0.45)

plog <- function(x){
   v <- rep(NA, length(x))
   v[which(x < 0)] <- log(-x[which(x < 0)])
   v[which(x > 0)] <- log(x[which(x > 0)])
   return(v)
}

plot(plog(x$egg.total.number - n), pch = 21, cex = 0.25)

plot(x$egg.total.number - n, pch = 21, cex = 0.25)

index <- which(abs(x$egg.total.number - n) > 1000)

