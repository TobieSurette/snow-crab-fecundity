
plot(log(z$carapace.width), log(z$egg.total.number), cex = 0.5)
grid()
ix <- which(as.numeric(substr(z$shell.condition,1,1)) == 2)
points(log(z$carapace.width[ix]), log(z$egg.total.number[ix]), pch = 21, bg = "orange", cex = 0.75, col = "grey60", lwd = 0.5)
ix <- which(as.numeric(substr(z$shell.condition,1,1)) == 3)
points(log(z$carapace.width[ix]), log(z$egg.total.number[ix]), pch = 21, bg = "red", cex = 0.75, col = "grey60", lwd = 0.5)
ix <- which(as.numeric(substr(z$shell.condition,1,1)) == 4)
points(log(z$carapace.width[ix]), log(z$egg.total.number[ix]), pch = 21, bg = "blue", cex = 0.75, col = "grey60", lwd = 0.5)

mtext("Carapace width (mm)", 1, 2.5, cex = 1.25)
mtext("Fecundity", 2, 2.5, cex = 1.25)
legend("topleft", 
       legend = c("SC2", "SC3", "SC4"),
       pch = 21:23,
       pt.bg = c("orange", "red3", "blue"),
       cex = 1.5 )
box(col = "grey60")


x$fecundity <- (x$egg.count.sample / x$egg.weight.sample) * x$egg.weight


plot(x$carapace.width, x$fecundity)

ix <- x$gonad.colour == "orange"
points(x$carapace.width[ix], x$fecundity[ix], pch = 21, bg = "orange")

ix <- x$gonad.colour == "beige"
points(x$carapace.width[ix], x$fecundity[ix], pch = 21, bg = "green3", cex = 2)
   
ix <- grep("par", x$comments.cleaning)
points(x$carapace.width[ix], x$fecundity[ix], pch = 21, bg = "blue", cex = 1.5)


plot(x$carapace.width, x$fecundity, xlab = "", ylab = "")
grid()
ix <- x$shell.condition == 2
points(x$carapace.width[ix], x$fecundity[ix], pch = 21, bg = "orange", cex = 1.25)
ix <- x$shell.condition == 3
points(x$carapace.width[ix], x$fecundity[ix], pch = 22, bg = "red3", cex = 1.5)
ix <- x$shell.condition == 4
points(x$carapace.width[ix], x$fecundity[ix], pch = 23, bg = "blue", cex = 1.5)
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25)
mtext("Fecundity", 2, 2.5, cex = 1.25)
legend("topleft", 
       legend = c("SC2", "SC3", "SC4"),
       pch = 21:23,
       pt.bg = c("orange", "red3", "blue"),
       cex = 1.5 )
box(col = "grey60")

# Egg development:
sc <- 3
ix <- which((as.numeric(substr(x$egg.development, 1, 1)) >= 8) & (x$shell.condition == sc))
iy <- which((as.numeric(substr(x$egg.development, 1, 1)) < 8) & (x$shell.condition == sc))
plot(x$carapace.width, x$fecundity, xlab = "", ylab = "")
grid()
points(x$carapace.width[ix], x$fecundity[ix], pch = 21, bg = "orange", cex = 1.25)
points(x$carapace.width[iy], x$fecundity[iy], pch = 21, bg = "green", cex = 1.25)
legend("topleft", 
       legend = c(paste0("SC", sc, " & 1st Year"), paste0("SC", sc, " & 2nd Year")),
       pch = 21,
       pt.bg = c("orange", "green"),
       cex = 1.5 )
box(col = "grey60")

# Map of 2nd year cycle locations:
sc <- 4
s <- read.scsset(2019, valid = 1, survey = "regular")
ix <- match(x$tow.id, s$tow.id)
x$longitude <- lon(s)[ix]
x$latitude <- lat(s)[ix]
ix <- which((as.numeric(substr(x$egg.development, 1, 1)) >= 8) & (x$shell.condition == sc))
iy <- which((as.numeric(substr(x$egg.development, 1, 1)) < 8) & (x$shell.condition == sc))
map.new()
points(jitter(x$longitude[ix], amount = 0.1), x$latitude[ix], pch = 21, bg = "orange", cex = 1.25)
points(jitter(x$longitude[iy], amount = 0.1), x$latitude[iy], pch = 21, bg = "green", cex = 1.25)
grid()
map("coast")
legend("topright", 
       legend = c(paste0("SC", sc, " & 1st Year"), paste0("SC", sc, " & 2nd Year")),
       pch = 21,
       pt.bg = c("orange", "green"),
       cex = 1.5 )
box(col = "grey60")


# Perform mixed regression:
loglike <- function(theta, x, y, fixed){
   ix <- !is.na(x) & !is.na(y)
   x <- x[ix]; y <- y[ix]
   
   # Parse parameter vector:
   if (!missing(fixed)) theta <- c(theta, fixed)
   log.alpha <- theta[grep("log.alpha", names(theta))]
   log.sigma <- theta[grep("log.sigma", names(theta))]
   beta <- theta[grep("beta", names(theta))]
   logit.p <- theta[grep("logit.p", names(theta))]
   p <- 1 / (1 + exp(-logit.p))
   
   mu <- cbind(log.alpha[1] + beta*log(x), log.alpha[2] + beta*log(x))
   v <- log(p * dnorm(y, mu[,1], exp(log.sigma)) + (1-p) * dnorm(y, mu[,2], exp(log.sigma)))
   
   return(-sum(v))
}
theta <- c(log.alpha = c(-1.53, -1.92), beta = 3, logit.p = 1, log.sigma = -1)
fixed <- theta[c(grep("log.alpha", names(theta)), grep("beta", names(theta)))]
theta <- theta[setdiff(names(theta), names(fixed))]
loglike(theta, log(x$carapace.width), log(x$fecundity), fixed)
optim(theta, loglike, x = log(x$carapace.width), y = log(x$fecundity), fixed = fixed, control = list(trace = 3))

plot(log(x$carapace.width), log(x$fecundity), xlab = "", ylab = "")
abline(-1.53, 3)
abline(-1.92, 3)

r <- log(x$fecundity) - (-1.53 + 3 * log(x$carapace.width))

# Perform fecundity classification:
scatter3d(x = x$egg.colour.a, y = x$egg.colour.b, z = x$egg.colour.L, 
          groups = as.factor(x$shell.condition), surface = FALSE, 
          surface.col = c("yellow", "blue", "red", "green", 'orange'), radius = 2)



