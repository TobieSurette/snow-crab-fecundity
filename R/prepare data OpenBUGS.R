library(gulf.data)
library(gulf.spatial)

options(max.print = 15000)

# Identify primparous and multiparous:
x$maturity <- NA
x$maturity[x$shell.condition %in% 1:2] <- 1
x$maturity[x$shell.condition %in% 4:5] <- 2

#x$eggs.remaining <- as.numeric(x$eggs.remaining)
x$yearf <- x$year - min(x$year) + 1
site.year <- unique(paste(x$year, x$location))
x$site  <- match(paste(x$year, x$location), unique(paste(x$year, x$location)))

clc()
paste0("n = ", nrow(x), ",")
paste0("x = c(", paste(round(x$carapace.width,1), collapse = ","), "),")
paste0("f = c(", paste(round(x$fecundity), collapse = ","), "),")
paste0("year = ", max(x$yearf), ",")
paste0("log.f = c(", paste(round(log(x$fecundity),3), collapse = ","), "),")
paste0("M = c(", paste(x$maturity, collapse = ","), "),")
paste0("SC = c(", paste(x$shell.condition, collapse = ","), "),")
paste0("SC45 = c(", paste((x$shell.condition %in% 4:5) + 1 - 1, collapse = ","), "),")
paste0("year = c(", paste(x$yearf, collapse = ","), "),")
paste0("site = c(", paste(x$site, collapse = ","), "),")

clc()
ix <- which(!is.na(y$gonad.weight))
lm(log(y$gonad.weight) ~ log(y$carapace.width))
paste(round(y$carapace.width[ix],1), collapse = ",")
paste(y$gonad.weight[ix], collapse = ",")
paste(round(log(y$gonad.weight),3), collapse = ",")


ix <- which(x$site == 31)
plot(log(x$carapace.width[ix]), log(x$fecundity[ix]), cex = 0.74)
points(log(x$carapace.width), log(x$fecundity), col = "grey50", cex = 0.25)
abline(-0.8581 + 0.01475, 2.75 , col  ="green2", lwd = 1, lty = "dashed")
abline(-0.5962 + 0.08157, 2.75, col  ="blue", lwd = 1, lty = "dashed")
abline(-0.8581, 2.75 , col  ="green2", lwd = 2)
abline(-0.5962, 2.75, col  ="blue", lwd = 2)

ix <- which(x$site == 32)
plot(log(x$carapace.width[ix]), log(x$fecundity[ix]), cex = 0.74)
points(log(x$carapace.width), log(x$fecundity), col = "grey50", cex = 0.25)
abline(-0.8581 - 0.1327, 2.75 , col  ="green2", lwd = 1, lty = "dashed")
abline(-0.5962 - 0.1935, 2.75, col  ="blue", lwd = 1, lty = "dashed")
abline(-0.8581, 2.75 , col  ="green2", lwd = 2)
abline(-0.5962, 2.75, col  ="blue", lwd = 2)

plot(log(x$carapace.width), log(x$fecundity), cex = 0.5, col = "grey50")
abline(-1.62, 2.929 , col  ="green2", lwd = 2)
abline(-0.1566	, 2.663, col  ="blue", lwd = 2)

site <- 33
ix <- which(x$site == site & x$shell.condition %in% 1:2)
points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 21, bg = "green2", cex = 0.74, col = "green3")
ix <- which(x$site == site & x$shell.condition %in% 3)
points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 21, bg = "red", cex = 0.74, col = "red")
ix <- which(x$site == site & x$shell.condition %in% 4:5)
points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 21, bg = "blue", cex = 0.74, col = "blue")



