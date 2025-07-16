library(gulf.graphics)

path <- "bugs/results/"

# Beta coefficients:
beta      <- read.table(paste0(path, "betaCODAchain1.txt"), sep = "\t")
ix        <- 1:which(diff(beta[,1]) < 0)
iy        <- (which(diff(beta[,1]) < 0)+1):nrow(beta)
beta <- data.frame(primiparous = beta[ix,2], multiparous = beta[iy,2])

# Global alpha coefficients:
log.alpha <- read.table(paste0(path, "log.alphaCODAchain1.txt"), sep = "\t")
ix        <- 1:which(diff(log.alpha[,1]) < 0)
iy        <- (which(diff(log.alpha[,1]) < 0)+1):nrow(log.alpha)
log.alpha <- data.frame(primiparous = log.alpha[ix,2], multiparous = log.alpha[iy,2])

xx <- seq(40, 90, by = 1)
p <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
r <- NULL
for (i in 1:length(xx)){
   t <- (exp(log.alpha$multiparous - log.alpha$primiparous) * xx[i] ^ (beta$multiparous - beta$primiparous))^(1/beta$multiparous)
   r <- rbind(r, quantile(t, p = p))
}
rownames(r) <- xx

png(file = "figures/Growth from fecundity allometry.png", width = 7, height = 7, res = 600, units = "in")

plot(c(40, 80), c(0, 25), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
polygon(c(xx, rev(xx)), 100 * (c(r[, "2.5%"], rev(r[, "97.5%"]))-1), col = fade("grey50"), border = "grey50", lwd = 0.5)
lines(xx, 100 * (r[, "50%"] - 1), lwd = 2)
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Growth increment (%)", 2, 2.5, cex = 1.25, font = 2)

b <- read.csv("data/raw/female moult data lab.csv")
points(b$Before[b$Moult == "M"], 100 * (b$After[b$Moult == "M"] - b$Before[b$Moult == "M"]) / b$Before[b$Moult == "M"], pch = 21, cex = 0.85, bg = fade("red", 0.5), lwd = 0.5, col = NA)

# Alunno-Bruscia (1998):
a = 1.071 
b = 5.099
xxx <- seq(45, 70, len = 1000)
lines(xxx, 100 * (a - 1 + b / xxx), lwd = 2, col = "green3")

xxx <- seq(40, 80, len = 1000)
px <- (exp(mean(log.alpha$multiparous - log.alpha$primiparous)) * xxx ^ (mean(beta$multiparous) - mean(beta$primiparous)))^(1/mean(beta$multiparous))
model <- lm(I(px * xxx) ~ xxx)
lines(xxx, 100 * (coef(model)[2] + coef(model)[1] / xxx - 1), lwd = 2, col = "grey30", lty = "dashed")

legend("topright",
       lwd = c(2, 2, 2, NA), 
       cex = 1.25,
       pch = c(NA, NA, NA, 21),
       lty = c("solid", "dashed", "solid", NA),
       legend = c("Fecundity allometry", "Hiatt approximation", "Alunno-Bruscia (1998)", "Mikio lab data"),
       col = c("black", "grey30", "green3", fade("red")), 
       pt.bg = c(NA, NA, NA, fade("red")), box.lwd = 0.5, box.col = "grey50")

box(col = "grey50")
dev.off()

# Plot showing pre-moult primiparous fecundity:
ix <- x$shell.condition %in% 4:5
plot(x$carapace.width[ix], x$fecundity[ix], xlim = c(35, 75), xaxs = "i", ylim = c(0, 90000), yaxs = "i", cex = 0.35)
ix <- x$shell.condition %in% 1:2
px <- 1.7724 * x$carapace.width[ix] ^ (-0.105)
points(x$carapace.width[ix], x$fecundity[ix], pch = 21, bg = fade("red"), col = fade("red"), cex = 0.5)
points(x$carapace.width[ix]/px, x$fecundity[ix], pch = 21, bg = fade("green2"), col = fade("green2"), cex = 0.5)


