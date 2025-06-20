
path <- "bugs/results/double beta/"

# Load model summary results:
loss        <- read.csv(paste0(path, "loss.indicator.csv"))
#loss.p      <- read.csv("bugs/loss.fraction.csv")
#loss.factor <- read.csv("bugs/loss.factor.csv")
outlier     <- read.csv(paste0(path, "outlier.csv"))
mat         <- read.csv(paste0(path, "maturity.csv"))
mat$ix      <- as.numeric(unlist(lapply(strsplit(gsub("]", "", mat$X), "[", fixed = TRUE), function(x) x[2])))
r           <- read.csv(paste0(path, "residuals.csv"))
m <- x$maturity
m[mat$ix] <- (mat$mean > 1.5) + 1

# Regression coeffcients:
beta      <- read.table(paste0(path, "beta.txt"), sep = "\t")
ix        <- 1:which(diff(beta[,1]) < 0)
iy        <- (which(diff(beta[,1]) < 0)+1):nrow(beta)
beta <- data.frame(primiparous = beta[ix,2], multiparous = beta[iy,2])
log.alpha <- read.table(paste0(path, "log.alpha.txt"), sep = "\t")
ix        <- 1:which(diff(log.alpha[,1]) < 0)
iy        <- (which(diff(log.alpha[,1]) < 0)+1):nrow(log.alpha)
log.alpha <- data.frame(primiparous = log.alpha[ix,2], multiparous = log.alpha[iy,2])
#sigma.obs <- read.table("bugs/sigma.obs.txt", sep = "\t")[,2]

png(file = "figures/Residual by size.png", units = "in", width = 7, height = 9, res = 500)
ll <- rbind(0, cbind(0, kronecker(1:2, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(ll)
par(mar = c(0,0,0,0))
ix <- (m == 2) & (loss$mean < 1.5) & (outlier$mean < 1.5)
plot(c(38, 90), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
boxplot(r$mean[ix] ~ round(x$carapace.width[ix]), ylim = c(-0.25, 0.25), 
        at = as.numeric(names(table(round(x$carapace.width[ix])))), add = TRUE, xaxt = "n", border = "grey40", lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.21, "Multiparous", cex = 1.5, font = 2)
box(col = "grey50")
mtext("Residual", 2, 2.75, at = par("usr")[3], cex = 1.5, font = 2)
plot(c(38, 90), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
boxplot(r$mean[m == 1] ~ round(x$carapace.width[m == 1]), 
        at = as.numeric(names(table(round(x$carapace.width[m == 1])))), add = TRUE, xaxt = "n", border = "grey40", lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.21, "Primiparous", cex = 1.5, font = 2)

axis(1, at = seq(40, 100, by = 2))
mtext("Carapace width (mm)", 1, 3.0, cex = 1.5, font = 2)
box(col = "grey50")
dev.off()

clg()
#png(file = "figures/Residual histogram.png", units = "in", width = 7, height = 7, res = 500)
rm <- log(x$fecundity) - mean(beta$multiparous) * log(x$carapace.width) - mean(log.alpha$multiparous)
rp <- log(x$fecundity) - mean(beta$primiparous) * log(x$carapace.width) - mean(log.alpha$primiparous)
gbarplot(table(round(rm[(m == 1) & (outlier$mean < 1.5)]*50)/50), xlim = c(-1.61, 0.35), 
         col = fade("green3", 0.5), ylim = c(0, 400), xaxs = "i", xaxt = "n", lwd = 0.5)
gbarplot(table(round(rm[(m == 1) & (outlier$mean > 1.5)]*50)/50), col = fade("green3", 0.4), add = TRUE, lwd = 0.5)
gbarplot(table(round(rm[m == 2 & (outlier$mean < 1.5) & (loss$mean < 1.5)]*50)/50), add = TRUE, col = fade("blue", 0.4), lwd = 0.5)
gbarplot(table(round(rm[m == 2 & (outlier$mean < 1.5) & (loss$mean > 1.5)]*50)/50), add = TRUE, col = fade("purple", 0.4), lwd = 0.5)
lines(as.numeric(names(t)), t, lwd = 1.5, col = "grey30")
xlab = seq(-90, 50, by = 10)  
vline(log((xlab / 100) + 1), lty = "dashed", lwd = 0.5, col = "grey70")
hline(seq(0, 350, by = 50), lty = "dashed", lwd = 0.5, col = "grey70")
vline(0, col = "blue", lwd = 1.5)
vline(-diff(apply(log.alpha, 2, mean)), col = "green3", lwd = 1.5)
axis(1, at = log((xlab / 100) + 1)[seq(1, length(xlab), by = 2)], labels = xlab[seq(1, length(xlab), by = 2)])
axis(1, at = log((xlab / 100) + 1)[seq(2, length(xlab), by = 2)], labels = xlab[seq(2, length(xlab), by = 2)])
mtext("Fecundity difference from multiparous (%)", 1, 2.5, cex = 1.25, font = 2, col = "grey20")
mtext("Frequency", 2, 2.5, cex = 1.25, font = 2, col = "grey20")
legend("topleft", 
       legend = c("Primiparous", "Multiparous full clutch", "Multiparous with loss", "Combined"),
       pch = c(22, 22, 22, NA),
       pt.cex = c(2, 2, 2, NA),
       pt.bg = c(fade(c("green3", "blue", "purple")), NA),
       col = c("grey50"),
       lwd = c(NA,NA,NA,2),
       box.col = "grey50",
       )
box(col = "grey50")
#dev.off()

# Residuals by year:
png(file = "figures/Residuals by year.png", units = "in", width = 7, height = 7, res = 500)
boxplot(r$mean ~  x$year, xlab = "", ylab = "")
hline(0, lty = "dashed", col = fade("red", 0.75))
mtext("Location", 1, 2.75, font = 2, cex = 1.5)
mtext("Residual (log-scale)", 2, 2.75, font = 2, cex = 1.5)
dev.off()

# Residuals by year by location:
png(file = "figures/Residuals multiparous by year.png", units = "in", width = 8.5, height = 11, res = 500)
locations <- unique(x$location)[c(2,3,4,1)]
m <- rbind(0, cbind(0, kronecker(1:4, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(m)
ylim = c(-0.25, 0.25)
for (i in 1:length(locations)){
   par(mar = c(0,0,0,0))
   plot(range(x$year), ylim, type = "n", yaxs = "i", xaxt = "n")
   ix <- which(mat == 2 & outlier$mean < 1.5 & x$location == locations[i]) # & loss$mean < 1.5 )
   boxplot(r$mean[ix] ~  x$year[ix], xlab = "", ylab = "", ylim = ylim, xaxt = "n", yaxs = "i", at = sort(unique(x$year[ix])), add = TRUE)
   hline(0, lty = "dashed", col = fade("red"))
   if (i == 2) mtext("Residual (log-scale)", 2, 2.75, at = ylim[1], font = 2, cex = 1.25)
   mtext(locations[i], 4, 1.75, font = 2, cex = 1.25)
}
axis(1, at = seq(2001, max(x$year), by = 2))
axis(1, at = seq(2002, max(x$year), by = 2))
mtext("Location", 1, 3.25, font = 2, cex = 1.5)
dev.off()

# Residuals by location:
png(file = "figures/Residuals by location.png", units = "in", width = 7, height = 7, res = 500)
boxplot(r$mean ~  x$location, xlab = "", ylab = "")
hline(0, lty = "dashed", col = fade("red", 0.75))
mtext("Location", 1, 2.75, font = 2, cex = 1.5)
mtext("Residual (log-scale)", 2, 2.75, font = 2, cex = 1.5)
dev.off()

png(file = "figures/Residual primiparous without egg loss by location.png", units = "in", width = 7, height = 7, res = 500)
ix <- which(mat == 1 & outlier$mean < 1.5 & loss$mean < 1.5)
boxplot(r$mean[ix] ~  x$location[ix], xlab = "", ylab = "", ylim = ylim, yaxs = "i")
hline(0, lty = "dashed", col = fade("red"))
mtext("Location", 1, 2.75, font = 2, cex = 1.5)
mtext("Residual (log-scale)", 2, 2.75, font = 2, cex = 1.5)
mtext("Primiparous with egg loss", 3, 0.5, font = 2, cex = 1.5)
axis(1, at = 2, labels = sort(unique(x$location))[2])
box(col = "grey50")
dev.off()

png(file = "figures/Residual multiparous without egg loss by location.png", units = "in", width = 7, height = 7, res = 500)
ix <- which(mat == 2 & outlier$mean < 1.5 & loss$mean < 1.5)
boxplot(r$mean[ix] ~  x$location[ix], xlab = "", ylab = "", ylim = ylim, yaxs = "i")
hline(0, lty = "dashed", col = fade("red"))
mtext("Location", 1, 2.75, font = 2, cex = 1.5)
mtext("Residual (log-scale)", 2, 2.75, font = 2, cex = 1.5)
mtext("Multiparous with egg loss", 3, 0.5, font = 2, cex = 1.5)
axis(1, at = 2, labels = sort(unique(x$location))[2])
box(col = "grey50")
dev.off()

png(file = "figures/Residual multiparous with egg loss by location.png", units = "in", width = 7, height = 7, res = 500)
ix <- which(mat == 2 & outlier$mean < 1.5 & loss$mean > 1.5)
boxplot(r$mean[ix] ~  x$location[ix], xlab = "", ylab = "", ylim = ylim, yaxs = "i")
hline(0, lty = "dashed", col = fade("red"))
mtext("Location", 1, 2.75, font = 2, cex = 1.5)
mtext("Residual (log-scale)", 2, 2.75, font = 2, cex = 1.5)
mtext("Multiparous with egg loss", 3, 0.5, font = 2, cex = 1.5)
axis(1, at = 2, labels = sort(unique(x$location))[2])
box(col = "grey50")
dev.off()

png(file = "figures/Residual primiparous by month.png", units = "in", width = 7, height = 7, res = 500)
ix <- which(mat == 1 & outlier$mean < 1.5 & loss$mean < 1.5)
boxplot(r$mean[ix] ~  x$month[ix], xlab = "", ylab = "", ylim = ylim, yaxs = "i")
hline(0, lty = "dashed", col = fade("red"))
mtext("Month", 1, 2.75, font = 2, cex = 1.5)
mtext("Residual (log-scale)", 2, 2.75, font = 2, cex = 1.5)
mtext("Primiparous", 3, 0.5, font = 2, cex = 1.5)
box(col = "grey50")
dev.off()



