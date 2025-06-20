# Load model summary results:
loss        <- read.csv("bugs/loss.indicator.csv")
loss.p      <- read.csv("bugs/loss.fraction.csv")
loss.factor <- read.csv("bugs/loss.factor.csv")
outlier     <- read.csv("bugs/outlier.csv")
mat         <- read.csv("bugs/maturity.csv")
mat$ix      <- as.numeric(unlist(lapply(strsplit(gsub("]", "", mat$X), "[", fixed = TRUE), function(x) x[2])))
r           <- read.csv("bugs/residuals.csv")

# Regression coeffcients:
beta      <- read.table("bugs/beta.txt", sep = "\t")[,2]
log.alpha <- read.table("bugs/log.alpha.txt", sep = "\t")
ix        <- 1:which(diff(log.alpha[,1]) < 0)
iy        <- (which(diff(log.alpha[,1]) < 0)+1):nrow(log.alpha)
log.alpha <- data.frame(primiparous = log.alpha[ix,2], multiparous = log.alpha[iy,2])
sigma.obs <- read.table("bugs/sigma.obs.txt", sep = "\t")[,2]

# Loss factor:
ix <- as.numeric(unlist(lapply(strsplit(gsub("]", "", loss.factor$X), "[", fixed = TRUE), function(x) x[2])))
rm <- log(x$fecundity) - (mean(beta) * log(x$carapace.width) + mean(log.alpha$multiparous))
png(file = "figures/Loss expected versus residual.png", units = "in", width = 7, height = 7, res = 500)
plot(c(-2.303, 0.40), c(0, 1), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
xlab = seq(-90, 50, by = 10)  
vline(log((xlab / 100) + 1), lty = "dashed", lwd = 0.5, col = "grey70")
hline(seq(0, 1, by = 0.1), lty = "dashed", lwd = 0.5, col = "grey70")
points(rm[ix], loss.factor$mean, pch = 21, bg = fade("grey30",0.5), col = NA, cex = 0.25)
vline(0, col = "red", lwd = 1.5)
axis(1, at = log((xlab / 100) + 1)[seq(1, length(xlab), by = 2)], labels = xlab[seq(1, length(xlab), by = 2)])
axis(1, at = log((xlab / 100) + 1)[seq(2, length(xlab), by = 2)], labels = xlab[seq(2, length(xlab), by = 2)])
axis(2, at = seq(0, 1, by = 0.1), labels = 100 * seq(0, 1, by = 0.1))
mtext("Fecundity difference from multiparous (%)", 1, 2.5, cex = 1.25, font = 2, col = "grey20")
mtext("Expected loss (%)", 2, 2.5, cex = 1.25, font = 2, col = "grey20")

# Annotations:
text(log(-29/100 + 1), 0.68, "Maximum loss", pos = 3, col = "grey20")
text(log(-65/100 + 1), 0.40, "Classified as outliers", pos = 3, srt = 38, col = "grey20")
text(log(8/100 + 1), 0.06, "Minimum egg loss", pos = 1, srt = 0, col = "grey20")
box(col = "grey50")
dev.off()

# Loss probability:
ix <- as.numeric(unlist(lapply(strsplit(gsub("]", "", loss.factor$X), "[", fixed = TRUE), function(x) x[2])))
rm <- log(x$fecundity) - (mean(beta) * log(x$carapace.width) + mean(log.alpha$multiparous))
png(file = "figures/Loss probability versus residual.png", units = "in", width = 7, height = 7, res = 500)
plot(c(-1.610, 0.40), c(0, 1), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
xlab = seq(-90, 50, by = 10)  
vline(log((xlab / 100) + 1), lty = "dashed", lwd = 0.5, col = "grey70")
hline(seq(0, 1, by = 0.1), lty = "dashed", lwd = 0.5, col = "grey70")
points(rm[ix], loss$mean[ix]-1, pch = 21, bg = fade("grey30",0.5), col = NA, cex = 0.25)
vline(0, col = fade("red", 0.75), lwd = 1.5)
mtext("Multiparous\nmean fecundity", 3, at = 0, col = fade("red", 0.75), cex = 0.75, font = 2)
vline(-diff(apply(log.alpha, 2, mean)), col = fade("green3", 0.75), lty = "dashed", lwd = 1.5)
mtext("Primiparous\nmean fecundity", 3, at = -diff(apply(log.alpha, 2, mean)), col = fade("green3", 0.75), cex = 0.75, font = 2)

axis(1, at = log((xlab / 100) + 1)[seq(1, length(xlab), by = 2)], labels = xlab[seq(1, length(xlab), by = 2)])
axis(1, at = log((xlab / 100) + 1)[seq(2, length(xlab), by = 2)], labels = xlab[seq(2, length(xlab), by = 2)])
axis(2, at = seq(0, 1, by = 0.1), labels = 100 * seq(0, 1, by = 0.1))
mtext("Fecundity difference from multiparous (%)", 1, 2.5, cex = 1.25, font = 2, col = "grey20")
mtext("Loss probability (%)", 2, 2.5, cex = 1.25, font = 2, col = "grey20")

# Annotations:
#text(log(-29/100 + 1), 0.68, "Maximum loss", pos = 3, col = "grey20")
#text(log(-65/100 + 1), 0.40, "Classified as outliers", pos = 3, srt = 38, col = "grey20")
#text(log(8/100 + 1), 0.06, "Minimum egg loss", pos = 1, srt = 0, col = "grey20")

box(col = "grey50")
dev.off()

# Loss fraction:
ix <- as.numeric(unlist(lapply(strsplit(gsub("]", "", loss.factor$X), "[", fixed = TRUE), function(x) x[2])))
rm <- log(x$fecundity) - (mean(beta) * log(x$carapace.width) + mean(log.alpha$multiparous))
png(file = "figures/Loss fraction versus residual.png", units = "in", width = 7, height = 7, res = 500)
plot(c(-1.610, 0.40), c(0, 1), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
xlab = seq(-90, 50, by = 10)  
vline(log((xlab / 100) + 1), lty = "dashed", lwd = 0.5, col = "grey70")
hline(seq(0, 1, by = 0.1), lty = "dashed", lwd = 0.5, col = "grey70")
points(rm[ix], loss.p$mean[ix], pch = 21, bg = fade("grey30",0.5), col = NA, cex = 0.25)
vline(0, col = "red", lwd = 1.5)
axis(1, at = log((xlab / 100) + 1)[seq(1, length(xlab), by = 2)], labels = xlab[seq(1, length(xlab), by = 2)])
axis(1, at = log((xlab / 100) + 1)[seq(2, length(xlab), by = 2)], labels = xlab[seq(2, length(xlab), by = 2)])
axis(2, at = seq(0, 1, by = 0.1), labels = 100 * seq(0, 1, by = 0.1))
mtext("Fecundity difference from multiparous (%)", 1, 2.5, cex = 1.25, font = 2, col = "grey20")
mtext("Loss fraction (%)", 2, 2.5, cex = 1.25, font = 2, col = "grey20")

# Annotations:
#text(log(-29/100 + 1), 0.68, "Maximum loss", pos = 3, col = "grey20")
#text(log(-65/100 + 1), 0.40, "Classified as outliers", pos = 3, srt = 38, col = "grey20")
#text(log(8/100 + 1), 0.06, "Minimum egg loss", pos = 1, srt = 0, col = "grey20")
box(col = "grey50")
dev.off()

# Senile fraction:
ix <- which(x$shell.condition %in% 4:5)
rm <- log(x$fecundity) - (mean(beta) * log(x$carapace.width) + mean(log.alpha$multiparous))
png(file = "figures/Senile versus residual.png", units = "in", width = 7, height = 7, res = 500)
plot(c(-1.610, 0.40), c(-0.05, 1.05), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
xlab = seq(-90, 50, by = 10)  
vline(log((xlab / 100) + 1), lty = "dashed", lwd = 0.5, col = "grey70")
hline(seq(0, 1, by = 0.1), lty = "dashed", lwd = 0.5, col = "grey70")
#points(rm[ix], jitter(as.numeric((x$egg.colour == "dark orange")[ix]), amount = 0.05), pch = 21, bg = fade("grey30",0.5), col = NA, cex = 0.25)
#tmp <- aggregate(x$egg.colour[ix] == "dark orange", by = list(x = round(rm[ix],1)), mean, na.rm = TRUE)
#lines(tmp[,1], tmp[,2], lwd = 2, col = "red")

points(rm[ix], jitter(as.numeric((x$shell.condition == 5)[ix]), amount = 0.05), pch = 21, bg = fade("grey30",0.5), col = NA, cex = 0.25)
tmp <- aggregate(x$shell.condition[ix] == 5, by = list(x = round(rm[ix],1)), mean, na.rm = TRUE)
lines(tmp[,1], tmp[,2], lwd = 2, col = "red")

vline(0, col = "red", lwd = 1.5)
axis(1, at = log((xlab / 100) + 1)[seq(1, length(xlab), by = 2)], labels = xlab[seq(1, length(xlab), by = 2)])
axis(1, at = log((xlab / 100) + 1)[seq(2, length(xlab), by = 2)], labels = xlab[seq(2, length(xlab), by = 2)])
axis(2, at = seq(0, 1, by = 0.1), labels = 100 * seq(0, 1, by = 0.1))
mtext("Fecundity difference from multiparous (%)", 1, 2.5, cex = 1.25, font = 2, col = "grey20")
mtext("Proportion", 2, 2.5, cex = 1.25, font = 2, col = "grey20")

# Annotations:
#text(log(-29/100 + 1), 0.68, "Maximum loss", pos = 3, col = "grey20")
#text(log(-65/100 + 1), 0.40, "Classified as outliers", pos = 3, srt = 38, col = "grey20")
#text(log(8/100 + 1), 0.06, "Minimum egg loss", pos = 1, srt = 0, col = "grey20")
box(col = "grey50")
dev.off()

# Colorimeter versus multiparous residual:
ix <- which(x$shell.condition %in% 4:5)
rm <- log(x$fecundity) - (mean(beta) * log(x$carapace.width) + mean(log.alpha$multiparous))
#png(file = "figures/Senile versus residual.png", units = "in", width = 7, height = 7, res = 500)
plot(c(-1.610, 0.40), c(-5, 70), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
xlab = seq(-90, 50, by = 10)  
vline(log((xlab / 100) + 1), lty = "dashed", lwd = 0.5, col = "grey70")
hline(seq(0, 1, by = 0.1), lty = "dashed", lwd = 0.5, col = "grey70")

points(rm[ix], x$egg.colour.L[ix], pch = 21, bg = fade("red",0.5), col = NA, cex = 0.5)
points(rm[ix], x$egg.colour.a[ix], pch = 21, bg = fade("blue",0.5), col = NA, cex = 0.5)
points(rm[ix], x$egg.colour.b[ix], pch = 21, bg = fade("green3",0.5), col = NA, cex = 0.75)
#tmp <- aggregate(x$shell.condition[ix] == 5, by = list(x = round(rm[ix],1)), mean, na.rm = TRUE)
#lines(tmp[,1], tmp[,2], lwd = 2, col = "red")

vline(0, col = "red", lwd = 1.5)
axis(1, at = log((xlab / 100) + 1)[seq(1, length(xlab), by = 2)], labels = xlab[seq(1, length(xlab), by = 2)])
axis(1, at = log((xlab / 100) + 1)[seq(2, length(xlab), by = 2)], labels = xlab[seq(2, length(xlab), by = 2)])
axis(2, at = seq(0, 1, by = 0.1), labels = 100 * seq(0, 1, by = 0.1))
mtext("Fecundity difference from multiparous (%)", 1, 2.5, cex = 1.25, font = 2, col = "grey20")
mtext("Proportion", 2, 2.5, cex = 1.25, font = 2, col = "grey20")

# Annotations:
#text(log(-29/100 + 1), 0.68, "Maximum loss", pos = 3, col = "grey20")
#text(log(-65/100 + 1), 0.40, "Classified as outliers", pos = 3, srt = 38, col = "grey20")
#text(log(8/100 + 1), 0.06, "Minimum egg loss", pos = 1, srt = 0, col = "grey20")
box(col = "grey50")
dev.off()