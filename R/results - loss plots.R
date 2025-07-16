# Regression coefficients:
beta      <- read.table(paste0(path, "betaCODAchain1.txt"), sep = "\t")
ix        <- 1:which(diff(beta[,1]) < 0)
iy        <- (which(diff(beta[,1]) < 0)+1):nrow(beta)
beta <- data.frame(primiparous = beta[ix,2], multiparous = beta[iy,2])
log.alpha <- read.table(paste0(path, "log.alphaCODAchain1.txt"), sep = "\t")
ix        <- 1:which(diff(log.alpha[,1]) < 0)
iy        <- (which(diff(log.alpha[,1]) < 0)+1):nrow(log.alpha)
log.alpha <- data.frame(primiparous = log.alpha[ix,2], multiparous = log.alpha[iy,2])

# Loss proportion results:
loss      <- read.table(paste0(path, "lossCODAchain1.txt"), sep = "\t")[,2]
dim(loss) <- c(length(loss) / nrow(x), nrow(x))

# Loss probability results:
L      <- read.table(paste0(path, "LCODAchain1.txt"), sep = "\t")[,2] - 1
dim(L) <- c(length(L) / nrow(x), nrow(x))

# Outlier process results:
Z         <- read.table(paste0(path, "ZCODAchain1.txt"), sep = "\t")[,2] - 1
dim(Z)    <- c(length(Z) / nrow(x), nrow(x))

# Maturity process results:
M         <- read.table(paste0(path, "MCODAchain1.txt"), sep = "\t")[,2] - 1
dim(M)    <- c(length(M) / sum(is.na(x$maturity)), sum(is.na(x$maturity)))
mat <- x$maturity
mat[is.na(mat)] <- (apply(M, 2, mean) > 0.5) + 1

# Multiparous loss scatterplot:
png(file = "figures/Multiparous loss plot.png", units = "in", width = 7, height = 7, res = 500)
plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = log(xscale), labels = xscale)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")

ix <- which(mat == 2 & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.3, bg = fade("red"), col = NA) 

ix <- which(mat == 2 & (apply(L, 2, mean) > 0.5) & (apply(Z, 2, mean) < 0.5))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 22, cex = 0.5, bg = fade("red4"), col = NA) 

mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)     
mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)     

legend("topleft", 
       legend = c("Multiparous - full clutch", "Multiparous - reduced clutch"),
       pch = c(21, 22), col = NA, pt.cex = 2, 
       box.lwd = 0.5, box.col = "grey50", 
       pt.bg = c(fade("red"), fade("red4")))

box(col = "grey50")
dev.off()


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