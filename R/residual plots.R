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

# Residual process results:
r         <- read.table(paste0(path, "rCODAchain1.txt"), sep = "\t")[,2]
dim(r)    <-  c(length(r) / nrow(x), nrow(x))


# By size and spawning stage for non-loss, non-outliers:
png(file = "figures/residual plots/Residual by size and spawning stage.png", units = "in", width = 7, height = 9, res = 500)
ll <- rbind(0, cbind(0, kronecker(1:2, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(ll)
par(mar = c(0,0,0,0))
ix <- which((mat == 2) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
plot(c(38, 90), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
boxplot(apply(r, 2, mean)[ix] ~ round(x$carapace.width[ix]), ylim = c(-0.25, 0.25), 
        at = as.numeric(names(table(round(x$carapace.width[ix])))), add = TRUE, xaxt = "n", border = "grey40", lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.21, "Multiparous", cex = 1.5, font = 2)
box(col = "grey50")
mtext("Residual", 2, 2.75, at = par("usr")[3], cex = 1.5, font = 2)
plot(c(38, 90), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 1) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ round(x$carapace.width[ix]), ylim = c(-0.25, 0.25), 
        at = as.numeric(names(table(round(x$carapace.width[ix])))), add = TRUE, xaxt = "n", border = "grey40", lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.21, "Primiparous", cex = 1.5, font = 2)

axis(1, at = seq(40, 100, by = 2))
mtext("Carapace width (mm)", 1, 3.0, cex = 1.5, font = 2)
box(col = "grey50")
dev.off()

# By size and spawning stage for with loss, non-outliers:
png(file = "figures/residual plots/Residual by size and spawning stage - with loss.png", units = "in", width = 7, height = 9, res = 500)
ll <- rbind(0, cbind(0, kronecker(1:2, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(ll)
par(mar = c(0,0,0,0))
ix <- which((mat == 2) & (apply(L, 2, mean) > 0.5) & (apply(Z, 2, mean) < 0.5))
plot(c(38, 90), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
boxplot(apply(r, 2, mean)[ix] ~ round(x$carapace.width[ix]), ylim = c(-0.25, 0.25), 
        at = as.numeric(names(table(round(x$carapace.width[ix])))), add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.21, "Multiparous", cex = 1.5, font = 2)
box(col = "grey50")
mtext("Residual", 2, 2.75, at = par("usr")[3], cex = 1.5, font = 2)
plot(c(38, 90), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 1) & (apply(L, 2, mean) > 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ round(x$carapace.width[ix]), ylim = c(-0.25, 0.25), 
        at = as.numeric(names(table(round(x$carapace.width[ix])))), add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.21, "Primiparous", cex = 1.5, font = 2)

axis(1, at = seq(40, 100, by = 2))
mtext("Carapace width (mm)", 1, 3.0, cex = 1.5, font = 2)
box(col = "grey50")
dev.off()

# Residuals by year with no loss, no outliers:
png(file = "figures/residual plots/residual by year and spawning stage.png", units = "in", width = 7, height = 9, res = 500)
ll <- rbind(0, cbind(0, kronecker(1:2, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(ll)
par(mar = c(0,0,0,0))

plot(c(1985, 2023), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 2) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ x$year[ix], ylim = c(-0.25, 0.25), 
        at = sort(unique(x$year[ix])),
        add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(2019, 0.21, "Multiparous", cex = 1.5, font = 2)
box(col = "grey50")
mtext("Residual", 2, 2.75, at = par("usr")[3], cex = 1.5, font = 2)
plot(c(1985, 2023), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 1) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ x$year[ix], ylim = c(-0.25, 0.25), 
        at = sort(unique(x$year[ix])),
        add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(2019, 0.21, "Primiparous", cex = 1.5, font = 2)
box(col = "grey50")
axis(1)
mtext("Year", 1, 3.0, cex = 1.5, font = 2)
box(col = "grey50")
dev.off()


# Residuals by month with no loss, no outliers:
png(file = "figures/residual plots/residual by month and spawning stage.png", units = "in", width = 7, height = 9, res = 500)
ll <- rbind(0, cbind(0, kronecker(1:2, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(ll)
par(mar = c(0,0,0,0))

plot(c(1, 12), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 2) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ x$month[ix], ylim = c(-0.25, 0.25), 
        at = sort(unique(x$month[ix])),
        add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(2.5, 0.21, "Multiparous", cex = 1.5, font = 2)
box(col = "grey50")
mtext("Residual", 2, 2.75, at = par("usr")[3], cex = 1.5, font = 2)
plot(c(1, 12), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 1) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ x$month[ix], ylim = c(-0.25, 0.25), 
        at = sort(unique(x$month[ix])),
        add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(2.5, 0.21, "Primiparous", cex = 1.5, font = 2)
box(col = "grey50")
axis(1, at = 1:12, labels = month.abb[1:12])
mtext("Month", 1, 3.0, cex = 1.5, font = 2)
box(col = "grey50")
dev.off()

# Residuals by location with no loss, no outliers:
png(file = "figures/residual plots/residual by location and spawning stage.png", units = "in", width = 7, height = 9, res = 500)
ll <- rbind(0, cbind(0, kronecker(1:2, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(ll)
par(mar = c(0,0,0,0))

plot(c(0, 7), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 2) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ x$location[ix], ylim = c(-0.25, 0.25), 
        add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(3.5, 0.21, "Multiparous", cex = 1.5, font = 2)
box(col = "grey50")
mtext("Residual", 2, 2.75, at = par("usr")[3], cex = 1.5, font = 2)
plot(c(0, 7), c(-0.25, 0.25), type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 1) & (apply(L, 2, mean) < 0.5) & (apply(Z, 2, mean) < 0.5))
boxplot(apply(r, 2, mean)[ix] ~ x$location[ix], ylim = c(-0.25, 0.25), 
        add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(3.5, 0.21, "Primiparous", cex = 1.5, font = 2)
box(col = "grey50")
axis(1, at = 1:6, labels = c("unknown", sort(unique(x$location))[-1]), las = 2)
#mtext("Month", 1, 3.0, cex = 1.5, font = 2)
box(col = "grey50")
dev.off()


clg()
png(file = "figures/residual plots/Residual by size and spawning stage - no outliers.png", units = "in", width = 7, height = 9, res = 500)

#png(file = "figures/Residual histogram.png", units = "in", width = 7, height = 7, res = 500)
rm <- log(x$fecundity) - mean(beta$multiparous) * log(x$carapace.width) - mean(log.alpha$multiparous)
rp <- log(x$fecundity) - mean(beta$primiparous) * log(x$carapace.width) - mean(log.alpha$primiparous)
ylim <- c(-0.35, 0.35)
ll <- rbind(0, cbind(0, kronecker(1:2, matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(ll)
par(mar = c(0,0,0,0))
ix <- which((mat == 2) & (apply(Z, 2, mean) < 0.5))
plot(c(38, 90), ylim, type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
boxplot(rm[ix] ~ round(x$carapace.width[ix]), ylim = c(-0.25, 0.25), 
        at = as.numeric(names(table(round(x$carapace.width[ix])))), add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.8 * diff(ylim) / 2, "Multiparous", cex = 1.5, font = 2)
box(col = "grey50")
mtext("Residual", 2, 2.75, at = par("usr")[3], cex = 1.25, font = 2)
plot(c(38, 90), ylim, type = "n", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
ix <- which((mat == 1) & (apply(Z, 2, mean) < 0.5))
boxplot(rp[ix] ~ round(x$carapace.width[ix]), ylim = c(-0.25, 0.25), 
        at = as.numeric(names(table(round(x$carapace.width[ix])))), add = TRUE, xaxt = "n", border = "grey40", cex = 0, lwd = 0.6)
hline(0, col = fade("red"), lwd = 2)
text(84, 0.8 * diff(ylim) / 2, "Primiparous", cex = 1.5, font = 2)

axis(1, at = seq(40, 100, by = 2))
mtext("Carapace width (mm)", 1, 3.0, cex = 1.25, font = 2)
box(col = "grey50")
dev.off()

