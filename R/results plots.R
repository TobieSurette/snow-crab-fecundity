path <- "bugs/results/double beta/"

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
r         <- read.table(paste0(path, "rCODAchain1.txt"), sep = "\t")[,2] - 1
dim(r)    <-  c(length(r) / nrow(x), nrow(x))

# Primiparous figure:
#png(file = "figures/Fecundity - primiparous.png", units = "in", width = 7, height = 7, res = 500)
plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = log(xscale), labels = xscale)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
points(log(x$carapace.width), log(x$fecundity), pch = 21, cex = 0.3, bg = "grey70", col = NA) 

mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)     
mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)    

cols <- fade(c("green3", "red", "orange", "orange"), 0.75)

ix <- (mat == 1) & (apply(Z, 2, mean) < 0.5) & (apply(L, 2, mean) < 0.5) & x$shell.condition %in% 3
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[1],  col = NA)
ix <- (mat == 2) & (apply(Z, 2, mean) < 0.5) & (apply(L, 2, mean) < 0.5) & x$shell.condition %in% 3
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[2],  col = NA)


ix <- (apply(Z, 2, mean) < 0.5) & (mat == 1) #& loss$mean < 1.5 & x$shell.condition %in% 1:2
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[1],  col = NA)
ix <- (apply(Z, 2, mean) < 0.5) & (mat == 2) # loss$mean < 1.5 & x$shell.condition %in% 3
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[2],  col = NA)



legend("topleft", 
       legend = c("Shell condition 2", "Shell condition 3", "Shell condition 4 & 5"),
       pch = 21:23, box.col = "grey50", 
       pt.bg = cols[1:3], col = cols[1:3])

box(col = "grey50")
dev.off()

# Multiparous figure:
png(file = "figures/Fecundity - multiparous.png", units = "in", width = 7, height = 7, res = 500)
plot(log(c(40, 95)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 95)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = log(xscale), labels = xscale)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
points(log(x$carapace.width), log(x$fecundity), pch = 21, cex = 0.3, bg = "grey50", col = NA) 
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)     
mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)     
cols <- fade(c("green3", "red", "orange", "orange"), 0.75)
m <- x$maturity
m[mat$ix] <- (mat$mean > 1.5) + 1
ix <- m == 2 & outlier$mean < 1.5 & x$shell.condition %in% 4:5
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.6, bg = cols[3],  col = NA)
ix <- m == 2 & outlier$mean < 1.5 & x$shell.condition %in% 3
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.6, bg = cols[2],  col = NA)

legend("topleft", 
       legend = c("Shell condition 2", "Shell condition 3", "Shell condition 4 & 5"),
       pch = 21:23, box.col = "grey50", 
       pt.bg = cols[1:3], col = cols[1:3])

box(col = "grey50")
dev.off()

# Multiparous egg loss figure:
png(file = "figures/Fecundity - multiparous with egg loss.png", units = "in", width = 7, height = 9, res = 500)

layout(rbind(0, cbind(0, kronecker(c(1,1,2), matrix(1, ncol = 5, nrow = 5)), 0), 0, 0))
par(mar = c(0,0,0,0))
plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 95)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
#points(log(x$carapace.width), log(x$fecundity), pch = 21, cex = 0.3, bg = "grey50", col = NA) 

mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)     

m <- x$maturity
m[mat$ix] <- (mat$mean > 1.5) + 1

cols <- c(fade("indianred1", 0.25), fade("indianred3", 0.75))
ix <- which((m == 2) & (outlier$mean < 1.5) & (loss$mean < 1.5)) 
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.6, bg = cols[1],  col = NA)

ix <- which((m == 2) & (outlier$mean < 1.5) & (loss$mean > 1.5)) 
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 22, cex = 0.6, bg = cols[2],  col = NA)

legend("topleft", 
       legend = c("Full-clutch multiparous", "Multiparous with egg loss"),
       pch = 21:22, box.col = "grey50", cex = 1.25, pt.cex = 2,
       pt.bg = cols, col = cols)

box(col = "grey50")

plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 95)), ylim = c(0,0.8), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(seq(0, 1, by = 0.2), lty = "dashed", lwd = 0.5, col = "grey70")
ix <- which((m == 2) & (loss$mean > 1.5))
iy <- which((m == 2) & (loss$mean < 1.5))
tx <- table(round(x$carapace.width[ix]))
ty <- table(round(x$carapace.width[iy]))
ty[setdiff(names(tx), names(ty))] <- 0
tx[setdiff(names(ty), names(tx))] <- 0
xx <- as.numeric(names(tx))
w <- log(xx + 0.2) - log(xx - 0.2)
for (i in 1:length(tx)){
   polygon(c(log(xx[i])-w[i], log(xx[i])-w[i], log(xx[i])+w[i], log(xx[i])+w[i]), 
           c(0, tx[i]/(tx[i]+ty[i]), tx[i]/(tx[i]+ty[i]), 0), col = "grey70", border = "grey40", lwd = 0.5)
   
}
axis(1, at = log(xscale), labels = xscale)
axis(2)
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2) 
mtext("Proportion of egg loss", 2, 2.5, cex = 1.25, font = 2) 
box(col = "grey50")
dev.off()





