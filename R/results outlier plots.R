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

# Outliers:
png(file = "figures/Fecundity - outliers.png", units = "in", width = 7, height = 7, res = 500)
plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = log(xscale), labels = xscale)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
points(log(x$carapace.width), log(x$fecundity), pch = 21, cex = 0.3, bg = fade("grey50", 0.25), col = NA) 

mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)     
mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)     

cols <- fade(c("green3", "red", "orange", "orange"), 0.75)
ix <- outlier$mean > 1.5 & x$shell.condition == 2
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[1],  col = NA)
ix <- outlier$mean > 1.5 & x$shell.condition == 3
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[2],  col = NA)
ix <- outlier$mean > 1.5 & x$shell.condition == 4
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[3],  col = NA)
ix <- outlier$mean > 1.5 & x$shell.condition == 5
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, cex = 0.7, bg = cols[4],  col = NA)

legend("topleft", 
       legend = c("Shell condition 2", "Shell condition 3", "Shell condition 4 & 5"),
       pch = 21:23, box.col = "grey50", 
       pt.bg = cols[1:3], col = cols[1:3])
box(col = "grey50")
dev.off()


