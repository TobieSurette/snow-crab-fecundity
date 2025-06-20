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

# SC3 alpha coefficients:
log.alpha.sc <- read.table(paste0(path, "log.alpha.scCODAchain1.txt"), sep = "\t")[,2]

# Site x year alpha coefficients:
log.alpha.site <- read.table(paste0(path, "log.alpha.siteCODAchain1.txt"), sep = "\t")[,2]
tmp <- matrix(log.alpha.site, ncol = length(site.year) * 2)
log.alpha.site <- list(primiparous = tmp[, seq(1, ncol(tmp), by = 2)],
                       multiparous = tmp[, seq(2, ncol(tmp), by = 2)])
names(log.alpha.site$primiparous) <- site.year
names(log.alpha.site$multiparous) <- site.year

# Site x year alpha coefficients:
p.loss <- read.table(paste0(path, "p.lossCODAchain1.txt"), sep = "\t")[,2]
tmp <- matrix(p.loss, ncol = length(site.year) * 2)
p.loss <- list(primiparous = tmp[, seq(1, ncol(tmp), by = 2)],
               multiparous = tmp[, seq(2, ncol(tmp), by = 2)])
colnames(p.loss$primiparous) <- site.year
colnames(p.loss$multiparous) <- site.year

# Mature proportions:
p.mat <- read.table(paste0(path, "p.matCODAchain1.txt"), sep = "\t")[,2]
p.mat <- matrix(p.mat, ncol = length(site.year))
colnames(p.mat) <- site.year

# Maturity indicator:
M <- read.table(paste0(path, "MCODAchain1.txt"), sep = "\t")[,2]
M <- matrix(M, ncol = sum(x$shell.condition == 3))
x$maturity <- NA
x$maturity[x$shell.condition %in% 1:2] <- 0
x$maturity[x$shell.condition %in% 4:5] <- 1
x$maturity[is.na(x$maturity)] <- apply(M, 2, mean) - 1

# Outlier indicator:
Z <- read.table(paste0(path, "ZCODAchain1.txt"), sep = "\t")[,2]
Z <- matrix(Z, ncol = nrow(x))

# Loss indicator:
L <- read.table(paste0(path, "LCODAchain1.txt"), sep = "\t")[,2]
L <- matrix(L, ncol = nrow(x))

# Loss proportion:
loss <- read.table(paste0(path, "lossCODAchain1.txt"), sep = "\t")[,2]
loss <- matrix(loss, ncol = nrow(x))

# Residuals :
r <- read.table(paste0(path, "rCODAchain1.txt"), sep = "\t")[,2]
r <- matrix(r, ncol = nrow(x))

clg()
for (site in 1:58){
   #site <- 31
   png(file = paste0("figures/Site x year ", site.year[site], ".png"), units = "in", width = 7, height = 7, res = 500)
   plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
        xlim = log(c(40, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
        xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
   xscale <- seq(40, 100, by = 10)
   axis(1, at = log(xscale), labels = xscale)
   yscale <- seq(10000, 150000, by = 10000)
   axis(2, at = log(yscale), labels = yscale / 1000)
   vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
   hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
   points(log(x$carapace.width), log(x$fecundity), pch = 21, cex = 0.3, bg = fade("grey50"), col = NA) 
   
   mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)     
   mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)     
   
   box(col = "grey50")
   
   abline(-1.62, 2.929 , col  ="forestgreen", lwd = 2)
   abline(-0.1566	, 2.663, col  ="navyblue", lwd = 2)
 
 #  cols <- fade(c("green3", "indianred1", "royalblue2"), 0.8)
   cols <- fade(c("green3", "indianred1", "royalblue2"), 0.8)
   ix <- which((x$site == site) & x$shell.condition %in% 4:5)
   points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 23, cex = 0.9, bg = fade(cols[3], 0.60), col = NA) 
   ix <- which((x$site == site) & x$shell.condition %in% 1:2)
   points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 21, cex = 1, bg = fade(cols[1], 0.90), col = NA) 
   ix <- which((x$site == site) & x$shell.condition %in% 3)
   points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 22, cex = 0.9, bg = fade(cols[2], 0.80), col = NA) 
   
   legend("topleft", 
          legend = c("Shell condition 1 & 2", "Shell condition 3", "Shell condition 4 & 5"),
          pch = 21:23, pt.cex = 1.35, box.col = "grey50", 
          pt.bg = cols[1:3], col = cols[1:3], text.col = "grey30")
   mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2, col = "grey30")     
   mtext("Fecundity (thousand eggs)", 2, 2.75, cex = 1.25, font = 2, col = "grey30") 
   
   xx <- seq(30, 100, len = 100)
   mu <- matrix(NA, nrow = nrow(log.alpha.site$primiparous), ncol = length(xx))
   mu <- list(primiparous = mu, multiparous = mu)
   for (i in 1:length(xx)){
      mu$primiparous[,i] <- beta$primiparous * log(xx[i]) + 
         log.alpha$primiparous + 
         log.alpha.site$primiparous[, site] 
      
      mu$multiparous[,i] <- beta$multiparous * log(xx[i]) + 
         log.alpha$multiparous + 
         log.alpha.site$multiparous[, site]  
   }
   res <- list(primiparous = apply(mu$primiparous, 2, quantile, p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)),
               multiparous = apply(mu$multiparous, 2, quantile, p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)))
   #lines(log(xx), 
   #      mean(beta$primiparous) * log(xx) +
   #           mean(log.alpha$primiparous) +  
   #           mean(log.alpha.site$primiparous[, site]), lwd = 2)
   polygon(c(log(xx), rev(log(xx))), 
           c(res$primiparous["2.5%",], rev(res$primiparous["97.5%",])), col = fade("green3"),
           border = fade("green3", 0.75))
   polygon(c(log(xx), rev(log(xx))), 
           c(res$multiparous["2.5%",], rev(res$multiparous["97.5%",])), col = fade("blue"),
           border = fade("blue", 0.75))       
   mtext(site.year[site], 3, 0.5, font = 2, cex = 1.5)
   
   dev.off()
}

sites <- c("Baie des Chaleurs", "Shediac Valley", "Bradelle Bank", "PEI", "Cape Breton", "")


library("RColorBrewer")
#display.brewer.all()         
cols <- brewer.pal(n = length(sites), name = "RdYlGn")

# Site x year effects boxplot:
png(file = "figures/Site xyear effects.png", units = "in", width = 7, height = 9, res = 500)
m <- rbind(0, cbind(0, kronecker(matrix(1:2, ncol = 1), matrix(1, nrow = 5, ncol = 5)), 0), 0)
layout(m)
par(mar = c(0,0,0,0))
plot(c(0, length(site.year)), c(-0.2, 0.2), type = "n", xaxt = "n", ylim = c(-0.2, 0.2), yaxs = "i", ylab = "", xlab = "")
grid()
boxplot(log.alpha.site$primiparous, col = cols[match(gsub("^[0-9]+ ", "", site.year), sites) ], 
        cex = 0, add = TRUE, xaxt = "n", yaxt = "n", lwd = 0.7, border = "grey20")
hline(0, col = "red", lty = "dashed", lwd = 2)
legend("topleft", 
       legend = sites, bg = NA, box.lwd = 0.5, box.col = "grey50",
       pch = 22, pt.bg = cols, pt.cex = 2.5, pt.lwd = 0.5)
mtext("Primiparous", 3, -2, font = 2, cex = 1.25)
box(col = "grey50")

plot(c(0, length(site.year)), c(-0.2, 0.2), type = "n", xaxt = "n", yaxt = "n", ylim = c(-0.2, 0.2), yaxs = "i", ylab = "", xlab = "")
grid()
boxplot(log.alpha.site$multiparous, col = cols[match(gsub("^[0-9]+ ", "", site.year), sites) ], 
        cex = 0, add = TRUE, xaxt = "n", yaxt = "n", lwd = 0.7, border = "grey20")
hline(0, col = "red", lty = "dashed", lwd = 2)
legend("topleft", 
       legend = sites, bg = NA, box.lwd = 0.5, box.col = "grey50",
       pch = 22, pt.bg = cols, pt.cex = 2.5, pt.lwd = 0.5)
mtext("Multiparous", 3, -2, font = 2, cex = 1.25)
axis(2, at = c(-0.2, -0.1, 0, 0.1))
axis(1, at = 1:length(site.year), labels = unlist(lapply(strsplit(site.year, " "), function(x) x[1])), las = 2)
box(col = "grey50")
dev.off()

png(file = "figures/Multiparous proportion.png", units = "in", width = 7, height = 5.5, res = 500)
plot(c(0, length(site.year)), c(0, 1), type = "n", xaxt = "n", ylim = c(0, 1.05), yaxs = "i", ylab = "", xlab = "")
grid()
boxplot(p.mat, col = cols[match(gsub("^[0-9]+ ", "", site.year), sites) ], 
        cex = 0, add = TRUE, xaxt = "n", yaxt = "n", lwd = 0.7, border = "grey20")
hline(mean(p.mat), col = "red", lty = "dashed", lwd = 2)
legend("bottomright", 
       legend = sites, bg = NA, box.lwd = 0.5, box.col = "grey50",
       pch = 22, pt.bg = cols, pt.cex = 2, pt.lwd = 0.5, cex = 0.75)
axis(1, at = 1:length(site.year), labels = unlist(lapply(strsplit(site.year, " "), function(x) x[1])), las = 2, cex = 0.6)
mtext("Year", 1, 3.5, font = 2, cex = 1)
mtext("Proportion of multiparous", 2, 2.25, font = 2, cex = 1)
box(col = "grey50")

dev.off()

# P loss proportions boxplot:
png(file = "figures/Loss probability plot.png", units = "in", width = 7, height = 9, res = 500)
m <- rbind(0, cbind(0, kronecker(matrix(1:2, ncol = 1), matrix(1, nrow = 5, ncol = 5)), 0), 0)
layout(m)
par(mar = c(0,0,0,0))
plot(c(0, length(site.year)), c(0, 1), type = "n", xaxt = "n", yaxt = "n", ylim = c(0, 1), yaxs = "i", ylab = "", xlab = "")
grid()
boxplot(p.loss$primiparous, col = cols[match(gsub("^[0-9]+ ", "", site.year), sites) ], 
        cex = 0, add = TRUE, xaxt = "n", yaxt = "n", lwd = 0.7, border = "grey20")
hline(mean(p.loss$primiparous), col = "red", lty = "dashed", lwd = 2)
legend("topleft", 
       legend = sites, bg = NA, box.lwd = 0.5, box.col = "grey50",
       pch = 22, pt.bg = cols, pt.cex = 2.5, pt.lwd = 0.5)
mtext("Primiparous", 3, -2, font = 2, cex = 1.25)
mtext("Proportion of egg loss", 2, 2.75, font = 2, at = 0, cex = 1.25)
axis(2, at = seq(0, 1, by = 0.1))
box(col = "grey50")

plot(c(0, length(site.year)), c(0, 1), type = "n", xaxt = "n", yaxt = "n", ylim = c(0, 1), yaxs = "i", ylab = "", xlab = "")
grid()
boxplot(p.loss$multiparous, col = cols[match(gsub("^[0-9]+ ", "", site.year), sites) ], 
        cex = 0, add = TRUE, xaxt = "n", yaxt = "n", lwd = 0.7, border = "grey20")
hline(mean(p.loss$multiparous), col = "red", lty = "dashed", lwd = 2)
#legend("topleft", 
#       legend = sites, bg = NA, box.lwd = 0.5, box.col = "grey50",
#       pch = 22, pt.bg = cols, pt.cex = 2.5, pt.lwd = 0.5)
mtext("Multiparous", 3, -2, font = 2, cex = 1.25)
axis(2, at = seq(0, 0.9, by = 0.1))
axis(1, at = 1:length(site.year), labels = unlist(lapply(strsplit(site.year, " "), function(x) x[1])), las = 2)
box(col = "grey50")
dev.off()

