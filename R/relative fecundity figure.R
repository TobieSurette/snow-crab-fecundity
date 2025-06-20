path <- "bugs/results/double beta/"

# Regression coefficients:
beta      <- read.table(paste0(path, "beta.txt"), sep = "\t")
ix        <- 1:which(diff(beta[,1]) < 0)
iy        <- (which(diff(beta[,1]) < 0)+1):nrow(beta)
beta <- data.frame(primiparous = beta[ix,2], multiparous = beta[iy,2])
log.alpha <- read.table(paste0(path, "log.alpha.txt"), sep = "\t")
ix        <- 1:which(diff(log.alpha[,1]) < 0)
iy        <- (which(diff(log.alpha[,1]) < 0)+1):nrow(log.alpha)
log.alpha <- data.frame(primiparous = log.alpha[ix,2], multiparous = log.alpha[iy,2])

mat         <- read.csv(paste0(path, "maturity.csv"))
mat$ix      <- as.numeric(unlist(lapply(strsplit(gsub("]", "", mat$X), "[", fixed = TRUE), function(x) x[2])))
m <- rep(NA, nrow(x))
m[which(x$shell.condition %in% 1:2)] <- 1
m[which(x$shell.condition %in% 4:5)] <- 2
m[mat$ix] <- (mat$mean > 1.5) + 1


xx <- seq(40, 90, by = 1)

r <- NULL
for (i in 1:length(xx)){
   w <- exp(log.alpha$primiparous + beta$primiparous * log(xx[i])) / 
        exp(log.alpha$multiparous + beta$multiparous * log(xx[i]))
   r <- rbind(r, quantile(w, p = c(0.025, 0.25, 0.5, 0.5, 0.75, 0.975)))
}
rownames(r) <- xx
r <- 100*r

# Relative fecundity of primiparous versus multiparous:
clg()
png(file = "figures/Relative fecundity versus size.png", units = "in", width = 7, height = 7, res = 500)
plot(c(39, 91), 100*c(0.60, 0.80), xaxs = "i", yaxs = "i", type = "n", xlab = "", ylab = "")
grid()
for (i in 1:length(xx)){
   w <- 0.8 * mean(diff(xx)) / 2
   polygon(c(xx[i]-w, xx[i]-w, xx[i]+w, xx[i]+w), c(r[i,"25%"], r[i,"75%"], r[i,"75%"], r[i,"25%"]),
           col = "grey70", border = "grey30", lwd = 0.5)
   lines(c(xx[i]-w, xx[i]+w), c(r[i,"50%"], r[i,"50%"]), lwd = 0.75, col = "grey30")
   lines(c(xx[i], xx[i]), c(r[i,"2.5%"], r[i,"25%"]), lwd = 0.75, col = "grey30")
   lines(c(xx[i], xx[i]), c(r[i,"75%"], r[i,"97.5%"]), lwd = 0.75, col = "grey30")
   lines(c(xx[i]-w/2, xx[i]+w/2), c(r[i,"97.5%"], r[i,"97.5%"]), lwd = 0.75, col = "grey30")
   lines(c(xx[i]-w/2, xx[i]+w/2), c(r[i,"2.5%"], r[i,"2.5%"]), lwd = 0.75, col = "grey30")
}
mtext("Carapace width (mm)", 1, 2.75, cex = 1.25, font = 2)
mtext("Relative fecundity (%)", 2, 2.75, cex = 1.25, font = 2)
box(col = "grey50")
dev.off()

# Regression lines:
png(file = "figures/Fecundity - regression lines 3.png", units = "in", width = 7, height = 7, res = 500)
plot(log(c(40, 95)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 95)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = log(xscale), labels = xscale)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)     
mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)     

# Draw confidence intervals on regression lines:
xx <- seq(3.5, 4.7, len = 100)

# Primiparous:
xx <- seq(40, 100, by = 1)
r <- NULL
points(log(x$carapace.width)[m == 1], log(x$fecundity)[m == 1], pch = 21, cex = 0.30, bg = "olivedrab3", col = NA) 
for (i in 1:length(xx)) r <- rbind(r, quantile(beta$primiparous * log(xx[i]) + log.alpha$primiparous, p = c(0.025, 0.975)))
polygon(log(c(xx, rev(xx))), c(r[,1], rev(r[,2])), col = fade("olivedrab4", 0.50), border = fade("olivedrab4", 0.50))
abline(mean(log.alpha$primiparous), mean(beta$primiparous), col = fade("olivedrab4", 1))

# Multiparous:
r <- NULL
points(log(x$carapace.width)[m == 2], log(x$fecundity)[m == 2], pch = 21, cex = 0.25, bg = "indianred2", col = NA) 
for (i in 1:length(xx)) r <- rbind(r, quantile(beta$multiparous * log(xx[i]) + log.alpha$multiparous, p = c(0.025, 0.975)))
polygon(log(c(xx, rev(xx))), c(r[,1], rev(r[,2])), col = fade("indianred3", 0.50), border = NA)
abline(mean(log.alpha$multiparous), mean(beta$multiparous), col = fade("indianred3", 1))

# Display equations:
text(log(48), log(24000), expression(f[m](x) == 0.6653*x^2.720), col = "grey20", pos = 3, srt = 32)
text(log(48), log(16000), expression(f[p](x) == 0.1304*x^3.028), col = "grey20", pos = 1, srt = 35)



# Display reference equations:
# Haynes 1976 (GSL):
abline(log(0.0012), 4.2, col = fade("blue", 1))
text(log(70), log(70000), expression(f[m](x) == 0.0012*x^4.2), pos = 3, srt = 55, col = fade("blue", 1))

# Divine 2019 (northern Alaska):
abline(-1.61, 2.9, col = fade("purple", 1))
text(log(80), log(65000), expression(f[m](x) == 0.0012*x^4.2), pos = 1, srt = 35, col = fade("purple", 1))

# Sainte-Marie 1993 (Baie Sainte-Marguerite) (something's wrong here):
#abline(-0.633, 2.948, col = fade("orange", 1), lwd = 3) # Primiparous by shell condition.
#abline(-0.255, 2.506, col = fade("orange", 1), lwd = 3) # Multiparous  by shell condition.
abline(0.342, 2.407, col = fade("orange", 1), lwd = 3)   # Primiparous by ejaculate load.
abline(-0.136, 2.725, col = fade("orange", 1), lwd = 3)  # Multiiparous by ejaculate load.

# Comeau 1999 (Bonne Bay) (surprisingly close:
abline(-1.3224, 2.9486, col = fade("pink", 1), lwd = 3)   # Multiparous by shell condition.

# Danielsen 2018 (Barents Sea) (sample contains primiparous and multiparous mixed):
abline(log(0.24), 2.93, col = fade("gold", 1), lwd = 3)   # Multiparous by shell condition.


#text(log(80), log(65000), expression(f[m](x) == 0.0012*x^4.2), pos = 1, srt = 35, col = fade("purple", 1))


box(col = "grey50")

legend("topleft", 
       legend = c("Multiparous", "Primiparous"),
       lwd = 2, cex = 1.25, box.col = "grey50", 
       col = c("indianred1", "olivedrab2"))

dev.off()



