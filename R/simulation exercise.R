
n <- 50

k <- 5000
beta <- matrix(NA, nrow = k, ncol = 2)
for (i in 1:k){
   ix <- sample(1:nrow(x), n)
   beta[i,] <- coef(lm(log(x$fecundity[ix]) ~ log(x$carapace.width[ix])))
}

plot(beta[,1], beta[,2], cex = 0.1)
points(log(0.7493), 2.6108, pch = 22, bg = "red")       # Taylor (1996) Newfoundland
points(log(0.7244), 2.6712, pch = 21, bg = "red")       # Burmeister 2002 Greenland:
points(log(0.5394), 2.7295, pch = 21, bg = "red")       # Burmeister 2002 Greenland:
points(log(0.4905), 2.7206, pch = 21, bg = "red")       # Haynes 1976 SBS
points(log(0.0249), 3.4822, pch = 23, bg = "red")       # Jewett (1981) Chukchi:
points(log(2.19786), 2.407, pch = 21, bg = "green")     # Sainte-Marie (1993) Primiparous.
points(log(0.73114), 2.725, pch = 21, bg = "green")     # Sainte-Marie (1993) Multiparous.
points(log(0.672), 2.668, , pch = 21, bg = "blue")      # Paul and Paul 1997:
points(log(0.2664949), 2.9486, pch = 21, bg = "purple") # Comeau 1999 multipare.
   
points(log(0.2331), 2.9437, pch = 23, bg = "red")       # Danielsen - Barents Sea (2019):
points(log(0.1998876), 2.9, pch = 23, bg = "red")       # Divine - Arctic Ocean (2019):

t <- 40:100
r <- matrix(NA, nrow = k, ncol = length(t))
for (i in 1:k){
   r[i, ] <- beta[i,1] + beta[i,2] * log(t)
}
colnames(r) <- t
res <- apply(r, 2, quantile, p = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.975))

# Fecundity by shell condition versus CW:
#png(file = "figures/simulation exrcise.png", units = "in", width = 7, height = 7, res = 500)
plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = log(xscale), labels = xscale, col.ticks = "grey40", col.axis = "grey40")
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000, col.ticks = "grey40", col.axis = "grey40")
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
cols <- fade(c("olivedrab2", "indianred1", "royalblue2"), 0.8)
ix <- which(x$shell.condition %in% 4:5)
points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 23, cex = 0.4, bg = fade("royalblue2", 0.60), col = NA) 
ix <- which(x$shell.condition %in% 1:2)
points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 21, cex = 0.4, bg = fade("olivedrab2", 0.80), col = NA) 
ix <- which(x$shell.condition %in% 3)
points(log(x$carapace.width[ix]), log(x$fecundity[ix]), pch = 22, cex = 0.4, bg = fade("indianred1", 0.80), col = NA) 

legend("topleft", 
       legend = c("Shell condition 1 & 2", "Shell condition 3", "Shell condition 4 & 5"),
       pch = 21:23, pt.cex = 1.35, box.col = "grey50", 
       pt.bg = cols[1:3], col = cols[1:3], text.col = "grey30")
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2, col = "grey30")     
mtext("Fecundity (thousand eggs)", 2, 2.75, cex = 1.25, font = 2, col = "grey30")       

box(col = "grey50")
#dev.off()
t <- as.numeric(colnames(res))
polygon(c(log(t), rev(log(t))), c(res["2.5%",], rev(res["97.5%",])), col = fade("red"))


# Taylor (1996) Newfoundland
t <- 45:80
lines(log(t), log(0.7493 * t^2.6108), lwd = 2) 

# Burmeister 2002 Greenland:
t <- 40:80
lines(log(t), log(0.7244 * t^2.6712), lwd = 2) 
lines(log(t), log(0.5394 * t^2.7295), lwd = 2) 

# Jewett (1981) 
t <- 40:70
lines(log(t), log(0.4905 * t^2.7206), lwd = 2)  # SBS (Haynes 1976):
t <- 40:63
lines(log(t), log(0.0249 * t^3.4822), lwd = 2)  # Chukchi:

# Kolts (2015) SBS:
t <- 40:80
lines(log(t), log(0.0292 * t^3.4026), lwd = 2) #NBS

# Kon & Adachi (2006)
t <- 73:98
lines(log(t), log(3063*t - 170904), lwd = 2) # Primiparous.
lines(log(t), log(4498*t - 248393), lwd = 2) # Multiparous.

lines(log(t), 3125.1*t - 140125, lwd = 2) # Pubescent (oocytes)
lines(log(t), 3836.4*t - 190622, lwd = 2) # Primiparous (oocytes).

# Sainte-Marie (1993)
t <- 40:80
lines(log(t), log(10^(2.948*log10(t)-0.633)), lwd = 2) # Primiparous.
lines(log(t), log(10^(2.506*log10(t)+0.255)), lwd = 2) # Multiparous.
lines(log(t), log(10^(2.407*log10(t)+0.342)), lwd = 2) # Primiparous.
lines(log(t), log(10^(2.725*log10(t)-0.136)), lwd = 2) # Multiparous.

# Paul and Paul 1997:
lines(log(t), log(0.672 * t ^ 2.668), lwd = 2)

# Elner 1984:
t <- 50:80
lines(log(t), log(3092.23 * t^0.70), lwd = 2) 
lines(log(t), log(147.17 * t^1.42), lwd = 2) 

# Danielsen - Barents Sea (2019):
t <- 50:90
lines(log(t), log(0.2331 * t ^ 2.9437), lwd = 2) 

# Divine - Arctic Ocean (2019):
t <- 50:90
lines(log(t), log(exp(2.9 * log(t) - 1.61)), lwd = 2) 

# Haynes - Cape Breton (1976):
t <- 55:80
lines(log(t), log(0.0012*t^4.2), lwd = 2) 
lines(log(t), log(0.4905*t^2.706), lwd = 2) # SBS

# Davidson 1984:
lines(log(t), log(6.4080 * t^2.1690), lwd = 2)    # Newfoundland
lines(log(t), log(14.8371 * t^1.9859), lwd = 2)   # Eastern Cape Breton
lines(log(t), log(38.4554 * t^1.7649), lwd = 2)   # Western Cape Breton
lines(log(t), log(13.2530 * t^1.9922), lwd = 2)   # Anticosti

# Comeau - Bonne Bay (1999):
t <- 50:90
lines(log(t), 2.9486 * log(t) -1.3224, lwd = 2) 
