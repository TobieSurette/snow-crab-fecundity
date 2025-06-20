# Histogram plot:
png(file = "figures/Histogram of CW.png", units = "in", width = 7, height = 7, res = 500)
gbarplot(table(round(x$carapace.width)), ylim = c(0, 500), grid = TRUE)
mtext("Frequency", 2, 2.75, cex = 1.25, font = 2) 
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)  
box(col = "grey50")
dev.off()

# Basic data plot:
png(file = "figures/fecundity versus CW.png", units = "in", width = 7, height = 7, res = 500)
plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
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

box(col = "grey50")
dev.off()

# Fecundity by shell condition versus CW:
png(file = "figures/fecundity by shell condition.png", units = "in", width = 7, height = 7, res = 500)
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
dev.off()

# Side-view of fecundity data along a reference line:
png(file = "figures/Data residuals by shell condition.png", units = "in", width = 7, height = 5.5, res = 500)
r <- log(x$fecundity) - 3 * log(x$carapace.width) + 1.73
t <- table(round(r*50)/50, x$shell.condition)
plot(c(-1.5, 0.6), c(0, 0.04), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
lines(as.numeric(rownames(t)), (t[,1] + t[,2]) / sum(t), col = fade("green3", 0.8), lwd = 2)
lines(as.numeric(rownames(t)), t[, 3] / sum(t), col = fade("indianred1", 0.8), lwd = 2)
lines(as.numeric(rownames(t)), (t[,4] + t[,5]) / sum(t), col = fade("royalblue2", 0.8), lwd = 2)
lines(as.numeric(rownames(t)), apply(t, 1, sum) / sum(t), col = fade("grey40", 0.5), lwd = 2)
mtext("Relative frequency", 2, 2.5, font = 2, cex = 1.25)
mtext("log-scale residual fecundity", 1, 2.5, font = 2, cex = 1.25)
p <- seq(-100, 100, by = 10)
v <- log(p/100 + 1)
#vline(v, col = fade("grey50"), lty = "dashed", lwd = 0.5)
#hline((0:5)/100, col = fade("grey50"), lty = "dashed", lwd = 0.5)
axis(1) #, at = v, labels = p)
legend("topleft", 
       legend = c("Shell condition 1 & 2", "Shell condition 3", "Shell condition 4 & 5", "Total"),
       col = fade(c("green3", "indianred1", "royalblue2", "grey40"), 0.8),
       lwd = 2, box.lwd = 0.5, box.col = "grey50")
box(col = "grey50")
dev.off()

# Side-view of fecundity data along a reference line (3-panel):
png(file = "figures/Data residuals by shell condition 3-panel.png", units = "in", width = 7, height = 7, res = 500)
r <- log(x$fecundity) - 3 * log(x$carapace.width) + 1.73
t <- table(round(r*50)/50, x$shell.condition)

m <- rbind(0, cbind(0, kronecker(matrix(1:3), matrix(1, nrow = 5, ncol = 5)), 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))
plot(c(-1.5, 0.6), c(0, 325), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
gbarplot(t[,1] + t[,2], as.numeric(rownames(t)), add = TRUE, col = fade("green3", 0.5), lwd = 0.5)
legend("top", legend = "Shell condition 4 & 5", cex = 1.3, text.font = 2, bg = NA, box.col =  NA,
       pch = 22, pt.bg = fade("green3", 0.5), pt.cex = 3)
plot(c(-1.5, 0.6), c(0, 325), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
gbarplot(t[, 3], as.numeric(rownames(t)), add = TRUE, col = fade("indianred1", 0.5), lwd = 0.5)
legend("top", legend = "Shell condition 4 & 5", cex = 1.3, text.font = 2, bg = NA, box.col =  NA,
       pch = 22, pt.bg = fade("indianred1", 0.5), pt.cex = 3)
mtext("Frequency", 2, 2.5, font = 2, cex = 1.25)
plot(c(-1.5, 0.6), c(0, 325), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
gbarplot(t[,4] + t[,5], as.numeric(rownames(t)), add = TRUE, col = fade("royalblue2", 0.5), lwd = 0.5)
legend("top", legend = "Shell condition 4 & 5", cex = 1.3, text.font = 2, bg = NA, box.col =  NA,
       pch = 22, pt.bg = fade("royalblue2", 0.5), pt.cex = 3)
mtext("log-scale residual fecundity", 1, 2.5, font = 2, cex = 1.25)
axis(1) #, at = v, labels = p)
box(col = "grey50")
dev.off()

#png(file = "figures/fecundity by shell condition - annotated.png", units = "in", width = 7, height = 7, res = 500)
plot(c(40, 100), c(5000, 125000), col = "grey", 
     xlim = c(35, 90), ylim = c(0, 125000), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = xscale, col.ticks = "grey30", col.axis = "grey30")
yscale <- seq(0, 150000, by = 10000)
axis(2, at = yscale, labels = yscale / 1000, col.ticks = "grey30", col.axis = "grey30")
vline(xscale, lty = "dashed", lwd = 0.5, col = "grey70")
hline(yscale, lty = "dashed", lwd = 0.5, col = "grey70")
cols <- fade(c("olivedrab2", "indianred1", "royalblue2"), 0.8)
ix <- which(x$shell.condition %in% 4:5 & x$year < 1991)
points(jitter(x$carapace.width[ix], amount = 0.5), x$fecundity[ix], pch = 23, cex = 0.4, bg = fade("royalblue2", 0.60), col = NA) 
ix <- which(x$shell.condition %in% 4:5 & x$year >= 1991)
points(x$carapace.width[ix], x$fecundity[ix], pch = 23, cex = 0.4, bg = fade("royalblue2", 0.60), col = NA) 
ix <- which(x$shell.condition %in% 1:2 & x$year < 1991)
points(jitter(x$carapace.width[ix], amount = 0.5), x$fecundity[ix], pch = 21, cex = 0.4, bg = fade("olivedrab2", 0.80), col = NA) 
ix <- which(x$shell.condition %in% 1:2 & x$year >= 1991)
points(x$carapace.width[ix], x$fecundity[ix], pch = 21, cex = 0.4, bg = fade("olivedrab2", 0.80), col = NA) 
ix <- which(x$shell.condition %in% 3 & x$year < 1991)
points(jitter(x$carapace.width[ix], amount = 0.5), x$fecundity[ix], pch = 22, cex = 0.4, bg = fade("indianred1", 0.80), col = NA) 
ix <- which(x$shell.condition %in% 3 & x$year >= 1991)
points(x$carapace.width[ix], x$fecundity[ix], pch = 22, cex = 0.4, bg = fade("indianred1", 0.80), col = NA) 

text(35, 4000, "Females start maturing at\n about 40mm CW", pos = 4, font = 3, srt = 0, cex = 0.75, col = "grey30")
text(75, 20000, "Primiparous females:\nSoft and new-shelled\nFecundity is 30% less that of\n multiparous females", pos = 1, font = 3, srt = 0, cex = 0.75, col = "grey30")
text(55, 70000, "Multiparous females:\nHard and old-shelled", pos = 3, font = 3, srt = 0, cex = 0.75, col = "grey30")
arrows(75, 20000, 67, 35000, lwd = 1.5, col = "grey40", length = 0.08)
arrows(55, 70000, 60, 55000, lwd = 1.5, col = "grey40", length = 0.08)

text(80, 35000, "Some females have\n  significant egg loss", pos = 1, font = 3, srt = 0, cex = 0.75, col = "grey30")
arrows(80, 35000, 74, 37000, lwd = 1, lty = "solid", col = "grey40", length = 0.05)
arrows(80, 35000, 78, 43000, lwd = 1, lty = "solid", col = "grey40", length = 0.05)
arrows(80, 35000, 84, 43500, lwd = 1, lty = "solid", col = "grey40", length = 0.05)

legend("topleft", 
       legend = c("Shell condition 1 & 2", "Shell condition 3", "Shell condition 4 & 5"),
       pch = 21:23, pt.cex = 1.35, box.col = "grey30", 
       pt.bg = cols[1:3], col = cols[1:3], text.col = "grey30")
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2, col = "grey20")     
mtext("Fecundity (thousand eggs)", 2, 2.75, cex = 1.25, font = 2, col = "grey20")     

#dev.off()

# Taylor (1996) Newfoundland
t <- 45:80
lines(t, 0.7493 * t^2.6108, lwd = 2) 

# Burmeister 2002 Greenland:
t <- 40:80
lines(t, 0.7244 * t^2.6712, lwd = 2, col = "blue") 
lines(t, 0.5394 * t^2.7295, lwd = 2, col = "blue") 

# Jewett (1981) 
t <- 40:70
lines(t, 0.4905 * t^2.7206, lwd = 2)  # SBS (Haynes 1976):
t <- 40:63
lines(t, 0.0249 * t^3.4822, lwd = 2)  # Chukchi:

# Kolts (2015) SBS:
t <- 40:80
lines(t, 0.0292 * t^3.4026, lwd = 2, col = "red") #NBS

# Kon & Adachi (2006)
t <- 73:97
lines(t, 3063*t - 170904, lwd = 2, col = "red") # Primiparous.
t <- 73:91
lines(t, 4498*t - 248393, lwd = 2, col = "red") # Multiparous.

t <- 65:82
lines(t, 3125.1*t - 140125, lwd = 2, col = "blue") # Pubescent (oocytes)
t <- 68:96
lines(t, 3836.4*t - 190622, lwd = 2) # Primiparous (oocytes).

# Sainte-Marie (1993)
t <- 40:80
lines(t, 10^(2.948*log10(t)-0.633), lwd = 2) # Primiparous.
lines(t, 10^(2.506*log10(t)+0.255), lwd = 2) # Multiparous.
lines(t, 10^(2.407*log10(t)+0.342), lwd = 2, col = "green3") # Primiparous.
lines(t, 10^(2.725*log10(t)-0.136), lwd = 2, col = "blue")   # Multiparous.

# Paul and Paul 1997:
t <- 40:80
lines(t, 0.672 * t ^ 2.668, lwd = 2)

# Elner 1984:
t <- 50:80
lines(t, 3092.23 * t^0.70, lwd = 2) 
lines(t, 147.17 * t^1.42, lwd = 2) 

# Danielsen - Barents Sea (2019):
t <- 50:90
lines(t, 0.2331 * t ^ 2.9437, lwd = 2) 

# Divine - Arctic Ocean (2019):
t <- 50:90
lines(t, exp(2.9 * log(t) - 1.61), lwd = 2) 

# Haynes - Cape Breton (1976):
t <- 55:80
lines(t, 0.0012*t^4.2, lwd = 2) 
lines(t, 0.4905*t^2.706, lwd = 2) # SBS

# Davidson 1984:
lines(t, 6.4080 * t^2.1690, lwd = 2)    # Newfoundland
lines(t, 14.8371 * t^1.9859, lwd = 2)   # Eastern Cape Breton
lines(t, 38.4554 * t^1.7649, lwd = 2)   # Western Cape Breton
lines(t, 13.2530 * t^1.9922, lwd = 2)   # Anticosti

# Comeau - Bonne Bay (1999):
t <- 50:90
lines(t, exp(2.9486 * log(t) -1.3224), lwd = 2) 

box(col = "grey50")
#dev.off()




ix <- which((x$egg.colour == "light orange") & (x$shell.condition %in% 4:5))
ix <- which((x$egg.colour == "dark orange") & (x$shell.condition %in% 4:5))

ix <- which((x$gonad.colour == "orange") & (x$shell.condition %in% 1:2))

points(x$carapace.width[ix], x$fecundity[ix], pch = 22, cex = 0.75, bg = "green2", col = NA)

plot(log(c(40, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(40, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(40, 100, by = 10)
axis(1, at = log(xscale), labels = xscale)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
x$egg.development

points(log(x$carapace.width), log(x$fecundity), pch = 21, cex = 0.3, bg = "grey50", col = NA) 

ix <- which(mat == 1 & outlier$mean < 1.5 & x$location == "Cape Breton" & x$month %in% 6)
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], 
       pch = 21, bg = "red", col = NA) 


ix <- which(x$egg.development == "NF")
ix <- which(x$shell.condition == 2 &  x$egg.development %in% c("9", "9-10", "10", "11", "12", "13"))
ix <- which(x$shell.condition %in% 4:5 &  x$egg.development %in% c("9", "9-10", "10", "11", "12", "13"))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], 
       pch = 21, bg = "red", col = NA) 

