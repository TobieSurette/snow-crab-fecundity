# Load model summary results:
mat         <- read.csv("bugs/results/single beta/maturity.csv")
mat$ix      <- as.numeric(unlist(lapply(strsplit(gsub("]", "", mat$X), "[", fixed = TRUE), function(x) x[2])))
m <- x$maturity
m[mat$ix] <- (mat$mean > 1.5) + 1
m[m == "primiparous"] <- 1
m[m == "multiparous"] <- 2
m[m == "senile"] <- 2
m <- as.numeric(m)

# Regression coeffcients:
beta      <- read.table("bugs/results/single beta/beta.txt", sep = "\t")[,2]
log.alpha <- read.table("bugs/results/single beta/log.alpha.txt", sep = "\t")
ix        <- 1:which(diff(log.alpha[,1]) < 0)
iy        <- (which(diff(log.alpha[,1]) < 0)+1):nrow(log.alpha)
log.alpha <- data.frame(primiparous = log.alpha[ix,2], multiparous = log.alpha[iy,2])

# Read Hiatt parameters:
intercept.hiatt <- read.table("bugs/results/double beta/intercept.hiatt.txt", sep = "\t")[,2]
slope.hiatt     <- read.table("bugs/results/double beta/slope.hiatt.txt", sep = "\t")[,2]

xlab = c(0.98, 1.18)
ylab = c(0, 10)
png(file = "figures/Hiatt growth parameters.png", width = 7, height = 7, res = 500, units = "in")
plot(xlab, ylab, type = "n",  xlab = "", ylab = "", xaxs = "i", yaxs = "i")
ss <- seq(xlab[1], xlab[2], by = 0.002)
ii <- seq(ylab[1], ylab[2], by = 0.02)
sss <- repvec(ss, nrow = length(ii))
iii <- repvec(ii, ncol = length(ss))
xx <- 70
growth <- ((sss - 1) * xx + iii) / xx
levels <- seq(-2, 36, by = 2)
cols <- colorRampPalette(c("#88CCEE", "white", "brown1"))(length(levels))
.filled.contour(ss, ii, 100 * t(growth), levels = levels, col = cols)
grid()
contour(ss, ii, 100 * t(growth), add = TRUE, levels = levels, labels = paste0(levels, " %"), labcex = 0.9, col = "grey50")
points(jitter(slope.hiatt, amount = 0.0005), intercept.hiatt, pch = 21, 
       bg = fade("grey30", 0.5), col = NA, cex = 0.5)

mtext("Hiatt slope parameter", 1, 2.5, cex = 1.25, font = 2)
mtext("Hiatt intercept parameter", 2, 2.5, cex = 1.25, font = 2)

points(1.071, 5.099, pch = 21, bg = "green3", cex = 1.25, col = "grey50")
points(1.160, 4.475, pch = 21, bg = "green3", cex = 1.25, col = "grey50")
points(1.106, 8.421, pch = 21, bg = "green3", cex = 1.25, col = "grey50")

box(col = "grey50")
dev.off()

# Reverse growth analysis:
hiatt <- c(1.071, 5.099)  # Sainte-Marie.
#hiatt <- c(1.106, 8.421)
#hiatt <- c(1.160, 4.475)
hiatt <- c(1.021, 5.74)  # Fecundity model fit.

hiatt <- c(0.992, 4.498)  # Fecundity model fit.
names(hiatt) <- c("slope", "intercept")
cw <- (x$carapace.width[m == 1] - hiatt["intercept"]) / hiatt["slope"]

png(file = "figures/Primiparous fecundity pre-moult size.png", width = 7, height = 7, res = 500, units = "in")
plot(log(c(30, 100)), log(c(5000, 150000)), col = "grey", 
     xlim = log(c(30, 100)), ylim = log(c(6000, 150000)), xlab = "", ylab = "", 
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", type = "n")
xscale <- seq(30, 100, by = 10)
axis(1, at = log(xscale), labels = xscale)
yscale <- seq(10000, 150000, by = 10000)
axis(2, at = log(yscale), labels = yscale / 1000)
vline(log(xscale), lty = "dashed", lwd = 0.5, col = "grey70")
hline(log(yscale), lty = "dashed", lwd = 0.5, col = "grey70")
points(log(x$carapace.width)[m == 2], log(x$fecundity)[m == 2], pch = 21, cex = 0.3, bg = "grey50", col = NA) 

points(log(cw), log(x$fecundity)[m == 1], pch = 21, cex = 0.3, bg = "green2", col = NA) 
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)     
mtext("Fecundity (x1000 eggs)", 2, 2.75, cex = 1.25, font = 2)     


abline(mean(log.alpha$multiparous), mean(beta))

legend("topleft", 
       legend = c("Multiparous", "Primiparous pre-moult size"),
       pch = 21, cex = 1.25, pt.cex = 2, 
       pt.bg = c("grey50", "green3"), col = "grey50", box.col = "grey50")
box(col = "grey50")
dev.off()

# Calculate how Hiatt de-growth compares to multiparous fecundity:
res <- log(x$fecundity[m == 1]) - (mean(beta) * log(cw) + mean(log.alpha$multiparous))

# Mikio's lab growth data:
b <- read.csv("data/raw/female moult data lab.csv")
png(file = "figures/Mikio lab growth data.png", width = 7, height = 7, res = 500, units = "in")
plot(b$Before, b$After - b$Before, xlim = c(20, 75), xaxs = "i", ylim = c(0, 20), yaxs = "i", 
     xlab = "", ylab = "", lwd = 0.5, col = fade("grey20", 0.75), cex = 0.85)
grid()
points(b$Before[b$Moult == "M"], b$After[b$Moult == "M"] - b$Before[b$Moult == "M"], pch = 21, cex = 0.85, bg = fade("red", 0.5), lwd = 0.5, col = NA)
points(b$Before[b$Moult == "I"], b$After[b$Moult == "I"] - b$Before[b$Moult == "I"], pch = 21, cex = 0.85, bg = fade("green3", 0.5), lwd = 0.5, col = NA)
legend("topleft", 
       legend = c("Immature-to-immature", 
                  "Immature-to-Mature", 
                  "Alunno-Bruscia immature-to-pubescent (1998)",
                  "Alunno-Bruscia immature-to-mature (1998)", 
                  "Fecundity retro-analysis"),
       pch = c(21, 21, NA, NA, NA),
       lwd = c(NA, NA, 2, 2, 2),
       col = c("grey20", "grey20", "green3", fade("red3", 0.75), fade("red3", 0.75)),
       lty = c(NA, NA, "dashed", "dashed", "solid"),
       pt.bg = c(fade(c("green3", "red"), 0.5), NA, NA, NA),
       pt.cex = 2, box.lwd = 0.5, box.col = "grey50")

mtext("Pre-moult carapace width (mm)", 1, 2.5, cex = 1.2, font = 2)
mtext("Growth increment (mm)", 2, 2.5, cex = 1.2, font = 2)

# Growth rate are very small compared to published sources:
abline(5.099, 0.071, col = "red3", lwd = 2, lty = "dashed")  # Alunno-Bruscia (1998).
abline(5.9267, 0.0204, col = "red3", lwd = 2)                # From fecundity retro-analysis.

#lines(20:30, -0.227 + 0.429 * (20:30), col = "green3", lwd = 2, lty = "dashed")  # Alunno-Bruscia immature (1998).
lines(30:50, 2.864 + 0.246 * (30:50), col = "green3", lwd = 2, lty = "dashed")   # Alunno-Bruscia prepubescent epsilon(1998).

# Pubescent growth from instar analysis:
#lines(30:50, 4.475 + 0.160 * (30:50), col = "green3", lwd = 2, lty = "dashed")   # Alunno-Bruscia prepubescent epsilon(1998).
# Immature growth from instar analysis:
#lines(10:30, 0.245 + 0.432 * (10:30), col = "green3", lwd = 2, lty = "dashed")   # Alunno-Bruscia prepubescent epsilon(1998).

# Relative growth:
png(file = "figures/Mikio lab relative growth.png", width = 7, height = 7, res = 500, units = "in")
plot(c(20, 75), c(0, 40), xaxs = "i", yaxs = "i", 
     xlab = "", ylab = "", lwd = 0.5, col = fade("grey20", 0.75), cex = 0.85)
hline(seq(0, 40, by = 5), lty = "dashed", lwd = 0.5, col = fade("grey50"))
vline((1:8)*10, lty = "dashed", lwd = 0.5, col = fade("grey50"))
points(b$Before[b$Moult == "M"], 100 * (b$After[b$Moult == "M"] - b$Before[b$Moult == "M"])/b$Before[b$Moult == "M"], pch = 21, cex = 0.85, bg = fade("red", 0.5), lwd = 0.5, col = NA)
points(b$Before[b$Moult == "I"], 100 * (b$After[b$Moult == "I"] - b$Before[b$Moult == "I"])/b$Before[b$Moult == "I"], pch = 21, cex = 0.85, bg = fade("green3", 0.5), lwd = 0.5, col = NA)
legend("topright", 
       legend = c("Immature-to-immature", 
                  "Immature-to-Mature", 
                  "Alunno-Bruscia immature-to-pubescent (1998)",
                  "Alunno-Bruscia immature-to-mature (1998)", 
                  "Fecundity retro-analysis"),
       bg = NA, 
       pch = c(21, 21, NA, NA, NA),
       lwd = c(NA, NA, 2, 2, 2),
       col = c("grey20", "grey20", "green3", fade("red3", 0.75), fade("red3", 0.75)),
       lty = c(NA, NA, "dashed", "dashed", "solid"),
       pt.bg = c(fade(c("green3", "red"), 0.5), NA, NA, NA),
       pt.cex = 2, box.lwd = 0.5, box.col = "grey50")

mtext("Pre-moult carapace width (mm)", 1, 2.5, cex = 1.2, font = 2)
mtext("Relative growth increment (%)", 2, 2.5, cex = 1.2, font = 2)

# Growth rate are very small compared to published sources:
xx <- seq(20, 75, len = 1000)
lines(xx, 100 * (0.071 + 5.099/xx), col = "red3", lwd = 2, lty = "dashed")   # Alunno-Bruscia (1998).
lines(xx, 100 * (0.0204 + 5.9267/xx), col = "red3", lwd = 2, lty = "solid")  # From fecundity retro-analysis.
lines(xx, 100 * (0.246 + 2.864/xx), col = "green3", lwd = 2, lty = "dashed") # Alunno-Bruscia prepubescent epsilon(1998).

dev.off()

# Theoretical difference curve between primiparous and multiparous:
png(file = "figures/Fecundity and growth diagram.png", width = 7, height = 7, res = 500, units = "in")

f <- function(b = 1.1, beta = 3) return(1-1/(b^beta))       # Reduction as a function of growth rate.
g <- function(r = 1.1, beta = 3) return(1/((1-r)^(1/beta))) # Growth rate as a function of reduction. 

b <- seq(1, 1.2, by = 0.01)
plot(b, 100 * f(b), type = "n", xlab = "", ylab = "", lwd = 1.5, xaxs = "i", yaxs = "i", xaxt = "n")
grid()
lines(b, 100 * f(b, beta = 3), type = "l", xlab = "", ylab = "", lwd = 2)
#lines(b, 100 * f(b, beta = 2.7), type = "l", xlab = "", ylab = "", lwd = 1.5)
mtext("Pubsecent growth rate (%)", 1, 2.5, font = 2, cex = 1.25)
mtext("Primiparous fecundity reduction (%)", 2, 2.5, font = 2, cex = 1.25)
axis(1, at = seq(0, 1.2, by = 0.05), labels = 100*(seq(0, 1.2, by = 0.05)-1))

polygon(c(0, 1.11, 1.11, 1.14, 1.14, 0, 0), 
        100 * c(f(1.11), f(1.11), 0, 0, f(1.14), f(1.14), f(1.11)), 
        col = fade("green2", 0.25), border = fade("green2", 0.5))

polygon(c(0, 1.16, 1.16, 1.18, 1.18, 0, 0), 
        100 * c(f(1.16), f(1.16), 0, 0, f(1.18), f(1.18), f(1.16)), 
        col = fade("red", 0.25), border = fade("red", 0.5))

lines(c(0, g(0.3), g(0.3)), 100 * c(0.3, 0.3, 0), col = fade("green2", 0.75), lwd = 2, lty = "dashed")
lines(c(0, g(0.2), g(0.2)), 100 * c(0.2, 0.2, 0), col = fade("red"), lwd = 2, lty = "dashed")
text(1+(g(0.2)-1)/2, 100*0.2, "Fecundity study -  \n Sainte-Marie (1993)")
text(1+(g(0.3)-1)/2, 100*0.3, "Fecundity study -  \n Moriyasu")

#text(1+(g(0.3)-1)/2, 100*0.3, "Lab growth data \n Mikio")
text(1+(1.18-1)/2, 100*f(1.18), "Lab growth data (Alunno-Bruscia et al. 1998)", pos = 3)
text(1+(1.14-1)/2, 100*f(1.14), "Lab growth data (Moriyasu)", pos = 3)

# x-axis labels:
mtext(paste0(round(100*(g(0.2)-1),1), "%"), 1, 0, at = g(0.2), cex = 0.8, font = 2, col = "red")
mtext("11%", 1, 0, at = 1.11, cex = 0.8, font = 2, col = "green2")
mtext("14%", 1, 0, at = 1.14, cex = 0.8, font = 2, col = "green2")
mtext("16%", 1, 0, at = 1.16, cex = 0.8, font = 2, col = "red")
mtext("18%", 1, 0, at = 1.18, cex = 0.8, font = 2, col = "red")

# y-axis labels:
mtext(paste0(round(100*f(1.11),0), "%"), 2, 0.25, at = 100*f(1.11), cex = 0.8, font = 2, col = "green2")
mtext(paste0(round(100*f(1.14),1), "%"), 2, 0.25, at = 100*f(1.14), cex = 0.8, font = 2, col = "green2")
mtext(paste0(round(100*f(1.16),0), "%"), 2, 0.25, at = 100*f(1.16), cex = 0.8, font = 2, col = "red")
mtext(paste0(round(100*f(1.18),0), "%"), 2, 0.25, at = 100*f(1.18), cex = 0.8, font = 2, col = "red")

box(col = "grey50")
dev.off()


# Assuming that the growth follows an allometric, rather than linear function, calculate its coefficients:
path <- "bugs/results/double beta/"
beta      <- read.table(paste0(path, "beta.txt"), sep = "\t")
ix        <- 1:which(diff(beta[,1]) < 0)
iy        <- (which(diff(beta[,1]) < 0)+1):nrow(beta)
beta <- data.frame(primiparous = beta[ix,2], multiparous = beta[iy,2])
log.alpha <- read.table(paste0(path, "log.alpha.txt"), sep = "\t")
ix        <- 1:which(diff(log.alpha[,1]) < 0)
iy        <- (which(diff(log.alpha[,1]) < 0)+1):nrow(log.alpha)
log.alpha <- data.frame(primiparous = log.alpha[ix,2], multiparous = log.alpha[iy,2])

beta.growth  <- beta$multiparous / beta$primiparous
alpha.growth <- (exp(log.alpha$multiparous) / exp(log.alpha$primiparous))^(1/beta$primiparous)
alpha.growth <- mean(alpha.growth)
beta.growth  <- mean(beta.growth)

xx <- 40:60
names(xx) <- 40:60
alpha.growth * xx ^ beta.growth


r <- data.frame(cw = 40:80)
r$cw.premoult <- round((r$cw / alpha.growth)^(1/beta.growth),1)
r$primiparous <- round(mean(exp(log.alpha$primiparous)) * r$cw ^ mean(beta$primiparous))
r$multiparous <- round(mean(exp(log.alpha$multiparous)) * r$cw ^ mean(beta$multiparous))
r$multi.pre <- round(mean(exp(log.alpha$multiparous)) * r$cw.premoult ^ mean(beta$multiparous))

mat

lines(r$cw.premoult, r$cw - r$cw.premoult, lwd = 2, col = "red")

(xx / alpha.growth) ^ (1/beta.growth)

box(col = "grey50")


plot(x$carapace.width, x$fecundity, cex = 0.1)
#points(x$carapace.width[m == 1], x$fecundity[m == 1], pch = 21, bg = "green3", cex = 0.5, lwd = 0.5, col = fade("grey20"))
#points(x$carapace.width[m == 2], x$fecundity[m == 2], pch = 21, bg = "red3", cex = 0.5, lwd = 0.5, col = fade("grey20"))

xx <- round((x$carapace.width[m == 1] / alpha.growth)^(1/beta.growth),1)
points(xx, x$fecundity[m == 1], pch = 21, bg = fade("red2"), cex = 0.25, lwd = 0.5, col = fade("grey20"))


dev.off()
