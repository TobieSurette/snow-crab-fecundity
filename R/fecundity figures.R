
abline(-1.3, 2.9, col = "red")
ix <- which(log(x$fecundity) > (2.9 * log(x$carapace.width) - 1.3))
plot(log(x$carapace.width), log(x$fecundity), ylim = c(9, 12), cex = 0.3)
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], col = "red", cex = 0.5)
points(log(x$carapace.width)[-ix], log(x$fecundity)[-ix], col = "green2", cex = 0.25)

plot(log(x$carapace.width), log(x$fecundity), ylim = c(9, 12), cex = 0.4)
abline(-1.27, 2.931, col = "red")
abline(-1.59, 2.931, col = "red")
abline(-1.113, 2.891, col = "green2", lwd = 2)
abline(-1.113-2*0.117, 2.891, col = "green2", lwd = 1, lty = "dashed")
abline(-1.113+2*0.117, 2.891, col = "green2", lwd = 1, lty = "dashed")
abline(-1.455, 2.891, col = "green2", lwd = 2)
abline(-1.455-2*0.117, 2.891, col = "green2", lwd = 1, lty = "dashed")
abline(-1.455+2*0.117, 2.891, col = "green2", lwd = 1, lty = "dashed")


png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/fecundity by shell condition.png",
    width = 7, height = 7, res = 500, units = "in")
plot(log(x$carapace.width), log(x$fecundity), pch = 21, bg = "grey50", col = NA, 
     xlim = c(3.6, 4.6), ylim = c(9, 12), xaxs = "i", yaxs = "i", cex = 0.4, xlab = "", ylab = "")
grid()


ix <- which(substr(x$shell.condition,1,1) %in% c("1"))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, bg = fade("red"), col = NA, cex = 0.5)

ix <- which(substr(x$shell.condition,1,1) %in% c("2"))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, bg = fade("green3"), col = NA, cex = 0.5)

ix <- which(substr(x$shell.condition,1,1) %in% c("3"))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, bg = fade("blue"), col = NA, cex = 0.5)

ix <- which(substr(x$shell.condition,1,1) %in% c("4"))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, bg = fade("orange"), col = NA, cex = 0.5)

ix <- which(substr(x$shell.condition,1,1) %in% c("5"))
points(log(x$carapace.width)[ix], log(x$fecundity)[ix], pch = 21, bg = fade("yellow"), col = NA, cex = 0.5)

mtext("ln(carapace width)", 1, 2.5, cex = 1.25, font = 2)
mtext("ln(fecundity)", 2, 2.5, cex = 1.25, font = 2)
legend("topleft",
       legend = paste("shell condition", 1:5),
       pt.bg = fade(c("red", "green3", "blue", "orange", "yellow"), 0.8),
       col = NA, 
       pch = 21, pt.cex = 2)


box(col = "grey50")
dev.off()

