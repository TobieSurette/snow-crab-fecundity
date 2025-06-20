

x$egg.mass <- 1000000 * x$sample.egg.weight / x$sample.egg.count

png(file = "figures/Dry egg mass primiparous SC3.png", width = 7, height = 7, res = 500, units = "in")

plot(c(0, 0.35), c(40, 80), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")

hline(seq(40, 80, by = 5), lwd = 0.5, col = fade("grey40"), lty = "dashed")
vline(seq(0, 0.35, by = 0.05), lwd = 0.5, col = fade("grey40"), lty = "dashed")

points(x$fecundity / x$carapace.width^3, x$egg.mass, xaxs = "i", yaxs = "i", xlab = "", ylab = "", 
       ylim = c(40, 80), pch = 21, cex = 0.5, bg = fade("grey", 0.25), col = fade("grey30", 0.25), lwd = 0.5)

mtext(expression(paste("Dry egg weight (", mu, "g)")), 2, 2.5, font = 2, cex = 1.25)
mtext(expression('Fecundity / CW' ^ 3), 1, 2.75, font = 2, cex = 1.25)


#ix <- which((m == 1) & (x$shell.condition == 2))
#points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
#       bg = fade("red"), col = fade("grey30"), lwd = 0.5, cex = 0.75)

ix <- which((m == 1) & (x$shell.condition == 3))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("red"), col = fade("grey30"), lwd = 0.5, cex = 0.5)

ix <- which((m == 2) & (x$shell.condition == 3))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("green"), col = fade("grey30"), lwd = 0.5, cex = 0.7)

ix <- which((m == 1) & (x$egg.colour == 3))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("red"), col = fade("grey30"), lwd = 0.5, cex = 0.5)



ix <- which((m == 2) & (x$egg.colour == "light orange"))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("green"), col = fade("grey30"), lwd = 0.5, cex = 0.7)

ix <- which((m == 2) & (x$egg.colour == "dark orange"))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("red"), col = fade("grey30"), lwd = 0.5, cex = 0.7)

ix <- which((m == 2) & (x$egg.colour == "light orange"))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("green"), col = fade("grey30"), lwd = 0.5, cex = 0.7)

ix <- which((m == 2) & (x$egg.colour == "dark orange"))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("red"), col = fade("grey30"), lwd = 0.5, cex = 0.7)

ix <- which((x$egg.colour == "light orange"))
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("green"), col = fade("grey30"), lwd = 0.5, cex = 0.7)

ix <- which (x$egg.colour == "dark orange")
points(x$fecundity[ix] / x$carapace.width[ix]^3, x$egg.mass[ix], pch = 21,
       bg = fade("red"), col = fade("grey30"), lwd = 0.5, cex = 0.7)

legend("topright", 
       legend = c("Primiparous SC3", "Multiparous SC3"),
       pch = 21, pt.bg = fade(c("red", "green")), pt.lwd = 0.5, pt.cex = 2,
       box.col = "grey50", box.lwd = 0.5)


box(col = "grey50")
dev.off()



