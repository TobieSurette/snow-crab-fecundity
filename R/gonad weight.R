# Egg mass plot:

x <- x[which(x$egg.mass %in% 1:2), ]

tmp <- aggregate(x$egg.mass, by = x["year"], function(x) sum(x == 2) / length(x))

png("C:/Users/SuretteTJ/Desktop/github/lobster-biology/results/half-clutch curves by year at-sea sampling.png",
    units = "in", height = 5.5, width = 7, res = 500)

gbarplot(100 * tmp[,2], tmp[,1], grid = TRUE)
mtext("Proportion of reduced-clutches (%)", 2, 2.5, cex = 1.25, font = 2)
mtext("Year", 1, 2.5, cex = 1.25, font = 2)

dev.off()


ix <- which((m == 1) & (x$shell.condition == 3))
table( x$month, x$location, !is.na(x$gonad.weight))

# Primiparous:
png(file = "figures/gonad weight by spawning stage & shell condition.png", units = "in", width = 7, height = 7, res = 500)
plot(x$carapace.width, x$gonad.weight, cex = 0.4, xlim = c(40, 90), ylim = c(0, 20), 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", col = "grey60")
grid()
ix <- which(!is.na(x$gonad.weight) & (m == 1) & (x$shell.condition == 2))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = fade("green3"), col = "green", cex = 1, lwd = 0.25)
ix <- which(!is.na(x$gonad.weight) & (m == 1) & (x$shell.condition == 3))
ix <- which(!is.na(x$gonad.weight) & (m == 1) & (x$shell.condition == 3)) 
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 22, bg = fade("red3"), col = "red", cex = 1, lwd = 0.25)
ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 3))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 23, bg = fade("blue"), col = "blue", cex = 1, lwd = 0.25)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.25)
mtext("Gonad weight (g)", 2, 2.5, font = 2, cex = 1.25)
legend("topleft",
       legend = c("Other data", "Primiparous SC2", "Primiparous SC3", "Multiparous SC3"),
       pch = c(1, 21:23),
       cex = 1.25, 
       pt.cex = c(1, 2, 2, 2),
       pt.bg = c(NA, fade(c("green3", "red3", "blue"))),
       col = c("grey50", "green3", "red3", "blue"),
       bg = NA)

box(col = "grey50")
dev.off()

# Multiparous:
plot(x$carapace.width, x$gonad.weight, cex = 0.5, xlim = c(40, 90), ylim = c(0, 20), xaxs = "i", yaxs = "i")
grid()
ix <- which((m == 2) & (x$shell.condition == 3))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = "red", col = "red", cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4))
table(x$location[ix], x$month[ix])

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 5))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = fade("red3", 0.25), col = fade("red", 0.25), cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 6))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = fade("red3", 0.5), col = fade("red", 0.25), cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 7))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = fade("pink", 0.75), col = fade("pink", 0.25), cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 9))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = "blue", col = "blue", cex = 1, lwd = 0.5)


box(col = "grey50")


ix <- which(!is.na(x$gonad.weight) & (m == 1))
table(x$location[ix], x$month[ix])

plot(x$carapace.width, x$gonad.weight, cex = 0.5, xlim = c(40, 90), ylim = c(0, 20), xaxs = "i", yaxs = "i")
grid()
ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = "green3", col = "green", cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4))
table(x$location[ix], x$month[ix])

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 5))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = fade("red3", 0.25), col = fade("red", 0.25), cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 6))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = fade("red3", 0.5), col = fade("red", 0.25), cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 7))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = fade("pink", 0.75), col = fade("pink", 0.25), cex = 1, lwd = 0.5)

ix <- which(!is.na(x$gonad.weight) & (m == 2) & (x$shell.condition == 4) & (x$month == 9))
points(x$carapace.width[ix], x$gonad.weight[ix], pch = 21, bg = "blue", col = "blue", cex = 1, lwd = 0.5)


box(col = "grey50")


plot(log(x$carapace.width), log(x$gonad.weight), pch = 21, bg = "grey70", 
     xlim = c(3.8, 4.5), ylim = c(-1, 2.5), col = "grey50", cex = 0.5, lwd = 0.5)
ix <- x$month == 5
points(log(x$carapace.width[ix]), log(x$gonad.weight[ix]), pch = 21, bg = "green", col = "green", cex = 1, lwd = 0.5)

ix <- x$month == 6
points(log(x$carapace.width[ix]), log(x$gonad.weight[ix]), pch = 21, bg = "orange", col = "orange", cex = 1, lwd = 0.5)

ix <- x$month == 7
points(log(x$carapace.width[ix]), log(x$gonad.weight[ix]), pch = 21, bg = "red", col = "red", cex = 1, lwd = 0.5)

ix <- x$month == 8
points(log(x$carapace.width[ix]), log(x$gonad.weight[ix]), pch = 21, bg = "skyblue", col = "grey50", cex = 1, lwd = 0.5)

ix <- x$month == 9
points(log(x$carapace.width[ix]), log(x$gonad.weight[ix]), pch = 21, bg = "blue", col = "blue", cex = 1, lwd = 0.5)

ix <- x$month == 10
points(log(x$carapace.width[ix]), log(x$gonad.weight[ix]), pch = 21, bg = "purple", col = "purple", cex = 1, lwd = 0.5)

plot(x$carapace.width, x$gonad.weight / x$carapace.width^3, cex = 0.5)
ix <- x$month == 5
points(x$carapace.width[ix], (x$gonad.weight / x$carapace.width^3)[ix], pch = 21, bg = "green", col = "green", cex = 1, lwd = 0.5)

ix <- x$month == 6
points(x$carapace.width[ix], (x$gonad.weight / x$carapace.width^3)[ix], pch = 21, bg = "orange", col = "orange", cex = 1, lwd = 0.5)

ix <- x$month == 7
points(x$carapace.width[ix], (x$gonad.weight / x$carapace.width^3)[ix], pch = 21, bg = "red", col = "red", cex = 1, lwd = 0.5)

ix <- x$month == 8
points(x$carapace.width[ix], (x$gonad.weight / x$carapace.width^3)[ix], pch = 21, bg = "skyblue", col = "grey50", cex = 1, lwd = 0.5)

ix <- x$month == 9
points(x$carapace.width[ix], (x$gonad.weight / x$carapace.width^3)[ix], pch = 21, bg = "blue", col = "blue", cex = 1, lwd = 0.5)

ix <- x$month == 10
points(x$carapace.width[ix], (x$gonad.weight / x$carapace.width^3)[ix], pch = 21, bg = "purple", col = "purple", cex = 1, lwd = 0.5)

