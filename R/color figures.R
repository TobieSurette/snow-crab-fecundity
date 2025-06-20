y <- x[which(!is.na(x$egg.color.a) | !is.na(x$gonad.color.a) | !is.na(x$hepato.color.a)), ]

iy <- which((y$project == "OERA") & (y$gear == "cage"))


# Egg color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/egg color Lb.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(20, 60), c(-5, 60), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$egg.color.a)
Lab <- cbind(y$egg.color.l[ix]+10, y$egg.color.a[ix], y$egg.color.b[ix])
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$egg.color.l[ix], y$egg.color.b[ix], pch = 21, bg = fade(cols), col = NA, cex = 1.25)
mtext("Color meter L", 1, 2.5, font = 2, cex = 1.5)
mtext("Color meter b", 2, 2.5, font = 2, cex = 1.5)
mtext("Egg color", 3, 0.5, font = 2, cex = 1.5)
text(55, 35, "Light orange", pos = 3, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(33, 25, "Dark orange", pos = 1, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(30, -1, "Black eggs", pos = 2, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(39, 45, "Stages 1-3", pos = 2, font = 4, srt = 0, col = "grey40")
text(35, 26, "Stages 4-9", pos = 2, font = 4, srt = 0, col = "grey40")
text(28, 13.5, "Stages 9-10", pos = 2, font = 4, srt = 0, col = "grey40")
text(29, 6, "Stages 10-13", pos = 1, font = 4, srt = 0, col = "grey40")

iy <- which((y$project == "OERA") & (y$gear == "cage"))
points(y$egg.color.l[iy], y$egg.color.b[iy], col = "green")

#y$egg.development == "NF"
#iy <- which(y$egg.development == "NF")
#iy <- which(y$egg.development == "0")
#points(y$egg.color.l[iy], y$egg.color.b[iy], pch = 21, bg = "green", col = NA, cex = 1.25)

#box(col = "grey50")

#iy <- which(y$egg.development[ix] %in% c("1-3", "2", "3"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "red", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("4-5", "5", "5-6", "6", "6-7"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "green2", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("7"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "blue", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("8"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "yellow3", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("9-10"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "purple", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("10", "11", "12", "13"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "orange", col = NA, cex = 1.25)
table(y$egg.development[ix])

dev.off()


# Egg color with gonad color
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/egg color by gonad color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(20, 60), c(-5, 60), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$gonad.color.a)
Lab <- cbind(y$gonad.color.l[ix]+10, y$gonad.color.a[ix], y$gonad.color.b[ix])
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$egg.color.l[ix], y$egg.color.b[ix], pch = 21, bg = fade(cols), col = NA, cex = 1.25)
mtext("Color meter L", 1, 2.5, font = 2, cex = 1.5)
mtext("Color meter b", 2, 2.5, font = 2, cex = 1.5)
mtext("Egg color but coloured by gonad color", 3, 0.5, font = 2, cex = 1.5)
text(55, 35, "Beige gonads", pos = 3, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(33, 25, "Orange gonads", pos = 1, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(33, 10, "Reddish gonads", pos = 2, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(40, 47.5, "Weird marroon gonads", pos = 2, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(39, 45, "Stages 1-3", pos = 2, font = 4, srt = 0, col = "grey40")
text(35, 26, "Stages 4-9", pos = 2, font = 4, srt = 0, col = "grey40")
text(28, 13.5, "Stages 9-10", pos = 2, font = 4, srt = 0, col = "grey40")
text(29, 6, "Stages 10-13", pos = 1, font = 4, srt = 0, col = "grey40")

iy <- which((y$project == "OERA") & (y$gear == "cage"))
points(y$egg.color.l[iy], y$egg.color.b[iy], col = "green")

#y$egg.development == "NF"
#iy <- which(y$egg.development == "NF")
#iy <- which(y$egg.development == "0")
#points(y$egg.color.l[iy], y$egg.color.b[iy], pch = 21, bg = "green", col = NA, cex = 1.25)

#box(col = "grey50")

#iy <- which(y$egg.development[ix] %in% c("1-3", "2", "3"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "red", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("4-5", "5", "5-6", "6", "6-7"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "green2", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("7"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "blue", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("8"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "yellow3", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("9-10"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "purple", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("10", "11", "12", "13"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "orange", col = NA, cex = 1.25)
table(y$egg.development[ix])

iy <- y$gear[ix] == "Cage"
points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], col = "green")

iy <- y$project[ix] == "OERA"
points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], col = "green")

iy <- y$location[ix] == "Baie des Chaleurs"  
points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], col = "green")

iy <- y$location[ix] == "Cheticamp"  
points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], col = "red")

iy <- y$gear[ix] == "Trap"  
points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], col = "purple")
dev.off()

# Egg color with hepato color
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/egg color by hepato color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(20, 60), c(-5, 60), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$hepato.color.a)
Lab <- cbind(y$hepato.color.l[ix]+10, y$hepato.color.a[ix], y$hepato.color.b[ix])
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$egg.color.l[ix], y$egg.color.b[ix], pch = 21, bg = fade(cols), col = NA, cex = 1.25)
mtext("Color meter L", 1, 2.5, font = 2, cex = 1.5)
mtext("Color meter a", 2, 2.5, font = 2, cex = 1.5)
mtext("Egg color but coloured by hepato color", 3, 0.5, font = 2, cex = 1.5)
text(55, 35, "Light orange", pos = 3, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(33, 25, "Dark orange", pos = 1, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(30, -1, "Black eggs", pos = 2, font = 2, srt = 0, col = "grey40", cex =  1.25)
text(39, 45, "Stages 1-3", pos = 2, font = 4, srt = 0, col = "grey40")
text(35, 26, "Stages 4-9", pos = 2, font = 4, srt = 0, col = "grey40")
text(28, 13.5, "Stages 9-10", pos = 2, font = 4, srt = 0, col = "grey40")
text(29, 6, "Stages 10-13", pos = 1, font = 4, srt = 0, col = "grey40")

iy <- which((y$project == "OERA") & (y$gear == "cage"))
points(y$egg.color.l[iy], y$egg.color.b[iy], col = "green")
iy <- which((y$project == "OERA") & (y$gear == "trap"))
points(y$egg.color.l[iy], y$egg.color.b[iy], col = "red")

#y$egg.development == "NF"
#iy <- which(y$egg.development == "NF")
#iy <- which(y$egg.development == "0")
#points(y$egg.color.l[iy], y$egg.color.b[iy], pch = 21, bg = "green", col = NA, cex = 1.25)

#box(col = "grey50")

#iy <- which(y$egg.development[ix] %in% c("1-3", "2", "3"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "red", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("4-5", "5", "5-6", "6", "6-7"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "green2", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("7"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "blue", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("8"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "yellow3", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("9-10"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "purple", col = NA, cex = 1.25)
#iy <- which(y$egg.development[ix] %in% c("10", "11", "12", "13"))
#points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy], pch = 21, bg = "orange", col = NA, cex = 1.25)
table(y$egg.development[ix])

dev.off()

table(y$egg.color[ix])
iy <- y$egg.color[ix] == "coccoon"
points(y$egg.color.l[ix][iy], y$egg.color.b[ix][iy])


# Gonad color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/gonad color ab.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(-5, 50), c(5, 60), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$gonad.color.a)
Lab <- cbind(y$gonad.color.l[ix]+10, y$gonad.color.a[ix], y$gonad.color.b[ix])
#Lab[,1] <- 40
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$gonad.color.a[ix], y$gonad.color.b[ix], pch = 21, bg = fade(cols, 0.6), col = NA, cex = 0.9*sqrt(y$gonad.weight[ix]))
mtext("Color meter a", 1, 2.5, font = 2, cex = 1.5)
mtext("Color meter b", 2, 2.5, font = 2, cex = 1.5)
mtext("Gonad color", 3, 0.5, font = 2, cex = 1.5)
text(40, 50, "Orange", pos = 4, font = 2, col = "grey40")
text(3, 11, "Beige", pos = 1, font = 2, col = "grey40")
text(12, 33, "Light orange", pos = 3, font = 2, srt = 49, col = "grey40")
text(35, 21.5, "Reddish orange \n with late-stage eggs", pos = 1, font = 2, srt = 0, col = "grey40", cex = 0.8)
text(20, 20, "Marroon with \n early-stage eggs", pos = 1, font = 2, srt = 0, col = "grey40", cex = 0.8)
box(col = "grey50")

iy <- which((y$project == "OERA") & (y$gear == "cage"))
points(y$gonad.color.a[iy], y$gonad.color.b[iy], col = "green")

#iy <- y$project[ix] == "OERA"
#points(y$gonad.color.a[ix][iy], y$gonad.color.b[ix][iy], col = "green")

dev.off()

# Gonad weight by color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/gonad weight by color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(40, 90), c(0, 15), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$gonad.color.a)
Lab <- cbind(y$gonad.color.l[ix]+10, y$gonad.color.a[ix], y$gonad.color.b[ix])
#Lab[,1] <- 40
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$carapace.width[ix], y$gonad.weight[ix], pch = 21, bg = fade(cols, 0.5), col = NA, cex = 1.15)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.5)
mtext("Gonad weight (g)", 2, 2.5, font = 2, cex = 1.5)
text(65, 0.5, "Beige gonads", font = 2, col = "grey40")
text(65, 7.5, "Large orange gonads", font = 2, pos = 3, col = "grey40", srt = 45)
text(65, 2.25, "Small orange gonads", font = 2, pos = 3, col = "grey40", srt = 20)
box(col = "grey50")
dev.off()

# Gonad weight by egg color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/gonad weight by egg color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(40, 90), c(0, 15), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$egg.color.a)
Lab <- cbind(y$egg.color.l[ix]+10, y$egg.color.a[ix], y$egg.color.b[ix])
#Lab[,1] <- 40
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$carapace.width[ix], y$gonad.weight[ix], pch = 21, bg = fade(cols, 0.5), col = NA, cex = 1.15)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.5)
mtext("Gonad weight (g)", 2, 2.5, font = 2, cex = 1.5)
#text(65, 0.5, "Beige gonads", font = 2, col = "grey40")
text(65, 7.5, "Dark orange eggs", font = 2, pos = 3, col = "grey40", srt = 45)
text(65, 2.25, "Light orange eggs", font = 2, pos = 3, col = "grey40", srt = 20)
box(col = "grey50")
dev.off()

# Gonad weight by hepato color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/gonad weight by hepato color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(40, 90), c(0, 15), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$hepato.color.a)
Lab <- cbind(y$hepato.color.l[ix]+10, y$hepato.color.a[ix], y$hepato.color.b[ix])
#Lab[,1] <- 40
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$carapace.width[ix], y$gonad.weight[ix], pch = 21, bg = fade(cols, 0.5), col = NA, cex = 1.15)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.5)
mtext("Gonad weight (g)", 2, 2.5, font = 2, cex = 1.5)
#text(65, 0.5, "Beige gonads", font = 2, col = "grey40")
text(65, 7.5, "Light hepatopancreas?", font = 2, pos = 3, col = "grey40", srt = 45)
text(65, 2.25, "Darker hepatopancreas?", font = 2, pos = 3, col = "grey40", srt = 20)
box(col = "grey50")
dev.off()

table(y$egg.color[ix])
iy <- y$gonad.color[ix] == "orange"
points(y$gonad.color.a[ix][iy], y$gonad.color.b[ix][iy])


# Hepato color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/hepato color la.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(15, 60), c(0, 15), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$hepato.color.a)
Lab <- cbind(y$hepato.color.l[ix]+10, y$hepato.color.a[ix], y$hepato.color.b[ix])
#Lab[,1] <- 40
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$hepato.color.l[ix], y$hepato.color.a[ix], pch = 21, bg = fade(cols, 0.60), col = NA, cex = 1.25)
mtext("Color meter a", 1, 2.5, font = 2, cex = 1.5)
mtext("Color meter b", 2, 2.5, font = 2, cex = 1.5)
mtext("Hepatopancreas color", 3, 0.5, font = 2, cex = 1.5)
#text(40, 50, "Orange", pos = 4, font = 2, col = "grey40")
#text(0, 10, "Beige", pos = 1, font = 2, col = "grey40")
##text(12, 33, "Light orange", pos = 3, font = 2, srt = 49, col = "grey40")
#text(35, 20, "Dark orange?", pos = 1, font = 2, srt = 0, col = "grey40")

iy <- which((y$project == "OERA") & (y$gear == "cage"))
points(y$hepato.color.l[iy], y$hepato.color.a[iy], col = "green")
iy <- which((y$project == "OERA") & (y$gear == "trap"))
points(y$hepato.color.l[iy], y$hepato.color.a[iy], col = "red")

iy <- which(y$gonad.color.a >= 30 & y$gonad.color.b <= 30)
points(y$hepato.color.l[iy], y$hepato.color.a[iy], pch = 21, bg = "green", col = NA, cex = 1.25)

box(col = "grey50")
dev.off()

# Hepato weight by color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/hepato weight by color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(40, 90), c(0, 14), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$hepato.color.a)
Lab <- cbind(y$hepato.color.l[ix], y$hepato.color.a[ix], y$hepato.color.b[ix])
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$carapace.width[ix], y$hepato.weight[ix], pch = 21, bg = fade(cols, 0.5), col = NA, cex = 1.25)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.5)
mtext("Hepato weight (g)", 2, 2.5, font = 2, cex = 1.5)
text(60, 7.5, "Pale green hepatopancreas", font = 2, pos = 3, col = "grey40", srt = 45)
text(75, 2.0, "Dark green hepatopancreas", font = 2, pos = 1, col = "grey40", srt = 0)
box(col = "grey50")

iy <- which((y$project == "OERA") & (y$gear == "cage"))
points(y$carapace.width[iy], y$hepato.weight[iy], col = "green")
iy <- which((y$project == "OERA") & (y$gear == "trap"))
points(y$carapace.width[iy], y$hepato.weight[iy], col = "red")

dev.off()

# Hepato weight by gonad color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/hepato weight by gonad color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(40, 90), c(0, 14), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$gonad.color.a)
Lab <- cbind(y$gonad.color.l[ix], y$gonad.color.a[ix], y$gonad.color.b[ix])
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$carapace.width[ix], y$hepato.weight[ix], pch = 21, bg = fade(cols, 0.75), col = NA, cex = 1.25)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.5)
mtext("Hepato weight (g)", 2, 2.5, font = 2, cex = 1.5)
text(60, 7.5, "Orange gonads", font = 2, pos = 3, col = "grey40", srt = 45)
text(75, 2.0, "Beige gonads", font = 2, pos = 1, col = "grey40", srt = 0)
box(col = "grey50")
dev.off()

# Hepato weight by egg color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/hepato weight by egg color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(40, 90), c(0, 14), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$egg.color.a)
Lab <- cbind(y$egg.color.l[ix], y$egg.color.a[ix], y$egg.color.b[ix])
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$carapace.width[ix], y$hepato.weight[ix], pch = 21, bg = fade(cols, 0.75), col = NA, cex = 1.25)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.5)
mtext("Hepato weight (g)", 2, 2.5, font = 2, cex = 1.5)
#text(60, 7.5, "Orange gonads", font = 2, pos = 3, col = "grey40", srt = 45)
#text(75, 2.0, "Beige gonads", font = 2, pos = 1, col = "grey40", srt = 0)
box(col = "grey50")
dev.off()

# Egg weight by egg color:
png(file = "C:/Users/SuretteTJ/Desktop/github/snow-crab-fecundity/figures/Egg weight by gonad color.png",
    width = 7, height = 7, res = 500, units = "in")
plot(c(40, 90), c(0, 9), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
ix <- !is.na(y$gonad.color.a)
Lab <- cbind(y$gonad.color.l[ix], y$gonad.color.a[ix], y$gonad.color.b[ix])
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
srgb[is.na(srgb)] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
points(y$carapace.width[ix], y$egg.weight[ix], pch = 21, bg = fade(cols, 0.75), col = NA, cex = 1.25)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.5)
mtext("Egg weight (g)", 2, 2.5, font = 2, cex = 1.5)
#text(60, 7.5, "Orange gonads", font = 2, pos = 3, col = "grey40", srt = 45)
#text(75, 2.0, "Beige gonads", font = 2, pos = 1, col = "grey40", srt = 0)
box(col = "grey50")
dev.off()

