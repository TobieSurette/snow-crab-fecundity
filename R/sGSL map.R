library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

clg()
language <- language("en")
height <- 7
width <- 8.5

png(file = "figures/sGSL geography.png", units = "in", res = 400, height = 7, width = 8.5)

map.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")

map("bathymetry")

map("coast", col = "antiquewhite")
map.axis(1:4)

# Locations:
text(-65.22, 47.95, "Baie des Chaleurs", srt = 0, cex = 0.9, font = 4, col = "grey10")
text(-64.17, 47.60,  "Shediac Valley", srt = 65, cex = 0.85, font = 4, col = "grey10")
text(-61.27, 46.75, "Western\nCape Breton", srt = 54, cex = 0.8, font = 4, col = "grey10")
text(-62.9, 47.4,   "Bradelle\nBank", srt = 0, cex = 0.9, font = 4, col = "grey10")
text(-61.6, 48.5,   "Laurentian  Channel", srt = -18, cex = 1.2, font = 3, col = "grey10")

# Provinces and regions:
text(-63.45, 46.38, "Prince Edward Island", srt = -18, cex = 0.75, font = 1, col = "grey30")
text(-60.9, 46.38,  "Cape Breton", srt = 54, cex = 0.85, font = 1, col = "grey30")
text(-63.5, 45.65,  "Nova Scotia", srt = 0, cex = 1.0, font = 1, col = "grey30")
text(-65.65, 46.75, "New\nBrunswick", srt = 0, cex = 1.0, font = 1, col = "grey30")
text(-65.5, 48.65,  "Québec", srt = 0, cex = 1.0, font = 1, col = "grey30")

box()
wind.rose()
scale.bar(length = 80)

dev.off()



