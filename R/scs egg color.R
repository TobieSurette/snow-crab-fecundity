
# Read female colorimetry data from the 2019 survey:
b <- read.csv(locate(package = "gulf.data", keywords = c("colorimeter", "2019")))
b <- b[b$body.part == "eggs", ]

# Shell condition plot:
plot(bb$colour.b, bb$colour.a, cex = 0.5)
ix <- b$shell.condition == 2
points(bb$colour.b[ix], bb$colour.a[ix], pch = 21, bg = fade("green3"), col = "grey50")

ix <- b$shell.condition == 3
points(bb$colour.b[ix], bb$colour.a[ix], pch = 21, bg = fade("red"), col = "grey50")

ix <- b$shell.condition == 4
points(bb$colour.b[ix], bb$colour.a[ix], pch = 21, bg = fade("blue"), col = "grey50")

ix <- b$gonad.colour == "beige"
points(b$colour.b[ix], b$colour.a[ix], pch = 21, bg = fade("yellow", 1), col = "grey50")

bb <- read.scsbio(2019)
key(bb)

b$date <- as.character(date(b))
ix <- match(b[key(bb)], bb[key(bb)])

b$egg.colour <-  bb$egg.colour[ix]

ix <- b$egg.colour == 1
points(b$colour.b[ix], b$colour.a[ix], pch = 21, bg = fade("orange", 1), col = "grey50")

ix <- b$egg.colour == 2
points(b$colour.b[ix], b$colour.a[ix], pch = 21, bg = fade("red", 1), col = "grey50")
