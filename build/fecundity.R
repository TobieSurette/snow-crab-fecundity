library(gulf.data)
library(gulf.spatial)

# Load main file:
x <- read.csv("data/raw/MASTER SNOW CRAB FECUNDITY 1989-2023.csv")
x <- x[!is.na(x$year), ]
names(x) <- tolower(names(x))

# Rename fields:
names(x)[names(x) == 'egg.l'] <- "egg.colour.L"
names(x)[names(x) == 'egg.a'] <- "egg.colour.a"
names(x)[names(x) == 'egg.b'] <- "egg.colour.b"
names(x)[names(x) == 'gon.l'] <- "gonad.colour.L"
names(x)[names(x) == 'gon.a'] <- "gonad.colour.a"
names(x)[names(x) == 'gon.b'] <- "gonad.colour.b"
names(x)[names(x) == 'hep.l'] <- "hepato.colour.L"
names(x)[names(x) == 'hep.a'] <- "hepato.colour.a"
names(x)[names(x) == 'hep.b'] <- "hepato.colour.b"
names(x)[names(x) == 'crab.weight'] <- "weight"
names(x)[names(x) == 'proteines'] <- "protein"
names(x) <- gsub("[.]+", ".", names(x))
names(x) <- gsub("colour", "colour", names(x))
names(x)[names(x) == 'reproductive.status'] <- "maturity"
names(x)[names(x) == 'poids.hepato'] <- "hepato.weight"
names(x) <- gsub("color", "colour", names(x))
names(x)[names(x) == 'condition'] <- "shell.condition"
names(x)[names(x) == 'total.egg.count']  <- "fecundity"
names(x)[names(x) == 'total.egg.weight'] <- "egg.weight"
names(x)[names(x) == "secteur"] <- "sector"
x$eggs.vis <- gsub("[* ]", "", x$eggs.vis)
x$gon.vis  <- gsub("[* ]", "", x$gon.vis)
names(x) <- gsub(".vis$", ".visible", names(x))

# Load isolated files:
years <- c(2012, 2015, 2019, 2021, 2022)
files <- paste0("data/raw/fecundity ", years, ".csv")
y <- NULL
for (i in 1:length(files)){
   # Read file:
   tmp <- read.csv(files[i])
   names(tmp) <- tolower(names(tmp))
   if ("egg.weight.sample" %in% names(tmp)) print(files[i])
   if (length(grep("crab.number", names(tmp))) > 1) print(files[i])
   
   # Concatenate data:
   if (i == 1){
      y <- tmp
   }else{
      y[setdiff(names(tmp), names(y))]   <- NA
      tmp[setdiff(names(y), names(tmp))] <- NA
      tmp <- tmp[names(y)]
      y <- rbind(y, tmp)
   }
}    
y <- y[, -which(apply(y, 2, function(x) all(is.na(x))))]
y$preservation <- ""
y$preservation[which(y$sample == "live")] <- "live"
y$preservation[which(y$sample.type == "live")] <- "live"
names(y)[names(y) == "dissection.comments"] <- "dissection.comment"
y <- y[!is.na(y$year), ]
names(y) <- gsub("[.]l$", ".L", names(y))

# Remove years to be replaced:
x <- x[-which(x$year %in% unique(y$year)), ]

# Missing legs:
source("C:/Users/SuretteTJ/Desktop/github/gulf.data/R/missing.legs.R")
iy <- which(is.na(y$missing.legs) & (nchar(y$missing.legs.left) == 5) & (nchar(y$missing.legs.right) == 5))
y$missing.legs[iy] <- paste0(y$missing.legs.left[iy], y$missing.legs.right[iy])
y$missing.legs.left[iy]  <- ""
y$missing.legs.right[iy] <- ""
iy <- which(is.na(y$missing.legs))
y$missing.legs.left[iy]  <- gsub("[* ]", "", y$missing.legs.left[iy])
y$missing.legs.right[iy] <- gsub("[* ]", "", y$missing.legs.right[iy])
y$missing.legs.left[which(y$missing.legs.left == "4H5H")] <- "4H,5H"
y$missing.legs.left[which(y$missing.legs.left == "1R2R")] <- "1R,2R"
iy <- which(y$missing.legs.left != "") 
y$missing.legs.left[iy] <- unlist(lapply(strsplit(y$missing.legs.left[iy], "[ ,]+"), function(x) paste0("L", x, collapse = ",")))
y$missing.legs.right[which(y$missing.legs.right == "2H3H4H")] <- "2H,3H,4H"
y$missing.legs.right[which(y$missing.legs.right == "4H5H")] <- "4H,5H"
y$missing.legs.right[which(y$missing.legs.right == "1H2H")] <- "1H,2H"
y$missing.legs.right[which(y$missing.legs.right == "3H4H5H")] <- "3H,4H,5H"
iy <- which(y$missing.legs.right != "")
y$missing.legs.right[iy] <- unlist(lapply(strsplit(y$missing.legs.right[iy], "[ ,]+"), function(x) paste0("R", x, collapse = ",")))
y$missing.legs[is.na(y$missing.legs)] <- ""
y$missing.legs.left[is.na(y$missing.legs.left)] <- ""
y$missing.legs.right[is.na(y$missing.legs.right)] <- ""
iy <- which((y$missing.legs == "") & ((y$missing.legs.left != "") | (y$missing.legs.right != "")))
y$missing.legs[iy] <- paste0(y$missing.legs.left[iy], ",", y$missing.legs.right[iy])
y$missing.legs[iy] <- missing.legs(y$missing.legs[iy])
y$missing.legs[y$missing.legs == ""] <- "**********"
y$missing.legs.left  <- ""
y$missing.legs.right <- ""
   
x$missing.legs.left  <- gsub("DACT", "D", x$missing.legs.left)
x$missing.legs.left  <- gsub("CL", "", x$missing.legs.left)
x$missing.legs.right <- gsub("DACT", "D", x$missing.legs.left)
x$missing.legs.right <- gsub("CL", "", x$missing.legs.left)
x$missing.legs.left  <- gsub("[.]", ",", x$missing.legs.left)
x$missing.legs.right <- gsub("[.]", ",", x$missing.legs.right)
x$missing.legs.left[x$missing.legs.left == "2R3R"]   <- "2R 3R"
x$missing.legs.right[x$missing.legs.right == "2R3R"] <- "2R 3R"
ix <- x$missing.legs.left != ""
x$missing.legs.left[ix] <- unlist(lapply(strsplit(x$missing.legs.left[ix], "[ ,]+"), function(x) paste0("L", x, collapse = ",")))
ix <- x$missing.legs.right != ""
x$missing.legs.right[ix] <- unlist(lapply(strsplit(x$missing.legs.right[ix], "[ ,]+"), function(x) paste0("R", x, collapse = ",")))
x$missing.legs <- ""
ix <- which((x$missing.legs.left != "") | (x$missing.legs.right != ""))
x$missing.legs[ix] <- paste0(x$missing.legs.left[ix], ",", x$missing.legs.right[ix])
x$missing.legs[ix] <- missing.legs(x$missing.legs[ix])
x$missing.legs[-ix] <- "**********"

# Joint data sets:
y[setdiff(names(x), names(y))] <- ""
x[setdiff(names(y), names(x))] <- ""
y <- y[names(x)]
x <- rbind(x, y)

# Old fecundity data files:
files <- paste0("data/raw/", c("Baie des Chaleurs April 1986.csv",
                               "Bradelle Bank June 1988.csv",
                               "Cape Breton August 1986.csv",
                               "Cape Breton July 1986.csv",
                               "PEI July 1986.csv",
                               "PEI July 1987.csv",
                               "PEI May 1986.csv"))
y <- NULL
for (i in 1:length(files)){
   # Read file:
   tmp <- read.csv(files[i])
   
   # Field name fixes:
   names(tmp) <- gsub("egg.sample.number", "sample.egg.count", names(tmp))
   names(tmp) <- gsub("egg.sample.count", "sample.egg.count", names(tmp)) 
   names(tmp) <- gsub("egg.sample.percentage", "sample.egg.percentage", names(tmp))
   names(tmp) <- gsub("egg.sample.weight", "sample.egg.weight", names(tmp))
   names(tmp) <- gsub("egg.total.weight", "egg.weight", names(tmp))
   names(tmp) <- gsub("claw[.]", "chela.", names(tmp))
   
   if (length(grep("egg.colour", names(tmp))) > 1) print(files[i])
   # Concatenate data:
   if (i == 1){
      y <- tmp
   }else{
      y[setdiff(names(tmp), names(y))]   <- NA
      tmp[setdiff(names(y), names(tmp))] <- NA
      tmp <- tmp[names(y)]
      y <- rbind(y, tmp)
   }
}          
y <- y[!is.na(y$year) & !is.na(y$carapace.width), ]
iy <- which(is.na(y$fecundity) & !is.na(y$egg.weight) & !is.na(y$egg.sample.count) & !is.na(y$egg.sample.weight))
y$fecundity[iy] <- round(y$egg.weight[iy] / (0.0001 * (y$egg.sample.weight[iy] / y$egg.sample.count[iy]))) 
y$project <- "fecundity"
y$maturity[y$maturity == 'm'] <- "multiparous"
y$maturity[y$maturity == 'p'] <- "primiparous"

# Egg colour:
y$egg.colour[y$egg.colour %in% c("o", "or", "ot")] <- "orange"
y$egg.colour[y$egg.colour == "of"] <- "dark orange"
y$egg.colour[y$egg.colour %in% c("br")] <- "brown"
y$egg.colour[y$egg.colour %in% c("y", "or-y", "oj", "oy", "n", "ne", "no")] <- "light orange"

# Shell condition:
y$shell.condition[y$shell.condition %in% c("1", "n", " n", "new")] <- "new-soft"
y$shell.condition[y$shell.condition %in% c("2", "m", "med", "new-med")] <- "new-hard"
y$shell.condition[y$shell.condition %in% c("3", "o", "old")] <- "old-shell"
y$shell.condition[which((y$shell.condition == "") & (y$maturity == "primiparous"))] <- "new-soft"
y$shell.condition[which((y$shell.condition == "") & (y$maturity == "multiparous"))] <- "old-shell"

# Joint data sets:
y[setdiff(names(x), names(y))] <- ""
x[setdiff(names(y), names(x))] <- ""
y <- y[names(x)]
x <- rbind(y, x)

# Date:
x$month[x$month == "Fall"] <- ""
x$month <- as.numeric(x$month)
x$day[x$day == "12,13"] <- "12"
x$day <- as.numeric(x$day)

# Tow number:
x$tow.number[is.na(x$tow.number)] <- ""

# Morphometric measurements:
x$carapace.width    <- as.numeric(gsub("[*]", "", x$carapace.width))
x$carapace.length   <- as.numeric(gsub("[*]", "", x$carapace.length))
x$abdomen.width     <- as.numeric(gsub("[*]", "", x$abdomen.width))
x$abdomen.width.lab <- as.numeric(gsub("[*]", "", x$abdomen.width.lab))
x$abdomen.width[x$abdomen.width > 100]  <- NA
x$chela.height      <- as.numeric(gsub("[*]", "", x$chela.height))
x$chela.length      <- as.numeric(gsub("[*]", "", x$chela.length))

# Crab weight:
x$weight <- as.numeric(gsub("[*]", "", x$weight))

# Season:
x$season <- ""
x$season[x$month %in% 3:5] <- "spring"
x$season[x$month %in% 6:8] <- "summer"
x$season[x$month %in% 9:12] <- "fall"

# Project description:
x$project[is.na(x$project)] <- ""
x$project[x$project == 'June Survey'] <- 'June survey'
x$project[x$project %in% c("Fall survey", "June survey", "Suvey")] <- 'Survey'
x$project[x$project %in% c("Colorimeter, Fecundity", "Colorimeter/Fecundity", "colorimetre/fecundity")] <- "Colorimeter/Fecundity"

# Fishing vessel:
x$vessel[is.na(x$vessel)] <- ""
x$vessel[x$vessel == "Marco Michel"] <- "Marco-Michel"
x$vessel[x$vessel %in% c("CCGS Perley", "CCGS M.PERLEY", "CCGS M.Perley", "CCGS M. PERLEY", "Perley") ] <- "CCGS M. Perley"
x$vessel[x$vessel %in% c("FISH FULL THINKING", "FISHFULL THINKING")] <- "Fishfull Thinking"
x$vessel[x$vessel %in% c("Avalon")] <- "Avalon Voyager II" 
x$vessel[x$vessel %in% c("BRITANNY MADDISON")] <- "Britanny Maddison" 
x$vessel[x$vessel %in% c("Opilio", "CGC OPILIO", "CPG OPILIO")] <- "CCGS Opilio"
x$vessel[x$vessel %in% c("JEAN MATHIEU")] <- "Jean Mathieu" 
survey.vessels <- c("Den C. Martin", "Jean Mathieu", "Marco-Michel", "Emy-Serge", "Avalon Voyager II")

# Additional project specifications:
x$project[which((x$project == "") & (x$vessel %in% survey.vessels))] <- "Survey"
x$project[which(x$vessel %in% 'CCGS M. Perley')] <- "Fecundity"
x$project[x$project == "Suvey"] <- "Survey"

# Location:
x$location[is.na(x$location)] <- ""
x$location <- gsub(" +", " ", x$location)
x$location[grep("[- ][Cc]haleur", x$location)] <- "Baie des Chaleurs" 
x$location[grep("MARGAREE", x$location)] <- "Margaree"
x$location[grep("CHETICAMP", x$location)] <- "Cheticamp"
x$location[grep("LOUISBOURG", x$location)] <- "Louisbourg"
x$location[grep("GRANDE RIVIERE", x$location)] <- "Grande Riviere" 
x$location[grep("Bradelle", x$location)] <- "Bradelle Bank" 
x$location <- gsub("GP ", "GP", x$location) 
x$location[which((x$location == "") & (x$zone %in% c("18", "19")))] <- "Cape Breton"

# Coordinate fixes:
x$lat  <- gsub("[º°]", "", x$lat)
x$long <- gsub("[º°]", "", x$long)
x$lat  <- gsub("^0", "", x$lat)
x$long <- gsub("^0", "", x$long)
x$latitude <- gulf.spatial::dmm2deg(as.numeric(x$lat))
x$longitude <- -abs(gulf.spatial::dmm2deg(as.numeric(x$long)))
x$location[grep("GP", x$location)]
s <- read.scsset(year = unique(x$year[grep("GP", x$location)]), valid = 1, survey = "regular")
ix <- match(x$location[grep("GP", x$location)], substr(s$tow.id, 1, 5))
x$longitude[grep("GP", x$location)] <- lon(s)[ix] 
x$latitude[grep("GP", x$location)]  <- lat(s)[ix]
x$loranXY[is.na(x$loranXY)] <- ""

# Coordinates from Loran XY:
ix <- which(x$loranXY != "")
tmp <- loran2deg(as.numeric(unlist(lapply(strsplit(x$loranXY[ix], "/"), function(x) x[1]))), as.numeric(unlist(lapply(strsplit(x$loranXY[ix], "/"), function(x) x[2]))))
x$longitude[ix] <- -abs(tmp[,1])
x$latitude[ix] <- tmp[,2]

# Find survey coordinates from survey sample dates:
ix <- which(x$project == "Survey" & is.na(x$longitude))
years <- sort(unique(x$year[ix]))
for (i in 1:length(years)){
   #print(years[i])
   s <- read.scsset(year = years[i], valid = 1, survey = "regular")
   import(s) <- catch(read.scsbio(year = years[i]), category = "FM")
   s <- s[s$FM > 0, ]
   dates <- unique(date(x[intersect(ix, which(x$year == years[i])), ]))
   for (j in 1:length(dates)){
      d <- difftime(dates[j], date(s), units = "days") 
      iy <- which((d >= 0) & (d <= 1)) # Within 3 days of sampling.
      if (length(iy) > 4) iy <- tail(iy, 5)
      #print(iy)
      if (length(iy) > 0){
         iz <- which((date(x) == dates[j]) & (x$project == "Survey") & is.na(x$longitude))
         x$longitude[iz] <- mean(lon(s)[iy])
         x$latitude[iz]  <- mean(lat(s)[iy])
      }
   }
}

# Coordinates from station specifications:
ix <- which(is.na(x$longitude) & x$station == "1" & x$year %in% 2001:2003)
x$longitude[ix] <- -64.23317
x$latitude[ix]  <- 47.548
ix <- which(is.na(x$longitude) & x$station == "2" & x$year %in% 2001:2003)
x$longitude[ix] <- -64.16250
x$latitude[ix]  <- 47.76150
ix <- which(is.na(x$longitude) & x$station == "3" & x$year %in% 2001:2003)
x$longitude[ix] <- -64.07200
x$latitude[ix]  <- 47.73683
ix <- which(is.na(x$longitude) & x$station == "4" & x$year %in% 2001:2003)
x$longitude[ix] <- -63.92317
x$latitude[ix]  <- 47.75650

# Update some locations using coordinates:
x$tow.number[grep("^GP", x$location)] <- x$location[grep("^GP", x$location)]
x$location[grep("^GP", x$location)] <- ""
x$project[grep("survey", tolower(x$location))] <- "Survey"
x$location[grep("survey", tolower(x$location))] <- ""
x$location[x$location == "14 Miles off Cheticamp"] <- "Cheticamp" 
x$location[x$location == "Cheticamp Lab"] <- "Cheticamp" 
x$location[x$location %in% c("chaleur 1", "chaleur 2")] <- "Baie des Chaleurs" 
ix <- which(x$location == "" & !is.na(x$longitude))
x$location[ix][x$longitude[ix] <= -64.33333] <- "Baie des Chaleurs"
ix <- which(x$location == "" & !is.na(x$longitude))
x$location[ix][x$longitude[ix] >= 48.3333] <- "American Bank"
ix <- which(x$location == "" & !is.na(x$longitude))
x$location[ix][x$longitude[ix] <= -63.66666] <- "Shediac Valley"
ix <- which(x$location == "" & !is.na(x$longitude) & !is.na(x$latitude))
x$location[ix][fishing.zone(x$longitude[ix], x$latitude[ix], species = 2526) == "19"] <- "Cape Breton"
ix <- which(x$location == "" & !is.na(x$longitude) & !is.na(x$latitude))

x$location[x$location %in% 1:4] <- "Shediac Valley"
x$location[x$location %in% c("Buffer Zone", "Cheticamp", "Margaree")] <- "Cape Breton"
x$location[x$location == "PEI; zone 26"] <- "PEI"
x <- x[which(x$location != "Louisbourg"), ]
x$location[x$location %in% c("Grande Riviere")] <- "Baie des Chaleurs"

x$location[which(x$year == 2021 & x$month == 7)] <- "Cape Breton"
x$location[which(x$year == 2015 & x$month == 10)] <- "Cape Breton"

x$location[which(x$year == 2008 & x$month == 7 & x$day == 18)] <- "Bradelle Bank"
x$location[which(x$year == 2008 & x$month == 7 & x$day == 29)] <- "Bradelle Bank"

x$location[x$year == 1999] <- "Baie des Chaleurs"


ix <- which(x$year == 1989 & unlist(lapply(strsplit(x$crab.number, "-"), function(x) x[1])) %in% c("C", "H", "N", "P", "T"))
x$location[ix] <- "Baie des Chaleurs"

ix <- which(x$year == 1989 & unlist(lapply(strsplit(x$crab.number, "-"), function(x) x[1])) %in% c("F", "G", "J", "Z", "R"))
x$location[ix] <- "Cape Breton"

ix <- which(x$year == 1989 & unlist(lapply(strsplit(x$crab.number, "-"), function(x) x[1])) %in% c("E", "I", "M", "Q"))
x$location[ix] <- "Bradelle Bank"

ix <- which(x$year == 1990 & unlist(lapply(strsplit(x$crab.number, "-"), function(x) x[1])) %in% c("BB", "DD", "HH", "LL", "MM", "II", "GG", "JJ", "OO", "PP", "QQ", "RR"))
x$location[ix] <- "Baie des Chaleurs"

ix <- which(x$year == 1990 & unlist(lapply(strsplit(x$crab.number, "-"), function(x) x[1])) %in% c("EE", "FF"))
x$location[ix] <- "Bradelle Bank"

ix <- which(x$year == 1991 & unlist(lapply(strsplit(x$crab.number, "-"), function(x) x[1])) %in% c("E", "G", "H", "B", "C", "D"))
x$location[ix] <- "Baie des Chaleurs"

x <- x[!(x$location %in% c("22", "25")), ]
x$location[x$year == 1992] <- "Baie des Chaleurs"

x$location[x$year == 1998 & x$month == 8 & x$day == 13] <- "Baie des Chaleurs"
x$location[x$year == 1998 & x$month == 7 & x$day == 15] <- "Bradelle Bank"
x$location[x$year == 1998 & x$month == 7 & x$day == 20] <- "Bradelle Bank"
x$location[x$year == 1998 & x$month == 7 & x$day == 29] <- "Cape Breton"

# Maturity status:
x$maturity <- toupper(x$maturity)
x$maturity <- gsub(",", "", x$maturity)
   
# Gonad weight:
x$gonad.weight[x$gonad.weight == "n/a"] <- "" 
x$gonad.weight <- gsub("[*]", "", x$gonad.weight)
x$gonad.weight <- as.numeric(x$gonad.weight)

# Hepato-pancreatic weight:
x$hepato.weight[x$hepato.weight == "n/a"] <- ""
x$hepato.weight <- as.numeric(gsub("[*]", "", x$hepato.weight))

# Maturity observations:
x$maturity[is.na(x$maturity) | x$maturity == "*"] <- ""
x$maturity[x$maturity %in% c("3")] <- ""
x$maturity[x$maturity %in% c("2E 22.4", "2E 22.4-5", "2E 22.5")] <- "multiparous"
x$maturity[x$maturity %in% c("1", "P", "P1", "P2", "PRIMIPAROUS")] <- "primiparous"
x$maturity[x$maturity %in% c("M1\\P2", "M", "M1", "M2", "M3", "MULTIPAROUS")] <- "multiparous"
x$maturity[x$maturity %in% c("M4", "M5", "M6", "SENILE")] <- "senile"

# Shell condition:
x$shell.condition[is.na(x$shell.condition)] <- ""
x$shell.condition <- toupper(x$shell.condition)
x$shell.condition <- gsub("[*]", "", x$shell.condition)
x$shell.condition <- gsub("[.][5-8]", "M", x$shell.condition)
x$shell.condition <- gsub("3[+]", "3M", x$shell.condition)
x$shell.condition[x$shell.condition == "3\\4"] <- "3M"
x$shell.condition <- gsub(" ", "", x$shell.condition)
x$shell.condition[x$shell.condition == "40"] <- "4"
x$shell.condition[x$shell.condition == "41"] <- "4+"
x$shell.condition[x$shell.condition == "42"] <- "4++"
sort(unique(x$shell.condition))
x$shell.condition.observed <- x$shell.condition
x$shell.condition <- as.numeric(substr(x$shell.condition,1,1))
x$shell.condition[x$year < 1991 & x$shell.condition == 3] <- 4
x$shell.condition[x$year < 1991 & x$shell.condition == 2] <- 3
x$shell.condition[x$year < 1991 & x$shell.condition == 1] <- 2
x$shell.condition[which(is.na(x$shell.condition) & (x$shell.condition.observed %in% "NEW-SOFT"))] <- 2
x$shell.condition[which(is.na(x$shell.condition) & (x$shell.condition.observed %in% "NEW-HARD"))] <- 3
x$shell.condition[which(is.na(x$shell.condition) & (x$shell.condition.observed %in% c("M", "OLD-SHELL")))] <- 4
x$shell.condition[which(is.na(x$shell.condition) & (x$maturity %in% c("multiparous", "M4", "M5")))] <- 4
x$shell.condition[which(is.na(x$shell.condition) & (x$maturity %in% c("M6", "senile")))] <- 5
x$shell.condition[which(is.na(x$shell.condition) & (x$maturity %in% c("primiparous")))] <- 2
x$maturity[x$maturity %in% c("M4", "M5", "M6")] <- "multiparous"

ix <- which((x$year == 2004) & (x$shell.condition == 2) & (x$fecundity > 0.18*x$carapace.width^3))
x$shell.condition[ix] <- 3

# Egg weights:
x$egg.weight[grep("[a-zA-Z]", x$egg.weight)] <- ""
x$egg.weight[-grep("[.]", x$egg.weight)] <- ""
x$egg.weight <- as.numeric(x$egg.weight)
x$sample.egg.weight[grep("[a-zA-Z]", x$sample.egg.weight)] <- ""
x$sample.egg.weight[-grep("[.]", x$sample.egg.weight)] <- ""
x$sample.egg.weight <- as.numeric(x$sample.egg.weight)
x$sample.egg.weight[which(x$sample.egg.weight > 1)] <- NA

# Eggs remaining:
x$eggs.remaining  <- gsub("[*]", "", x$eggs.remaining)
x$eggs.remaining[x$eggs.remaining %in% c("none", "NONE")] <- "0"
x$eggs.remaining[x$eggs.remaining %in% c("4+")] <- "4"
x$eggs.remaining[x$eggs.remaining %in% c("100+")] <- "100"

# Egg counts:
x$fecundity[x$fecundity %in% c("échappé", "0.00", "#VALUE!")] <- ""
x$fecundity <- round(as.numeric(x$fecundity))
x$fecundity[which(x$fecundity < 500)] <- NA
x$sample.egg.count[grep("[a-zA-Z]", x$sample.egg.count)] <- ""
x$sample.egg.count <- as.numeric(x$sample.egg.count)
x$sample.egg.count[which(x$sample.egg.count < 30)] <- NA

# Egg development:
x$egg.development[x$egg.development %in% c("-", "*", "?", "no sample", "none")] <- ""
x$egg.development[x$egg.development %in% c("1", "1-2", "1-3/mixed dev.", "1-4", "1-6 mixed")] <- "1-3"
x$egg.development[x$egg.development %in% c("3-")] <- "3"
x$egg.development[x$egg.development %in% c("3+")] <- "3-4"
x$egg.development[x$egg.development %in% c("4,0")] <- "4"
x$egg.development[x$egg.development %in% c("5+")] <- "5-6"
x$egg.development[x$egg.development %in% c("6+", "7-6")] <- "6-7"
x$egg.development[x$egg.development %in% c("7-", "7+")] <- "7"
x$egg.development[x$egg.development %in% c("7+", "7-mix dev.")] <- "7-8"
x$egg.development[x$egg.development %in% c("8,9", "8+")] <- "8-9"     
x$egg.development[x$egg.development %in% c("9+", "9,10", "9-11", "10,9")] <- "9-10"
x$egg.development[x$egg.development %in% c("11-", "11+")] <- "11" 
x$egg.development[x$egg.development %in% c("nf", "NF ")] <- "NF"
x$egg.development[x$egg.development %in% c("13 + capsule", "13+ capsule")] <- "capsule"

# Egg colour:
x$egg.colour[x$egg.colour %in% c("1", "OP", "OC", "OC + OF (15%)")] <- "light orange"
x$egg.colour[x$egg.colour %in% c("-", "*", "none", "NONE", "dead")] <- ""
x$egg.colour <- gsub(" +$", "", x$egg.colour)
x$egg.colour[x$egg.colour %in% c("Capsule", "COCOON")] <- "coccoon"
x$egg.colour[x$egg.colour %in% c("R")] <- ""
x$egg.colour[x$egg.colour %in% c("O")] <- "orange"
x$egg.colour[x$egg.colour %in% c("2", "FO", "OF", "0F", "B", "Br", "Brun", "OF+Br", "Ofqqe", "OFqqe", "QQE OF")] <- "dark orange"
x$egg.colour[x$egg.colour %in% c("3",  "N")] <- "black"
ix <- which(x$egg.development %in% c("O", "OC", "OF"))
x$egg.colour[ix] <- egg.colour(x$egg.development[ix])  # Overwrite egg colours from egg development.
x$egg.colour[x$egg.colour %in% c("brown")] <- "dark orange"
x$egg.colour.L <- as.numeric(gsub("[*]", "", x$egg.colour.L))
x$egg.colour.a <- as.numeric(gsub("[*]", "", x$egg.colour.a))
x$egg.colour.b <- as.numeric(gsub("[*]", "", x$egg.colour.b))
x$eggs.visible <- gsub("[*]", "", x$eggs.visible)

# Hepatopancreas colour:
x$hepato.colour.L <- as.numeric(gsub("[*]", "", x$hepato.colour.L))
x$hepato.colour.a <- as.numeric(gsub("[*]", "", x$hepato.colour.a))
x$hepato.colour.b <- as.numeric(gsub("[*]", "", x$hepato.colour.b))
x$hepato.visible <- gsub("[*]", "", x$hep.visible)

# Gonad colour:
x$gonad.colour <- toupper(x$gonad.colour)
x$gonad.colour <- gsub("[ _*-]$", "", x$gonad.colour)
x$gonad.colour[x$gonad.colour %in% c("B", "BE", "BEI", "BEIGE")] <- "beige"
x$gonad.colour[x$gonad.colour %in% c("1") & !(x$year %in% 1991:1992)] <- "white"
x$gonad.colour[x$gonad.colour %in% c("2") & !(x$year %in% 1991:1992)] <- "beige"
x$gonad.colour[x$gonad.colour %in% c("3") & !(x$year %in% 1991:1992)] <- "orange"
x$gonad.colour[x$gonad.colour %in% c("4") & !(x$year %in% 1991:1992)] <- "light orange"
x$gonad.colour[x$gonad.colour %in% c("NONE")] <- ""
x$gonad.colour[x$gonad.colour %in% c("W", "BL")] <- "white"
x$gonad.colour[x$gonad.colour %in% c("B/O", "BO", "O/B", "OB", "BE-O")] <- "beige" # "beige-orange"
x$gonad.colour[x$gonad.colour %in% c("O", "ORANGE")] <- "orange"
x$gonad.colour[x$gonad.colour %in% c("OC")] <- "light orange"
x$gonad.colour[x$gonad.colour %in% c("OF")] <- "dark orange"
x$gonad.colour.L <- as.numeric(gsub("[*]", "", x$gonad.colour.L))
x$gonad.colour.a <- as.numeric(gsub("[*]", "", x$gonad.colour.a))
x$gonad.colour.b <- as.numeric(gsub("[*]", "", x$gonad.colour.b))
x$gonad.visible <- gsub("[*]", "", x$gon.visible)

# Spermatheca weight:
x$spermatheca.weight[grep("[a-zA-Z-]", x$spermatheca.weight)] <- ""
x$spermatheca.weight <- as.numeric(x$spermatheca.weight)
x$spermatheca.weight[x$spermatheca.weight > 3] <- NA

# Sperm count:
x$sperm.count <- x$spermatheca.sperm
x$sperm.count[grep("[a-zA-Z-]", x$sperm.count)] <- NA
x$sperm.count <- gsub(",", "", x$sperm.count)
x$sperm.count <- as.numeric(x$sperm.count)
x$sperm.count[x$sperm.count > 8E8] <- NA
x$sperm.count.sample <- x$sperm.sample.count 
x$sperm.count.sample[grep("[a-zA-Z-]", x$sperm.count.sample)] <- ""
x$sperm.count.sample <- as.numeric(x$sperm.count.sample)
x$sperm.square.count[x$sperm.square.count == "vide"] <- "0"
x$sperm.square.count[grep("[a-zA-Z-]", x$sperm.square.count)] <- ""
x$sperm.square.count <- as.numeric(x$sperm.square.count)
x$spermatheca.volume[grep("[a-zA-Z-]", x$spermatheca.volume)] <- ""
x$spermatheca.volume <- as.numeric(x$spermatheca.volume)
x$spermatheca.volume[x$spermatheca.volume > 10] <- NA

# Gear:
x$gear <- tolower(x$gear)
x$gear[x$gear == "traps"] <- "trap"

# Clean-up:
remove <- c('eggs.valid', "lat", "long", 'missing.legs.left', 'missing.legs.right', "condition", "total.egg.weight", 
            "spermatheca..sperm", "sperm.sample.count", 'sperm.sample.count', "egg.l", "egg.a", "egg.b",
            "gon.l", "gon.a", "gon.b", "hep.l", "hep.a", "hep.b", "sample", "sample.type", "hep.visible", "gon.visible")
x <- x[, setdiff(names(x), remove)]
vars <- c(names(x)[grep("sperm", names(x))], names(x)[grep("egg", names(x))], names(x)[grep("gon", names(x))], names(x)[grep("hep", names(x))] )
x <- x[c(setdiff(names(x), vars), vars)]

# Egg weight filter:
ix <- which((-10.025 > log(x$sample.egg.weight / x$sample.egg.count)) | (log(x$sample.egg.weight / x$sample.egg.count) > -9.45))
x <- x[-ix, ]

# Filter out extreme and missing observations:
ix <- which(!is.na(x$carapace.width) & (x$fecundity < 135000) & !is.na(x$fecundity))
x <- x[ix, ]

# Remove surplus Shediac sites:
ix <- which(x$year== 2006 & x$location %in% c("", "Shediac Valley"))
x <- x[-ix, ]

# Egg weight filter:
x <- x[which(x$carapace.width >= 40 & x$carapace.width <= 95), ]

# Determine valid fecundity:
x$valid.fecundity <- 1
terms <- c("worm", "vers", "parasite", "empty", "damage", "deformed", "déformés", "brisé", "cassé",
           "nécros[ée]", "pleopod[e]* missing", "hyas", "capsules", "coc", "popped", "drop", "oeuf perdu",
           "échappé", "echappe", "egg[ a-z]*abnormal", "not grinded well enough to count", "pourrit") 
for (i in 1:length(terms)){
   x$valid.fecundity[grep(terms[i], tolower(x$comment))] <- 0
   x$valid.fecundity[grep(terms[i], tolower(x$egg.comment))] <- 0
}
x <- x[x$valid.fecundity == 1, ]
x <- x[x$fecundity >= 5000, ]

# Fill-in 2003 and 2004 with missing shell conditions:
ix <- which(is.na(x$shell.condition))
tmp <- unique(data.frame(date = date(x[ix, ]), tow.number = as.numeric(x$tow.number[ix])))
b <- read.scsbio(2003:2004)
sc <- rep(NA, nrow(x))
for (i in 1:nrow(tmp)){
   iy <- which(tmp$date[i] == b$date & tmp$tow.number[i] == b$tow.number)
   iz <- which((date(x) == tmp$date[i]) & (as.numeric(x$tow.number) == tmp$tow.number[i]))
   #x$shell.condition[iz] <- median(b$shell.condition[iy])
   sc[iz] <- median(b$shell.condition[iy])
   x$gear[iz] <- "Nephrops trawl"
}      
# Shell condition 2s with fecundities that indicate multiparity:
ix <- which((sc %in% 1:2) & (log(x$fecundity) >= (3 * log(x$carapace.width) - 1.73)))
sc[ix] <- 3
x$shell.condition[!is.na(sc)] <- sc[!is.na(sc)]

# Shell condition 2s that are clear multiparous:
ix <- which((x$location == "Baie des Chaleurs") & (x$year == 1986) & 
            (x$shell.condition == 2) & (log(x$fecundity) >= (3 * log(x$carapace.width) - 1.73)))
x$shell.condition[ix] <- 4

ix <- which((x$location == "Baie des Chaleurs") & (x$year == 2009) & 
               (x$shell.condition == 2) & (log(x$fecundity) >= (3 * log(x$carapace.width) - 1.73)))
x$shell.condition[ix] <- 3

#ix <- which((x$location == "Cape Breton") & (x$year == 2007) & 
#               (x$shell.condition == 2) & (log(x$fecundity) >= (3 * log(x$carapace.width) - 1.73)))
#x$shell.condition[ix] <- 3

# Output:
#write.csv(x, file = "data/fecundity.csv", row.names = FALSE)

