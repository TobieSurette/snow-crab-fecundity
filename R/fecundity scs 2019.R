library(gulf.data)

# Read 2019 snow crab survey data:
s <- read.scsset(year = 2019, valid = 1, survey = "regular")
b <- read.scsbio(year = 2019, valid = 1, survey = "regular")

# Fecundity data from the 2019 snow crab survey:
x <- read.csv("C:/Users/SuretteTJ/Desktop/snow-crab-fecundity/data/Fecundity 2019 (Emily and Murray for Tobie).csv")

names(x) <- tolower(names(x))

names(x) <- gsub("[.]+", ".", names(x))
names(x) <- gsub("[.]$", "", names(x))
names(x) <- gsub("sampler.id", "sampler", names(x))
names(x) <- gsub("survey.tow.id", "tow.id", names(x))
names(x) <- gsub("crab", "crab.number", names(x)) 
names(x) <- gsub("legende", "legend", names(x))

for (i in 1:ncol(x)) x[, i] <- gsub("*", "", x[, i])

# Fix tow IDs:
x$tow.id <- toupper(gsub(" ", "", x$tow.id))
x$tow.id <- as.numeric(gsub("[AF][0-9]*", "", x$tow.id))
x$tow.id <- s$tow.id[match(x$tow.id, as.numeric(substr(s$tow.id, 3, 5)))]

# Fix crab number:
x$crab.number <- gsub("-", "", x$crab.number)
x$crab.number <- gsub("[C][1-5]", "", x$crab.number)
x$crab.number <- as.numeric(gsub(" ", "", x$crab.number))

# Spot corrections:
x$crab.number[which(x$tow.id == "GP073F")] <- 7
x <- x[-c(which(x$tow.id == "GP132F" & x$crab.number == 194), which(x$tow.id == "GP204F" & x$crab.number == 43))]
 
# Fix abdomen measurements:
x$abdomen.width  <- as.numeric(gsub("[*]", "", x$abdomen.width))
x$abdomen.height <- as.numeric(gsub("[*]", "", x$abdomen.height))

# Fix time variables:
reformat.time <- function(x){
   x <- gsub("h", ":", x)
   x <- gsub("[*]", "", x)
   x <- time(x)
   return(x)
}
x$start.time.counting  <- reformat.time(x$start.time.counting)
x$end.time.counting    <- reformat.time(x$end.time.counting)
x$time.out.of.formalin <- substr(reformat.time(x$time.out.of.formalin), 1, 5)
x$rinse.start.time <- reformat.time(x$rinse.start.time)
x$rinse.end.time   <- reformat.time(x$rinse.end.time)

# Fix date variables:
reformat.date <- function(x){
  x <- gsub("[*]", "", x)
  x <- gsub("[/]", "-", x)
  s <- strsplit(x, "-")
  year  <- as.numeric(unlist(lapply(s, function(x) x[3])))
  month <- as.numeric(unlist(lapply(s, function(x) x[2])))
  day   <- as.numeric(unlist(lapply(s, function(x) x[1])))
  ix <- !is.na(year) & !is.na(month) & !is.na(day)
  v <- rep("", length(x))
  v[ix] <- as.character(as.Date(paste0(year[ix], "-", month[ix], "-", day[ix])))
  v[is.na(v)] <- ""
  return(v)
}
x$date.measured <- reformat.date(x$date.measured)
x$date.of.egg.cleaning <- reformat.date(x$date.of.egg.cleaning)
x$date.eggs.were.counted <- reformat.date(x$date.eggs.were.counted)
x$date.weighed.pleopod.plus.dust <- reformat.date(x$date.weighed.pleopod.plus.dust)

# Import biological observations:
ix <- match(x[c("tow.id", "crab.number")], b[c("tow.id", "crab.number")])
x$shell.condition <- b$shell.condition[ix]
x$carapace.width  <- b$carapace.width[ix]
x$egg.colour      <- b$egg.colour[ix]
x$date            <- b$date[ix]
x$tow.number      <- b$tow.number[ix]

# Fix weight variables:
x$weight.of.subsample.1 <- as.numeric(x$weight.of.subsample.1)
x$dry.weight.1.clean.dry.eggs.out.of.oven <- as.numeric(x$dry.weight.1.clean.dry.eggs.out.of.oven)
x$number.in.subsample.1 <- as.numeric(gsub("[ (P)]", "", x$number.in.subsample.1))
x$dry.weight.pleopods.plus.dust <- as.numeric(x$dry.weight.pleopods.plus.dust)
x$total.weight.dry.uncleaned.eggs <- as.numeric(x$total.weight.dry.uncleaned.eggs)

# Calculate number of eggs:
x$fecundity <- x$number.in.subsample.1 * x$dry.weight.1.clean.dry.eggs.out.of.oven / x$weight.of.subsample.1 

plot(x$carapace.width, x$fecundity)

plot(x$carapace.width, x$fecundity)

