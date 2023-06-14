library(gulf.data)

# Read 2019 snow crab survey data:
s <- read.scsset(year = 2019, valid = 1, survey = "regular")
b <- read.scsbio(year = 2019, valid = 1, survey = "regular")

# Fecundity data from the 2019 snow crab survey:
x <- read.csv("C:/Users/SuretteTJ/Desktop/snow-crab-fecundity/data/Fecundity 2019 (Emily and Murray for Tobie).csv")

# Read colorimeter data:
y <- read.csv(locate(package = "gulf.data", keyword = c("color", "2019")))
y <- y[y$body.part == "eggs", ]

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
#x$fecundity <- x$number.in.subsample.1 * x$dry.weight.1.clean.dry.eggs.out.of.oven / x$weight.of.subsample.1 

# Sampler:
ix <- which(x$sampler %in% c("1", "2"))
for (i in 1:length(ix)) x$sampler[ix[i]] <- paste0(x$sampler[ix[i]-1], x$sampler[ix[i]])
x$sampler <- deblank(gsub(" [12]+ *", "", x$sampler))
x$sampler[x$sampler == "Murray2"] <- "Murray"
x$sampler <- gsub("Kat", "Katherine", x$sampler)

# Process:
x$process <- gsub("[*]", "", x$process)
x$process <- deblank(gsub("WBC", " Weighed before counting", x$process))

# Egg development stage:
x$development.stage <- gsub("[* ]", "", x$development.stage)
x$egg.development.comment <- x$comments.3.development 
x$development.stage[is.na(x$development.stage) | (x$development.stage == "NA")] <- ""
ix <- grep("NF", x$development.stage)
x$egg.development.comment[ix] <- paste0(x$egg.development.comment[ix], "; Not fertilized")
ix <- grep("B", x$development.stage)
x$egg.development.comment[ix] <- paste0(x$egg.development.comment[ix], "; Big patches of stain")
ix <- grep("S", x$development.stage)
x$egg.development.comment[ix] <- paste0(x$egg.development.comment[ix], "; Irregular spotting in eggs/Dev not clear")
ix <- setdiff(grep("F", x$development.stage), grep("NF", x$development.stage))
x$egg.development.comment[ix] <- paste0(x$egg.development.comment[ix], "; Fertilized but development not clear")
x$egg.development.comment <- deblank(x$egg.development.comment)
x$egg.development.comment <- gsub("^; ", "", x$egg.development.comment)
x$egg.development.comment[x$egg.development.comment == "fertilized but develop not clear; Fertilized but development not clear"] <- "Fertilized but development not clear"
x$egg.development.comment[x$egg.development.comment == "Irregular spotting in eggs; Irregular spotting in eggs/Dev not clear"] <- "Irregular spotting in eggs/Dev not clear"
x$egg.development.comment[x$egg.development.comment == "big patches of stain; Big patches of stain"] <- "Big patches of stain"
x$egg.development <- x$development.stage
x$comment.egg.development <- x$egg.development.comment

# Clean egg sample and weight data:
x$number.in.subsample.1 <- as.numeric(gsub("[*]", "", x$number.in.subsample.1))
x$weight.of.subsample.1 <- as.numeric(gsub("[*]", "", x$weight.of.subsample.1))
x$dry.weight.1.clean.dry.eggs.out.of.oven <- as.numeric(gsub("[*]", "", x$dry.weight.1.clean.dry.eggs.out.of.oven))
x$number.in.subsample.2 <- as.numeric(gsub("[*]", "", x$number.in.subsample.2))
x$weight.of.subsample.2 <- as.numeric(gsub("[*]", "", x$weight.of.subsample.2))
x$dry.weight.2.clean.dry.eggs.out.of.oven <- as.numeric(gsub("[*]", "", x$dry.weight.2.clean.dry.eggs.out.of.oven))

x$egg.weight   <- x$dry.weight.1.clean.dry.eggs.out.of.oven
x$egg.weight.2 <- x$dry.weight.2.clean.dry.eggs.out.of.oven
x$egg.weight.sample <- x$weight.of.subsample.1 
x$egg.count.sample  <- x$number.in.subsample.1
x$egg.weight.sample.2 <- x$weight.of.subsample.2 
x$egg.count.sample.2  <- x$number.in.subsample.2

x$comment.rinsing <- x$comments.1.rincage
x$comments.cleaning <- x$comments.2.nettoyage

x$date.counting <- x$date.eggs.were.counted 

# Remove extraneous fields:
remove <- c("combined.wet.weight", 
            "wet.weight.egg.and.pleopods",
            "wet.weight.setaes.only",
            "wet.weight.abdomen.flap",
            "total.weight.dry.uncleaned.eggs",
            "dry.weight.egg.dust.only",
            "dry.weight.pleopods.only", 
            "dry.weight.pleopods.plus.dust", 
            "dry.weight.1.clean.dry.eggs.out.of.oven", 
            "number.in.subsample.1",
            "weight.of.subsample.1",
            "dry.weight.2.clean.dry.eggs.out.of.oven",
            "number.in.subsample.2",
            "weight.of.subsample.2",
            "weight.to.add",
            "comments.1.rincage",
            "comments.2.nettoyage",
            "egg.development.comment",
            "date.eggs.were.counted",
            "time.out.of.formalin",
            "legend", "development.stage", "comments.3.development", "date.of.egg.cleaning", "date.weighed.pleopod.plus.dust", "date.weighed.egg.dust.only", "date.weighed.dry.pleopods.only")
x <- x[,-which(names(x) %in% remove)]

# Attach colorimeter data:
ix <- match(x[c("tow.id", "crab.number")], y[c("tow.id", "crab.number")])
tmp <- y[ix, c("gonad.colour", "colour.L", "colour.a", "colour.b")]
names(tmp) <- c("gonad.colour", "egg.colour.L", "egg.colour.a", "egg.colour.b")
tmp$gonad.colour[is.na(tmp$gonad.colour)] <- ""
x <- cbind(x, tmp)

# Re-order fields:
vars <- c("date", "tow.id", "tow.number", "crab.number", "sampler", "process", "carapace.width", "abdomen.width", "abdomen.height", "shell.condition",
          "egg.development", "egg.weight", "egg.colour", "gonad.colour", "egg.colour.L", "egg.colour.a", "egg.colour.b",
          "egg.weight.sample", "egg.count.sample",  "egg.weight.2", "egg.weight.sample.2", "egg.count.sample.2",
          "date.measured", "date.counting", "start.time.counting", "end.time.counting")
x <- x[, c(vars, setdiff(names(x), vars))]
x <- cbind(x[, -grep("comment", names(x))], x[, grep("comment", names(x))])
rownames(x) <- NULL

x <- sort(x, by = c("date", "tow.number", "crab.number"))

write.csv(x, file = "data/scs.2019.csv", row.names = FALSE)

