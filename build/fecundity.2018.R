library(gulf.data)

x <- read.csv("data/raw/fecundity.2018.csv")

names(x) <- tolower(names(x))
x <- x[-which(is.na(x$date.sampled)), ]

# Fix dates:
tmp <- as.character(x$date.measured)
x$date.measured <- as.character(date(paste0(substr(tmp, 1, nchar(tmp)-6), "-", substr(tmp, nchar(tmp)-5, nchar(tmp)-4), "-", substr(tmp, nchar(tmp)-3, nchar(tmp)))))

tmp <- as.character(x$date.sampled)
x$date.sampled <- as.character(date(paste0(substr(tmp, 1, nchar(tmp)-6), "-", substr(tmp, nchar(tmp)-5, nchar(tmp)-4), "-", substr(tmp, nchar(tmp)-3, nchar(tmp)))))

names(x) <- gsub("[.]+", ".", names(x))
names(x) <- gsub("^x.", "", names(x))
names(x) <- gsub("[.]$", "", names(x))
names(x) <- gsub("abd[.]", "abdomen.", names(x))
names(x) <- gsub("col[.]", "colour.", names(x))
names(x) <- gsub("color", "colour", names(x))
names(x) <- gsub("gon[.]", "gonad.", names(x))
names(x) <- gsub("hep[.]", "hepato.", names(x))
names(x) <- gsub("^crab$", "crab.number", names(x))
names(x) <- gsub("^claw.height$", "chela.height", names(x))
names(x) <- gsub("commentaire", "comment", names(x))

x$egg.colour   <- egg.colour(deblank(x$egg.colour))
x$gonad.colour <- gonad.colour(x$gonad.colour)

x$sex[which(x$sex == FALSE)] <- 2

x$maturity[x$maturity == "I"] <- "immature"
x$maturity[x$maturity == "P"] <- "primiparous"
x$maturity[x$maturity == "M"] <- "multiparous"

# Remove "*"s:
for (i in 1:ncol(x)) x[which(x[, i] == "*"), i] <- "" 

# Convert numerical fields:
for (i in 1:ncol(x)){
   if (length(grep("date", names(x)[i])) == 0){
      ix <- which(gsub("[-0-9.]", "", x[, i]) == "")
      if (length(ix) == nrow(x)) x[, i] <- as.numeric(x[, i])
   }
} 

x <- compress(x)

x$comment[is.na(x$comment)] <- ""

# Prepare egg count data:
y <- read.csv("data/raw/fecundity.egg.count.2018.csv")
names(y) <- tolower(names(y))
names(y) <- gsub("[.]+", ".", names(y))
names(y) <- gsub("[.]$", "", names(y))
names(y) <- gsub("crab.id", "crab.number", names(y))
names(y) <- gsub("comments", "comment.egg", names(y))
names(y) <- gsub("total.weight", "egg.weight", names(y))
names(y)[grep("develop", names(y))] <- "egg.development"
names(y) <- gsub("x.of.eggs.counted", "count.egg.sample", names(y))
names(y) <- gsub("weight.sub.sample", "weight.egg.sample", names(y))
y <- y[!is.na(y$crab.number), ]

# Join data sets:
match(x$crab.number, y$crab.number)

write.csv(x, file = "data/fecundity.2018.csv", row.names = FALSE)

