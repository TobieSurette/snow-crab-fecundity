library(gulf.data)

x <- read.csv("data/raw/fecundity.2021.csv")
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

x$vessel <- "Avalon Voyager II"
x$egg.colour <- egg.colour(deblank(x$egg.colour))
x$gonad.colour <- gonad.colour(x$gonad.colour)

x$sex[which(x$sex == FALSE)] <- 2

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

write.csv(x, file = "data/fecundity.2021.csv", row.names = FALSE)

