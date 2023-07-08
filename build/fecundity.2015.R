library(gulf.data)

# Read ASCII file:
y <- readLines("data/raw/fecundity.dissection.2015.txt")

# Parse date:
x <- data.frame(date = paste0(substr(y, 5, 8), "-", substr(y, 3, 4), "-", substr(y, 1, 2)))
x$zone <- deblank(substr(y, 9, 11))
x$vessel <- deblank(substr(y, 12, 32))
x$sampler <- deblank(substr(y, 33, 63))

# Coordinates:
x$longitude <- as.numeric(substr(y, 64, 72))
x$latitude  <- as.numeric(substr(y, 73, 81))
x$longitude[floor(x$longitude) == 9999] <- NA
x$latitude[floor(x$latitude) == 9999] <- NA
x$depth <- as.numeric(gsub("[ *]+", "", substr(y, 83, 85)))
x$trap.number <- as.numeric(gsub("[ *]+", "", substr(y, 87, 91)))

# Sampling time?
x$start.time <- deblank(substr(y, 93, 97))
x$start.time[x$start.time == "*"] <- ""
x$end.time   <- deblank(substr(y, 99, 103))
x$end.time[x$end.time == "*"] <- ""

x$crab.number <- as.numeric(substr(y, 105, 108))
x$sex <- as.numeric(substr(y, 109, 110))
x$shell.condition <- gsub("[ *]+", "", substr(y, 112, 113))  
x$carapace.width <- as.numeric(substr(y, 116, 120))  

# Female variables:
x$abdomen.width  <- as.numeric(substr(y, 121, 126)) 
x$egg.colour     <- as.numeric(substr(y, 128, 128)) 
x$eggs.remaining <- as.numeric(substr(y, 129, 130)) 
x$gonad.colour   <- as.numeric(gsub("[ *]+", "", substr(y, 131, 132)))

# Diseases:
x$missing.legs   <- substr(y, 134, 143)
x$maturity       <- deblank(substr(y, 144, 145))
x$barnacles      <- as.numeric(gsub("[ *]+", "", substr(y, 146, 147))) 
x$spiroidea      <- as.numeric(gsub("[ *]+", "", substr(y, 148, 149))) 
x$cigarette.burn <- as.numeric(gsub("[ *]+", "", substr(y, 150, 151))) 


# Read colorimeter file:
y <- read.csv("data/raw/fecundity.laboratory.2015.csv")
names(y) <- tolower(names(y))
y$date <- as.character(date(year  = as.numeric(substr(y$date, nchar(y$date)-3, nchar(y$date))), 
                            month = as.numeric(substr(y$date, nchar(y$date)-5, nchar(y$date)-4)),  
                            day   = as.numeric(substr(y$date, nchar(y$date)-7, nchar(y$date)-6))))
names(y) <- gsub("commentaire", "comment", names(y))

# Read fecundity file:
z <- read.csv("data/raw/fecundity.2015.csv")
z <- sort(z, by = c("location", "crab.number"))
names(z) <- gsub("comment", "egg.comment", names(z))
z2 <- z[which(z$location != "Snow Crab Survey"), ]
z <- z[which(z$location == "Snow Crab Survey"), ]

# Attach fecundity data:
ix <- which(month(y) == 10)
iy <- match(y$crab.number[ix], z$crab.number)
y[, c("egg.count.sample", "egg.weight.sample", "egg.weight")] <- NA
y$egg.comment <- ""
y$egg.count.sample[ix]  <- z$egg.count.sample[iy]
y$egg.weight.sample[ix] <- z$egg.weight.sample[iy]
y$egg.weight[ix]        <- z$egg.weight[iy]
y$egg.comment[ix]       <- z$egg.comment[iy]
rm(z)

ix <- which((y$date == "2015-09-16") & (y$sex == 2))
iy <- match(y$crab.number[ix], z2$crab.number + 9)
y$egg.count.sample[ix]  <- z2$egg.count.sample[iy]
y$egg.weight.sample[ix] <- z2$egg.weight.sample[iy]
y$egg.weight[ix]        <- z2$egg.weight[iy]
y$egg.comment[ix]       <- z2$egg.comment[iy]
rm(z2)

# Attach dissection data:
y$abdomen.width <- NA
vars <- setdiff(names(y), names(x))
x[vars] <- NA
x$comment <- ""
x$egg.comment <- ""
x <- x[names(y)]
y <- rbind(x, y)

# Fix final version:
y$vessel[y$vessel == 'CCGC M.PERLEY'] <- 'CCGS M.PERLEY'
y$sampler <- gsub(" CORMIER", " R. CORMIER", y$sampler)
y$depth <- as.numeric(NA)
y$trap.number <- as.numeric(gsub("[ *]+", "", y$trap.number))
y$start.time  <- gsub("[ *]+", "", y$start.time)
y$end.time    <- gsub("[ *]+", "", y$end.time)
y$chela.height <- as.numeric(gsub("[ *]+", "", y$chela.height))
y$egg.colour  <- as.numeric(gsub("[ *]+", "", y$egg.colour))
y$eggs.remaining <- as.numeric(gsub("[ *]+", "", y$eggs.remaining))
y$gonad.colour   <- as.numeric(gsub("[ *]+", "", y$gonad.colour))

y$maturity <- as.numeric(gsub("[ *]+", "", y$maturity))
y$barnacles <- as.numeric(gsub("[ *]+", "", y$barnacles))
y$spiroidea <- as.numeric(gsub("[ *]+", "", y$spiroidea))
y$cigarette.burn <- as.numeric(gsub("[ *]+", "", y$cigarette.burn))

y$l1 <- as.numeric(gsub("[ *]+", "", y$l1))
y$a1 <- as.numeric(gsub("[ *]+", "", y$a1))
y$b1 <- as.numeric(gsub("[ *]+", "", y$b1))

y$l2 <- as.numeric(gsub("[ *]+", "", y$l2))
y$a2 <- as.numeric(gsub("[ *]+", "", y$a2))
y$b2 <- as.numeric(gsub("[ *]+", "", y$b2))

y$l3 <- as.numeric(gsub("[ *]+", "", y$l3))
y$a3 <- as.numeric(gsub("[ *]+", "", y$a3))
y$b3 <- as.numeric(gsub("[ *]+", "", y$b3))

y$l4 <- as.numeric(gsub("[ *]+", "", y$l4))
y$a4 <- as.numeric(gsub("[ *]+", "", y$a4))
y$b4 <- as.numeric(gsub("[ *]+", "", y$b4))

y$l5 <- as.numeric(gsub("[ *]+", "", y$l5))
y$a5 <- as.numeric(gsub("[ *]+", "", y$a5))
y$b5 <- as.numeric(gsub("[ *]+", "", y$b5))

y$weight <- as.numeric(gsub("[ *]+", "", y$weight))

# Clean-up sampler list:
y$sampler <- gsub(" R.[ ]*CORMIER", ";RITA CORMIER", y$sampler)
y$sampler <- gsub(" MCCAIE", ";MATHIEU MCCAIE", y$sampler)
y$sampler <- gsub(" MORIYASU", " M. MORIYASU", y$sampler)
y$sampler <- gsub("M[.][ ]*MORIYASU", ";MIKIO MORIYASU", y$sampler)
y$sampler <- gsub("^ALLAIN", "R.ALLAIN", y$sampler)
y$sampler <- gsub("R[.][ ]*ALLAIN", "RENEE ALLAIN", y$sampler)
y$sampler <- gsub(" ;", ";", y$sampler) 
y$sampler <- gsub(";", "; ", y$sampler) 
y$sampler <- gsub("^;", "", y$sampler) 

write.csv(y, file = "data/fecundity.2015.csv", row.names = FALSE)
