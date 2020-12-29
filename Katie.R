library(gulf.utils)
library(gulf.metadata)
library(gulf.data)
library(gulf.spatial)

#==================================== 1986-1988 data =======================================
files <- dir(path = "data/raw")
files <- files[-grep("1989-2017", files)]
files <- files[-grep("Colormeter", files)]

res <- NULL
for (i in 1:length(files)){
   x <- read.csv(paste0("data/", files[i]), header = TRUE, stringsAsFactors = FALSE, fileEncoding = "Windows-1252")
   
   str <- names(x)
   str <- gsub("boat", "vessel", str)
   str[str == "Comments"] <- "comment"
   str[str == "comment.1"] <- "comment1"
   str <- gsub("lenght", "length", str)  
   str <- gsub("weight.egg", "egg.weight", str)  
   str <- gsub("egg.sample.count", "sample.count", str) 
   str <- gsub("weight.sample", "egg.sample.weight", str) 
   str <- gsub("claw", "chela", str) 
   str <- gsub("color", "colour", str) 
   names(x) <- str
   
   if (is.null(res)){
      res <- x
   }else{
      vars <- setdiff(names(x), names(res))
      res[vars] <- NA
      vars <- setdiff(names(res), names(x))
      x[vars] <- NA
      res <- rbind(res, x)
   }
}

# Standardize variables:
res$sampler[res$sampler == "Mikio"] <- "Mikio Moriyasu"
res$sampler[is.na(res$sampler)] <- ""

# Vessel fix:
res$vessel[is.na(res$vessel)] <- ""
res$vessel[res$vessel == 'Opilio'] <- "CCGS Opilio"

# Define project:
res$project <- "fecundity"

# Shell condition fix:
res$shell.condition <- gsub(" ", "", res$shell.condition)
res$shell.condition[res$shell.condition %in% c("1", "n", "new-med")] <- "new"
res$shell.condition[res$shell.condition %in% c("2", "m", "med")] <- "medium"
res$shell.condition[res$shell.condition %in% c("3", "o")] <- "old"
res$shell.condition[which(is.na(res$shell.condition) & res$maturity.stage == "p")] <- "new"
res$shell.condition[which(res$shell.condition == "" & res$maturity.stage == "p")] <- "new"
res$shell.condition[which(is.na(res$shell.condition) & res$maturity.stage == "m")] <- "medium"

# Maturity stage:
res$maturity.stage <- c("primiparous", "multiparous", "multiparous")[match(res$shell.condition, c("new", "medium", "old"))]

# Egg colour fix:
res$egg.colour <- gsub(" ", "", res$egg.colour)
index <- which(is.na(res$egg.colour) | res$egg.colour == "")
res$egg.colour[index] <- res$eggs[index]
res$egg.colour[res$egg.colour %in% c("o", "or", "or-y", "oy", "y")] <- "orange" 
res$egg.colour[res$egg.colour == "of"] <- "dark orange" 
res$egg.colour[res$egg.colour == "+"] <- ""
 
# Sex fix:
res$sex[which(is.na(res$sex) & (!is.na(res$fecundity) | !is.na(res$egg.sample.number)))] <- 2
res <- res[!is.na(res$sex), ]

# Convert loran coordinates:
index <- !is.na(res$loran) 
tmp <- loran2deg(as.numeric(unlist(lapply(strsplit(res$loran[index], "/"), function(x) x[1]))),
                 as.numeric(unlist(lapply(strsplit(res$loran[index], "/"), function(x) x[2]))))
res$longitude[index] <- tmp[, 1]
res$latitude[index] <- tmp[, 2]

# 
index <- which(is.na(res$egg.weight) & !is.na(res$egg.total.weight))
res$egg.weight[index] <- res$egg.total.weight[index]
res$weight[which((res$weight > 400) & (res$year == 1988))] <- NA

res$gear[is.na(res$gear)] <- ""

index <- !is.na(res$sample.weight) | !is.na(res$sample)

res$egg.sample.count <- res$egg.sample.number

index <- !is.na(res$sample.count) & is.na(res$egg.sample.count)
res$egg.sample.count[index] <- res$sample.count[index] 

# Species:
res$species <- "snow crab"
res$species[grep("hyas", res$comment)] <- "hyas sp."
res$sampling.location <- res$comment2
res$sampling.location[is.na(res$sampling.location)] <- ""

# Missing legs:
res$missing.legs[which(nchar(res$missing.legs) != 10)] <- ""

# Adomen width fix:
res$abdomen.width <- as.numeric(res$abdomen.width)
index <- !is.na(res$carapace.width.lab)
res$abdomen.width[index] <- res$carapace.width.lab[index]

# Comment fix:
res$comment <- deblank(res$comment)
res$comment1 <- deblank(res$comment1)
res$comment1[is.na(res$comment1)] <- ""
res$comment[is.na(res$comment)] <- ""
index <- res$comment1 != "" & res$comment != ""
res$comment[index] <- paste0(res$comment1[index], "; ", res$comment[index])
index <- res$comment1 != "" & res$comment == ""
res$comment[index] <- res$comment1[index]

# Remove old fields:
res <- res[, -which(names(res) %in% c("comment1", "comment2", "loran", "eggs", "carapace.width.lab", "egg.sample.number", "egg.total.weight", "sample.count"))]
f <- round(10000 * res$egg.weight * res$egg.sample.count / res$egg.sample.weight)
res$fecundity[is.na(res$fecundity)] <- f[is.na(res$fecundity)]

y <- res

#==================================== End 1986-1988 data =======================================
