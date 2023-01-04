library(gulf.utils)
library(gulf.metadata)
library(gulf.data)
library(gulf.spatial)

#========================================== 1986-1988 data ==============================================
files <- dir(path = "data/raw", full.names = TRUE)
files <- files[-grep("1989-2017", files)]
#files <- files[-grep("Colormeter", files)] #this is eliminating all??

res <- NULL
for (i in 1:length(files)){
   x <- read.csv(paste0(files[i]), header = TRUE, stringsAsFactors = FALSE, fileEncoding = "Windows-1252")

   str <- names(x)
   str <- gsub("boat", "vessel", str)
   str[str == "Comments"] <- "comment" #why not str <- gsub("Comments", "comment", str)
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
      vars <- setdiff(names(x), names(res)) #outputs names that are in x but not in res
      res[vars] <- NA
      vars <- setdiff(names(res), names(x))
      x[vars] <- NA
      res <- rbind(res, x)
   }
}


# Remove incomplete records:
res <- res[!is.na(res$year), ] #removes records where year is NA

# Standardize variables:
res$sampler[res$sampler == "Mikio"] <- "Mikio Moriyasu" #could I use res$sampler <- gsub("Mikio", "Mikio Moriyasu", res$sampler)
res$sampler[is.na(res$sampler)] <- ""

# Vessel fix:
res$vessel[is.na(res$vessel)] <- ""
res$vessel[res$vessel == 'Opilio'] <- "CCGS Opilio"

# Define project:
res$project <- "fecundity"

# Shell condition fix:
res$shell.condition <- gsub(" ", "", res$shell.condition)
res$shell.condition[res$shell.condition %in% c("1", "n", "new-med")] <- "new" #could I use res$shell.condition[res$shell.condition == "1"] <- "new"  
                                                                              # or res$shell.condition <- gsub("1", "new", res$shell.condition)
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
res$egg.colour[index] <- res$eggs[index] #what is the difference between eggs and egg.colour
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
res$egg.total.weight <- res$egg.weight
res$weight[which((res$weight > 400) & (res$year == 1988))] <- NA

res$gear[is.na(res$gear)] <- ""
index <- !is.na(res$sample.weight) | !is.na(res$sample) #??
res$egg.sample.number #??

index <- !is.na(res$sample.count) & is.na(res$egg.sample.count) #don't see a egg.sample.count ??
res$egg.sample.count[index] <- res$sample.count[index]  #??

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
res <- res[, -which(names(res) %in% c("comment1", "comment2", "loran", "eggs", "carapace.width.lab", "egg.sample.count", "egg.total.weight", "sample.count"))]
f <- round(10000 * res$egg.weight * res$egg.sample.count / res$egg.sample.weight) #Why * 10000
res$fecundity[is.na(res$fecundity)] <- f[is.na(res$fecundity)]

res$egg.preservation <- "formaldehyde"

site.early <- unique(res[c("sampler", "project", "vessel", "location", "sampling.location", "year", "month", "day", "egg.preservation")])

y <- res

#========================================= 1989+ data ===========================================

# Reformat and correct snow crab fecundity data:
x <- read.csv("data/raw/Fecundity data 1989-2017.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "Windows-1252")
names(x) <- tolower(names(x))

# Parse data field names:
fields <- names(x)
fields <- gsub("^ ", "", fields)
fields <- gsub(" ", ".", fields)
fields <- gsub("#", "", fields)
fields[fields == "lat"]               <- "latitude"
fields[fields == "long"]              <- "longitude"
fields[fields == "secteur"]           <- "sector"
fields[fields == "condition"]         <- "shell.condition"
fields[fields == "sample.egg.weight"] <- "egg.sample.weight"
fields[fields == "sample.egg.count"]  <- "egg.sample.number"
fields[fields == "total.egg.weight"]  <- "egg.total.weight"
fields[fields == "total.egg.count"]   <- "egg.total.number"
fields[fields == "poids.hepato"]      <- "hepato.weight"
fields[fields == "hep.vis"]           <- "hepato.visual"
fields[fields == "eggs.vis"]          <- "egg.visual"
fields[fields == "gon.vis"]           <- "gonad.visual"
fields[fields == "proteines"]         <- "proteins" 
fields[fields == "secteur"]           <- "sector"

# Update column names:
names(x) <- fields

# Reformat missing legs field:
x$missing.legs.left <- toupper(gsub("[; .]", "", x$missing.legs.left))
x$missing.legs.right <- toupper(gsub("[; .]", "", x$missing.legs.right))
x$missing.legs.left <- gsub("D", "H", x$missing.legs.left) #H?
x$missing.legs.right <- gsub("D", "H", x$missing.legs.right)

# Function to convert strings to standard missing leg format:
as.missing.legs <- function(x){
   r <- rep("", length(x))
   for (i in 1:length(x)){
      if (!is.na(x[i]) & (x[i] != "")){
         str <- toupper(unlist(strsplit(x[i], "")))
         str <- c(str, "")
         v <- rep("x", 5)
         for (j in 1:5){
            index <- which(str == as.character(j))
            if (length(index) > 0){
               v[j] <- "1"
               if (str[index + 1] == "R") v[j] <- "2"
               if (str[index + 1] == "H") v[j] <- "7"
            }
         }
         r[i] <- paste0(v, collapse = "")           
      }
   }

   return(r)
}

x$missing.legs.left <- as.missing.legs(x$missing.legs.left)
x$missing.legs.right <- as.missing.legs(x$missing.legs.right)
index <- x$missing.legs.right == "" & x$missing.legs.left != ""
x$missing.legs.right[x$missing.legs.right == "" & x$missing.legs.left != ""] <- "xxxxx"  
x$missing.legs.left[x$missing.legs.right != "" & x$missing.legs.left == ""] <- "xxxxx"
x$missing.legs <- paste0(x$missing.legs.left, x$missing.legs.right)

#--------------------------------here
# Reformat total egg number:
x$egg.total.number <- gsub(",", "", x$egg.total.number)
x$egg.total.number[grep("DIV", x$egg.total.number)] <- ""
x$egg.total.number[grep("echa", x$egg.total.number)] <- ""
x$egg.total.number[grep("REF", x$egg.total.number)] <- ""
x$egg.total.number[grep("VALUE", x$egg.total.number)] <- ""
x$egg.total.number <- as.numeric(x$egg.total.number)
x$egg.total.number[which(x$egg.total.number == 0)] <- NA

# Reformat sample egg number:
x$egg.sample.number[grep("0.035", x$egg.sample.number)] <- ""
x$egg.sample.number[grep("echa", x$egg.sample.number)] <- ""
x$egg.sample.number[grep("Lost", x$egg.sample.number)] <- ""
x$egg.sample.number[grep("na", x$egg.sample.number)] <- ""
x$egg.sample.number[grep("drop", x$egg.sample.number)] <- ""
x$egg.sample.number <- as.numeric(x$egg.sample.number)
index <- which(x$year == 2001 & x$egg.sample.number < 200)
x$egg.sample.number[index] <- NA
x$egg.sample.weight[index] <- NA

# Correct eggs remaining:
x$eggs.remaining[which((x$year == 2013) & (x$eggs.remaining == "none"))] <- ""
x$eggs.remaining[which(x$eggs.remaining == "3 (70%)")] <- "70%"
x$eggs.remaining[which(x$eggs.remaining == "3 (80%)")] <- "80%"
x$eggs.remaining[which(x$eggs.remaining == "3 (85%)")] <- "85%"
x$eggs.remaining[which(x$eggs.remaining == "3 (90%)")] <- "90%"
x$eggs.remaining[which(x$eggs.remaining == "3 (95%)")] <- "95%"
x$eggs.remaining[which(x$eggs.remaining == "4 (85%)")] <- "85%"
x$eggs.remaining[which(x$eggs.remaining == "4 (90%)")] <- "90%"
x$eggs.remaining[which(x$eggs.remaining == "4 (95%)")] <- "95%"
x$eggs.remaining[which(x$eggs.remaining == "100+")] <- "100%"
x$eggs.remaining[which(x$eggs.remaining == "<26")] <- "1-25%"
index <- which(x$eggs.remaining %in% as.character(5 * (1:20)))
x$eggs.remaining[index] <- paste0(x$eggs.remaining[index], "%")
x$eggs.remaining[which(x$eggs.remaining == "4+")] <- "4"
x$eggs.remaining[which(x$eggs.remaining == "4")] <- "100%"
x$eggs.remaining[which(x$eggs.remaining == "3")] <- "75-99%"
x$eggs.remaining[which(x$eggs.remaining == "2")] <- "50-74%"
x$eggs.remaining[which(x$eggs.remaining == "1")] <- "1-49%"
x$eggs.remaining[which(x$eggs.remaining == "0")] <- "0%"
x$eggs.remaining[which(x$eggs.remaining == "*")] <- "" 
x$eggs.remaining[which(x$eggs.remaining == "NONE" & x$year == 2013)] <- "0%" 

# Reformat sample egg weight and extract egg comments:
index <- which(gsub("[0-9E.-]", "", x$egg.sample.weight) != "")
x$egg.comment <- ""
x$egg.comment[index] <- x$egg.sample.weight[index]
x$egg.sample.weight[index] <- ""

# Egg comment corrections:
index <- grep("echap", tolower(x$egg.comment))
x$egg.comment[index] <- "dropped"
index <- grep("drop", tolower(x$egg.comment))
x$egg.comment[index] <- "dropped"
x$egg.comment[x$egg.comment == "na"] <- ""
x$egg.comment <- gsub("*", "", x$egg.comment, fixed = TRUE)
x$egg.comment <- gsub("^ +", "", x$egg.comment)
x$egg.comment <- gsub(" $", "", x$egg.comment)
index <- which(x$eggs.dropped != "")
x$egg.comment[index] <- x$eggs.dropped[index]

# Update parasite field:
index <- grep("parasite", tolower(x$egg.comment))
x$parasite[index] <- "Parasite infested"
x$parasite[is.na(x$parasite)] <- ""

# Reformat total egg weight:
x$egg.total.weight[gsub("[0-9.]", "", x$egg.total.weight) != ""] <- ""
x$egg.total.weight <- as.numeric(x$egg.total.weight)

# Latitude and longitude corrections:
x$latitude <- gsub("48.02", "4802", x$latitude)
x$latitude[x$latitude == "47ø50.612"] <- "4750.612"
x$latitude <- as.numeric(x$latitude) 
x$longitude <- as.numeric(substr(x$longitude, 1, 2)) + 
               as.numeric(substr(x$longitude, 3, 4)) / 60 + 
               + (x$longitude - floor(x$longitude)) / 3600
x$longitude <- -abs(x$longitude)
x$latitude <- as.numeric(substr(x$latitude, 1, 2)) + 
               as.numeric(substr(x$latitude, 3, 4)) / 60 + 
               + (x$longitude - floor(x$latitude)) / 3600

# Import coordinates:
tmp <- data.frame(year       = 2006,
                  tow.number = c(2, 4, 7, 5, 5, 6),
                  location   = c("GP004", "GP005", "GP149", "GP035", "GP079", "GP350"),
                  latitude   = c(47.90333, 47.90837, 47.73698, 46.7142, 46.88015, 46.7211),
                  longitude  = c(-65.347, -65.2858, -64.0733, -61.935, -62.8897, -62.8805),
                  stringsAsFactors = FALSE)
index <- match(x[c("year", "location")], tmp[c("year", "location")])
ii <- which(!is.na(index))
x$tow.number[ii] <- tmp$tow.number[index[ii]]
x$longitude[ii]  <- tmp$longitude[index[ii]]
x$latitude[ii]   <- tmp$latitude[index[ii]]

# Fix gonad colors (what a mess):
x$gonad.color <- toupper(x$gonad.color)
x$gonad.color[x$gonad.color == "-"] <- ""
x$gonad.color[x$gonad.color == "*"] <- ""
x$gonad.color[x$gonad.color == "NONE"] <- ""
x$gonad.color[x$gonad.color == "BEI"] <- "BEIGE"
x$gonad.color <- gsub("[;*_ ]", "", x$gonad.color)
x$gonad.color[x$gonad.color == "BE-O"] <- "BEIGE-ORANGE"  
x$gonad.color[x$gonad.color == "BL"]   <- "WHITE"
x$gonad.color[x$gonad.color == "W"]    <- "WHITE"
x$gonad.color[x$gonad.color == "BE"]   <- "BEIGE"
x$gonad.color[x$gonad.color == "B"]    <- "BEIGE"
x$gonad.color[x$gonad.color == "O"]    <- "ORANGE" 
x$gonad.color[x$gonad.color == "OB"]   <- "BEIGE-ORANGE"
x$gonad.color[x$gonad.color == "BO"]   <- "BEIGE" # "BEIGE-ORANGE" 
x$gonad.color[x$gonad.color == "OC"]   <- "LIGHT ORANGE"
x$gonad.color[x$gonad.color == "OF"]   <- "ORANGE" # "DARK ORANGE"
x$gonad.color[which((x$gonad.color == "B") & !is.na(x$total.egg.weight))] <- "BEIGE"

# Shell condition corrections:
x$shell.condition <- toupper(x$shell.condition)
x$shell.condition <- gsub(" ", "", x$shell.condition)
x$shell.condition <- gsub("[*]", "", x$shell.condition)
x$shell.condition[x$shell.condition %in% c("2.5")] <- "2M" 
x$shell.condition[x$shell.condition %in% c("3.5", "3+", "3;4")] <- "3M" 

# Reproductive status corrections:
x$reproductive.status <- toupper(x$reproductive.status)

# Reformat colour variables:
vars <- c("hep.l", "hep.a", "hep.b", "gon.l", "gon.a", "gon.b", "egg.l", "egg.a", "egg.b")
for (i in 1:length(vars)) x[, vars[i]] <- gsub("[*]", "", x[, vars[i]])
for (i in 1:length(vars)) x[, vars[i]] <- as.numeric(x[, vars[i]])

# Remove data rows with duplicates:
vars <- c("year", "month", "day", "crab.number", "station", "location")
index  <- which(duplicated(x[vars]))
if (length(index) > 0) x <- x[-index, ]

# Correct sampling vessel:
x$vessel[grep("perley", tolower(x$vessel))]   <- "CCGS Perley"
x$vessel[grep("opilio", tolower(x$vessel))]   <- "CCGS Opilio"
x$vessel[grep("thinking", tolower(x$vessel))] <- "Fishfull Thinking"
x$vessel[grep("marco", tolower(x$vessel))]    <- "Marco-Michel"
x$vessel[grep("britanny", tolower(x$vessel))] <- "Britanny Maddison"

# Correct locations table:
x$location[x$location == "Baie Chaleurs"] <- "Baie des Chaleurs"
x$location[x$location == "Banc  Bradelle"] <- "Bradelle Bank"
x$location[x$location == "Banc Bradelle"] <- "Bradelle Bank"
x$location[x$location == "Banc Bradelle N"] <- "Bradelle Bank North"
x$location[x$location == "chaleur 1"] <- "Baie des Chaleurs 1"
x$location[x$location == "chaleur 2"] <- "Baie des Chaleurs 2"
x$location[x$location == "MARGAREE CAGE"] <- "Margaree"    
x$location[x$location == "MARGAREE TRAP"] <- "Margaree"  
x$location[x$location == "MARGAREE TRAPS"] <- "Margaree"
x$location[x$location == "CHETICAMP CAGE"] <- "Cheticamp"    
x$location[x$location == "CHETICAMP TRAP"] <- "Cheticamp"  
x$location[x$location == "CHETICAMP TRAPS"] <- "Cheticamp"
x$location[x$location == "GRANDE RIVIERE"] <- "Grande Riviere"
x$location <- gsub("LOUISBOURG", "Louisbourg", x$location)
x$location <- gsub("FOURCHU", "", x$location)
x$location <- gsub("^;", "", x$location)
x$location <- gsub("^GP ", "GP", x$location)
x$location <- gsub("; ", "-", x$location)

#index <- which(x$year == 2007 & x$month == 6 & x$day == 20)
#x$gear[index] <- "beam trawl"
#x$project[index] <- "Tagging"
#index <- which(x$year == 2007 & x$month == 6 & x$day == 20)
#18 et 20 juin 2007 : Marco Michel

# Compile colorimeter data table:
body.part <- c("gonad", "eggs", "hepato-pancreas")
r <- NULL
for (i in 1:nrow(x)){
   for (j in 1:length(body.part)){
      tmp <- x[i, paste0(substr(body.part[j], 1, 3), ".", c("l", "a", "b"))]
      if (!any(is.na(tmp))){
         r <- rbind(r, cbind(x[i, vars], data.frame(body.part = body.part[j], L = tmp[,1], a = tmp[,2], b = tmp[,3])))
      }
   }
}

# Remove redundant fields:
x <- x[, -grep("[.][lab]$", names(x))]
x <- x[, -which(names(x) %in% c("eggs.dropped", "missing.legs.left", "missing.legs.right", "eggs.valid"))]

# Correct gonad weight:
x$gonad.weight[x$gonad.weight == "na"] <- ""
x$gonad.weight[x$gonad.weight == "5..2726"] <- "5.2726"
x$gonad.weight <- gsub("[*]", "", x$gonad.weight)
x$gonad.weight <- as.numeric(x$gonad.weight)
x$gonad.weight[which(x$gonad.weight > 30)] <- NA

# Fix reproductive status:
x$reproductive.status[x$reproductive.status == "M;"] <- "M"
x$reproductive.status[x$reproductive.status == "M1;P2"] <- "M1"

x$project[x$project == "June Survey"] <- "June survey"
x$project[x$project == "Suvey"] <- "Survey"

x$gear[x$gear == "" & !is.na(x$tow.number)] <- "Trawl"
x$gear <- tolower(x$gear)
x$preservation <- tolower(x$preservation)

# Correct abdomen width:
x$abdomen.width[x$abdomen.width == "35;74"] <- 35.74
x$abdomen.width <- as.numeric(x$abdomen.width)
x$abdomen.width[x$abdomen.width == 0] <- NA
x$abdomen.width[x$abdomen.width > 90] <- NA

# Correct carapace width:
x$carapace.width[x$carapace.width >= 100] <- NA

# Remove crap in character fields:
for (i in 1:ncol(x)){
   if (is.character(x[, i])){
      x[, i] <- gsub("[*]+", "*", x[, i])
      x[which(toupper(x[, i]) %in% c("*", "-", "?", "NA", "REF!", "VALUE!", "DIV;0!")), i] <- ""
   }
}

# Correct general comments:
x$comment <- gsub("_ufs", "oeufs", x$comment)
x$comment <- gsub(";+", ";", x$comment) 
x$comment[grep("gonade sous-developpee", x$comment, fixed = TRUE)] <- "cote gauche de gonade sous-developpee"

# Correct egg colour:
x$egg.color <- toupper(x$egg.color)
x$egg.color <- gsub(" ", "", x$egg.color)
x$egg.color[x$egg.color %in% c("OF+BR", "FO")] <- "OF"
x$egg.color[x$egg.color %in% c("NONE", "DEAD", "R")] <- ""
x$egg.color[x$egg.color == "OC+OF(15%)"] <- "OC"
x$egg.color[x$egg.color == "BRUN"] <- "BR"
x$egg.color[x$egg.color %in% c("OFQQE", "QQEOF", "OP")] <- "OC"
x$egg.color[x$egg.color == "O"]  <- "ORANGE"
x$egg.color[x$egg.color == "OF"] <- "DARK ORANGE"
x$egg.color[x$egg.color == "OC"] <- "LIGHT ORANGE"
x$egg.color[x$egg.color == "BR"] <- "BROWN"

# Correct hepato visual:
hepato.color <- data.frame(code = c(as.character(1:14), c("cm", "m", "dm")),
                           description = c("Brown", "Watery brown", "Dark brown-black", "Brown blackish greenish", "Light brown", 
                                           "Watery light brown", "Brown-pinkish", "Dark brown", "Watery dark brown", "Brown-reddish", 
                                           "Dark green/greenish", "Watery green/greenish", "Reddish",	"Yellowish", "Clear maroon", "Maroon", "Dark maroon"),
                           stringsAsFactors = FALSE)
x$hepato.color <- hepato.color$description[match(x$hepato.vis, hepato.color$code)]
x$hepato.color[is.na(x$hepato.color)] <- ""

# Drop visual in favour of gonad.color
x$gonad.visual <- toupper(c("white", "beige", "orange")[as.numeric(x$gonad.visual)])
x$gonad.visual[is.na(x$gonad.visual)] <- ""
index <- which((x$gonad.color == "") & (x$gonad.visual != ""))
x$gonad.color[index] <- x$gonad.visual[index]

# Fix egg color:
x$egg.visual <- toupper(c("light orange", "dark orange", "black", "cocoon")[as.numeric(x$egg.visual)])
x$egg.visual[is.na(x$egg.visual)] <- ""
index <- which((x$egg.color == "") & (x$egg.visual != ""))
x$egg.color[index] <- x$egg.visual[index]
index <- which(x$egg.color %in% as.character(1:3))
x$egg.color[index] <- toupper(c("light orange", "dark orange", "black", "cocoon")[as.numeric(x$egg.color[index])]) 
x$egg.color[x$egg.color == "BROWN"] <- "BLACK"

# Drop visual variables:
x <- x[, -which(names(x) %in% c("egg.visual", "gonad.visual", "hepato.visual"))]

# Correct egg sampole weight:
x$egg.sample.weight <- as.numeric(x$egg.sample.weight)
x$egg.sample.weight[which(x$egg.sample.weight == 0)] <- NA

index <- which(x$egg.sample.weight == 1.4583)
tmp <- x$egg.total.weight[index]
x$egg.sample.weight[index] <- x$egg.total.weight[index]
x$egg.total.weight[index] <- tmp
x$egg.total.number[index] <- NA

index <- which(x$egg.sample.weight > 0.1) 
x$egg.sample.weight[index] <- NA
x$egg.total.number[index] <- NA

# Check 0 % eggs remaining:
x$eggs.remaining[which(x$eggs.remaining == "0%" & (x$egg.total.weight > 0))] <- ""
x$egg.total.weight[which(x$egg.sample.weight == x$egg.total.weight)] <- NA
x$eggs.remaining[which(x$eggs.remaining == "5%" & (x$egg.total.weight > 0))] 

# Egg total weight:
x$egg.total.weight[which(x$egg.total.weight > 10)] <- NA

# Correct comments:
x$comment <- tolower(x$comment)
x$comment <- gsub("sinile", "senile", x$comment)
x$comment <- gsub("beoge", "beige", x$comment)
x$comment <- gsub("gond", "gonade", x$comment)
index <- setdiff(grep("gon", tolower(x$comment)), grep("gonad", tolower(x$comment)))
x$comment[index] <- gsub("gon", "gonade", x$comment[index], fixed = TRUE)
index <- setdiff(grep("gon", tolower(x$comment)), grep("gonad", tolower(x$comment)))
x$comment[index] <- gsub("gon", "gonade", x$comment[index], fixed = TRUE)
x$comment <- gsub("pts", "points", x$comment)
x$comment <- gsub("petite_", "petite", x$comment)
x$comment <- gsub("pt ", "point ", x$comment)
x$comment <- gsub(" org", " orange", x$comment)
x$comment <- gsub("org ", " orange ", x$comment)
x$comment <- gsub("ptrouge", "points rouges", x$comment)
x$comment <- gsub("pntrouge", "points rouges", x$comment)
x$comment <- gsub("spermateques", "spermatheque", x$comment)
x$comment <- gsub(" oc", " orange clair", x$comment)

# Total number of eggs:
index <- which((x$year == 2002) & (x$egg.sample.weight < 1E-4))
x$egg.weight <- NA
x$egg.weight[index] <- x$egg.sample.weight[index]
x$egg.sample.number[index] <- 500
x$egg.sample.weight[index] <- x$egg.sample.number[index] * x$egg.weight[index]

# Length measurement precisions:
x$carapace.width.precision <- NA
x$carapace.width.precision[x$year == 1989] <- 0.5
x$carapace.width.precision[x$year == 1990] <- 1
x$carapace.width.precision[x$year %in% 1991:2001] <- 0.1
x$carapace.width.precision[x$year > 2001] <- 0.01

x$abdomen.width.precision <- NA
x$abdomen.width.precision[x$year == 1989] <- 0.5
x$abdomen.width.precision[x$year == 1990] <- 1
x$abdomen.width.precision[x$year %in% 1991:2001] <- 0.1
x$abdomen.width.precision[x$year > 2001] <- 0.01

x$maturity.stage <- x$reproductive.status

# Sperm corrections:
x$sperm.points.noires <- gsub("[ ;.]", "", x$sperm.points.noires)

# Re-order variables:
vars <- c('year','month','day', 'project', 'zone', 'sector', 'location', 'station', 'tow.number', 'latitude', 'longitude', 'gear', 'vessel',
          'preservation', 'crab.number', 'sex', 'carapace.width', 'abdomen.width', 'weight', 'shell.condition', 'maturity.stage', 'missing.legs',
          'egg.color', 'eggs.remaining', 'egg.sample.weight', 'egg.sample.number', 'egg.total.weight', 'egg.total.number', 'egg.development',
          'parasite', 'gonad.weight', 'gonad.color', 
          'spermatheca.weight', 'spermatheca.volume', 'sperm.square.count', 'sperm.sample.volume', 'sperm.sample.count', 'sperm.color', 'spermatheca.sperm', 
          'sperm.points.noires', 'hepato.weight', 'hepato.color', 'proteins', 'comment', 'egg.comment')
x <- x[vars]

# Compile study table:
vars <- c("year", "month", "day", "project", "zone", "location", "sector", "station", "gear", "vessel", "preservation") 
res <- aggregate(list(female.number = x$longitude), by = x[vars], length)
res <- cbind(res, aggregate(x[c("longitude", "latitude")], by = x[vars], mean, na.rm = TRUE)[c("longitude", "latitude")])
res$tow.number <- unlist(lapply(aggregate(x["tow.number"], by = x[vars], function(x) unique(x[!is.na(x)]))[, "tow.number"], paste, collapse = ","))
res <- res[order(date(res)), ]
rownames(res) <- NULL

# 'color' with 'colour' replace:
names(x) <- gsub("color", "colour", names(x))
names(r) <- gsub("color", "colour", names(r))
names(res) <- gsub("color", "colour", names(res))

# Output files:
#write.csv(x, file = "data/biological.csv", row.names = FALSE)
#write.csv(r, file = "data/colorimeter.csv", row.names = FALSE)
#write.csv(res, file = "data/sites.csv", row.names = FALSE)


 
 
 
 
