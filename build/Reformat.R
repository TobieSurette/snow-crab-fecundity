# Reformat and correct snow crab fecundity data:
x <- read.csv("data/raw/Fecundity data 1989-2017.csv", header = TRUE, stringsAsFactors = FALSE)
names(x) <- tolower(names(x))

# Parse data field names:
fields <- names(x)
fields <- gsub("^ ", "", fields)
fields <- gsub(" ", ".", fields)
fields <- gsub("#", "", fields)
fields[fields == "lat"] <- "latitude"
fields[fields == "long"] <- "longitude"
fields[fields == "secteur"] <- "sector"
fields[fields == "condition"] <- "shell.condition"

# Update column names:
names(x) <- fields

# Reformat missing legs field:
x$missing.legs.left <- toupper(gsub("[; .]", "", x$missing.legs.left))
x$missing.legs.right <- toupper(gsub("[; .]", "", x$missing.legs.right))
x$missing.legs.left <- gsub("D", "H", x$missing.legs.left)
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

# Reformat total egg count:
x$total.egg.count <- gsub(",", "", x$total.egg.count)
x$total.egg.count[grep("DIV", x$total.egg.count)] <- ""
x$total.egg.count[grep("echa", x$total.egg.count)] <- ""
x$total.egg.count[grep("REF", x$total.egg.count)] <- ""
x$total.egg.count[grep("VALUE", x$total.egg.count)] <- ""
x$total.egg.count <- as.numeric(x$total.egg.count)
x$total.egg.count[which(x$total.egg.count == 0)] <- NA

x$month[x$project == "Survey"]


# Reformat sample egg count:
x$sample.egg.count[grep("0.035", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("echa", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("Lost", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("na", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("drop", x$sample.egg.count)] <- ""
x$sample.egg.count <- as.numeric(x$sample.egg.count)
index <- which(x$year == 2001 & x$sample.egg.count < 200)
x$sample.egg.count[index] <- NA
x$sample.egg.weight[index] <- NA

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
index <- which(gsub("[0-9E.-]", "", x$sample.egg.weight) != "")
x$egg.comment <- ""
x$egg.comment[index] <- x$sample.egg.weight[index]
x$sample.egg.weight[index] <- ""

# Egg comment corrections:
index <- grep("echap", tolower(x$egg.comment))
x$egg.comment[index] <- "dropped"
index <- grep("drop", tolower(x$egg.comment))
x$egg.comment[index] <- "Dropped"
x$egg.comment[x$egg.comment == "na"] <- ""
x$egg.comment <- gsub("*", "", x$egg.comment, fixed = TRUE)
x$egg.comment <- gsub("^ +", "", x$egg.comment)
x$egg.comment <- gsub(" $", "", x$egg.comment)
index <- which(x$eggs.dropped != "")
x$egg.comment[index] <- x$eggs.dropped[index]

fields[fields == "secteur"] <- "sector"

# Update parasite field:
index <- grep("parasite", tolower(x$egg.comment))
x$parasite[index] <- "Parasite infested"

# Reformat total egg weight:
x$total.egg.weight[gsub("[0-9.]", "", x$total.egg.weight) != ""] <- ""
x$total.egg.weight <- as.numeric(x$total.egg.weight)

# Latitude and longitude corrections:
x$latitude <- gsub("48.02", "4802", x$latitude)
x$latitude[x$latitude == "47\xf850.612"] <- "4750.612"
x$latitude <- as.numeric(x$latitude) 
x$longitude <- as.numeric(substr(x$longitude, 1, 2)) + 
               as.numeric(substr(x$longitude, 3, 4)) / 60 + 
               + (x$longitude - floor(x$longitude)) / 3600
x$longitude <- -abs(x$longitude)
x$latitude <- as.numeric(substr(x$latitude, 1, 2)) + 
               as.numeric(substr(x$latitude, 3, 4)) / 60 + 
               + (x$longitude - floor(x$latitude)) / 3600

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
x$gonad.color[x$gonad.color == "O"]    <- "ORANGE" 
x$gonad.color[x$gonad.color == "OB"]   <- "BEIGE-ORANGE"
x$gonad.color[x$gonad.color == "BO"]   <- "BEIGE-ORANGE" 
x$gonad.color[x$gonad.color == "OC"]   <- "LIGHT ORANGE"
x$gonad.color[x$gonad.color == "OF"]   <- "DARK ORANGE"
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
x$location <- gsub("FOURCHU", "Fourchu", x$location)
x$location <- gsub("^GP ", "GP", x$location)
x$location <- gsub("; ", "-", x$location)

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
x <- x[, -which(names(x) %in% c("eggs.dropped", "missing.legs.left", "missing.legs.right"))]

# Correct gonad weight:
x$gonad.weight[x$gonad.weight == "na"] <- ""
x$gonad.weight[x$gonad.weight == "5..2726"] <- "5.2726"
x$gonad.weight <- gsub("[*]", "", x$gonad.weight)
x$gonad.weight <- as.numeric(x$gonad.weight)

# Fix reproductive status:
x$reproductive.status[x$reproductive.status == "M;"] <- "M"
x$reproductive.status[x$reproductive.status == "M1;P2"] <- "M1"

"year",	"month", "day",	"project",	"location", "gear", "vessel",	"tow.number", "station", "crab.number"

preservation	zone	latitude	longitude	sector

shell.condition	reproductive.status	carapace.width	abdomen.width	weight
egg.color	gonad.color	gonad.weight	eggs.remaining	sample.egg.weight	sample.egg.count
total.egg.weight	total.egg.count
spermatheca.weight	spermatheca.volume	sperm.square.count	sperm.sample.volume	sperm.sample.count sperm.color	spermatheca.sperm	
egg.development	egg.comment	parasite	eggs.valid	comment	poids.hepato
sperm.points.noires	proteines	eggs.vis	gon.vis	hep.vis	missing.legs





# Compile study table:

# Compile site table:


