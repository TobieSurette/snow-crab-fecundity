
x <- read.csv(file = "/Users/crustacean/Desktop/2019 SNOW CRAB FECUNDITY DATA MASTER (6).csv", 
                 stringsAsFactors = FALSE)
names(x) <- tolower(names(data))

# Parse data field names:
fields <- names(x)
fields <- gsub("^ ", "", fields)
fields <- gsub(" ", ".", fields)
fields <- gsub("#", "", fields)
fields[fields == "lat"] <- "latitude"
fields[fields == "long"] <- "longitude"
fields[fields == "secteur"] <- "sector"

names(x) < fields

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

# Reformat sample egg count:
x$sample.egg.count[grep("0.035", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("echa", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("Lost", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("na", x$sample.egg.count)] <- ""
x$sample.egg.count[grep("drop", x$sample.egg.count)] <- ""
x$sample.egg.count <- as.numeric(x$sample.egg.count)

# Reformat total egg weight:
x$total.egg.weight[gsub("[0-9.]", "", x$total.egg.weight) != ""] <- ""
x$total.egg.weight <- as.numeric(x$total.egg.weight)
plot(x$total.egg.weight, ylim = c(0, 8))

# Reformat sample egg weight and extract egg comments:
index <- gsub("[0-9E.-]", "", x$sample.egg.weight) != "" 
x$egg.comment <- ""
x$egg.comment[index] <- x$sample.egg.weight[index]
x$sample.egg.weight[index] <- ""
x$egg.comment[x$egg.comment == "na"] <- ""
x$egg.comment <- gsub("*", "", x$egg.comment, fixed = TRUE)
x$egg.comment <- gsub("^ +", "", x$egg.comment)
x$egg.comment <- gsub(" $", "", x$egg.comment)
x$sample.egg.weight[x$sample.egg.weight == "-"] <- ""
x$sample.egg.weight <- as.numeric(x$sample.egg.weight)
# There are a lot of numerical errors in this field.

# Spot fixes for latitude:
x$latitude <- gsub("48.02", "4802", x$latitude)
x$latitude[x$latitude == "47\xf850.612"] <- "4750.612"
x$latitude <- as.numeric(x$latitude) 

# Convert to decimal degree format:
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

#B


