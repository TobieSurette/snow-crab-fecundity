

x <- read.csv("data/fecundity.csv")

t <- table(x$year, x$location)

str <- as.character(t)
dim(str) <- dim(t)
dimnames(str) <- dimnames(t)
for (i in 1:nrow(t)){
   for (j in 1:ncol(t)){
      if (t[i,j] > 0){
         #print(c(as.numeric(rownames(t))[i], colnames(t)[j]))
         ix <- which((x$year == as.numeric(rownames(t))[i]) & (x$location == colnames(t)[j]))
         if (length(ix) > 0){
            #print(paste0(t[i,j], "(", paste0(unique(x$season[ix]), collapse = ","), ")"))
            str[i,j] <- paste0(str[i,j], " (", paste0(month.abb[unique(x$month[ix])], collapse = ","), ")")
            #str[i,j] <- paste0(str[i,j], describe(x$month[ix]))
         }
      }
   }
}

plot(x$carapace.width, x$fecundity, cex = 0.25)
abline(-1.269, 2.93, col = "red", lwd = 2)
abline(-1.587, 2.93, col = "red", lwd = 2)

