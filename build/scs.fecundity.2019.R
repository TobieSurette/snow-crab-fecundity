library(gulf.data)

data <- read.csv("data/raw/scs.fecundity.2019.csv")

names(data) <- gsub("..", ".", tolower(names(data)), fixed = TRUE)
names(data) <- gsub("[.]$", "", tolower(names(data)))

# 
data$crab.number <- as.numeric(gsub("[-]*C[1-5][-]*", "", data$crab.number))

# Format time fields:
data$start.time.counting <- substr(gulf.utils::time(data$start.time.counting), 1, 5)
data$end.time.counting <- substr(gulf.utils::time(data$end.time.counting), 1, 5)

# Tow ID format:
v <- formatC(gsub("[FA][1-5]*", "", toupper(data$tow.id)), width = 2, flag = "0", format = "d")
v <- gsub(" ", "0", substr(v, 2, 4))
suffix <- gsub("^[0-9]+", "", toupper(data$tow.id))
suffix[suffix == ""] <- "F"
data$tow.id <- paste0("GP", v, suffix)

s <- read.scsset(2019, valid = 1, survey = "regular")
b <- read.scsbio(2019, valid = 1, survey = "regular")

ix <- match(data["tow.id"], s["tow.id"])
data$date <- s$date[ix]
data$tow.number <- s$tow.number[ix]

vars <- c("tow.id", "crab.number")
ix <- match(data[vars], b[vars])

data$carapace.width <- b$carapace.width[ix]
data$tow.number <- s$tow.number[ix]


# Format data fields:
vars <- names(data)[grep("date", names(data))]
for (i in 1:length(vars)){
   v <- data[, vars[i]]
   v <- gsub("-", "/", v)
   v <- gsub("[*]", "", v)
   
   ix <- setdiff(which(nchar(v) == 8), c(grep("2019", v), grep("2020", v), grep("2021", v)))
   
   v[v == "13/12/19"] <- "19/12/13"
   v[v == "17/12/19"] <- "19/12/17"
   v[v == "16/12/19"] <- "19/12/16"
   
   if (length(ix) > 0) v[ix] <-  paste0("20", v[ix])
   
   #data[, vars[i]] <- as.character(gulf.utils::date(v))
   #print(data[, vars[i]]) 
   
   ix <- which(v != "")
   year  <- unlist(lapply(strsplit(v[ix], "/"), function(x) x[3]))
   month <- unlist(lapply(strsplit(v[ix], "/"), function(x) x[2]))
   day   <- unlist(lapply(strsplit(v[ix], "/"), function(x) x[1]))
   
   # 
   iy <- nchar(year) < 4  & !is.na(year) & !is.na(day)
   tmp      <- year[iy]
   year[iy] <- day[iy]
   day[iy]  <- tmp
   
   day   <- gsub(" ", "0", formatC(as.numeric(day), width = 2))
   month <- gsub(" ", "0", formatC(as.numeric(month), width = 2))
   d <- paste0(year, "/", month, "/", day)
   print(vars[i])

   v[ix] <- d
   
   data[, vars[i]] <- v 
}

head(data)

data$number.in.subsample.1 <- as.numeric(gsub("[(]P[)]", "", data$number.in.subsample.1))
data$weight.of.subsample.1 <- as.numeric(data$weight.of.subsample.1 )
data$dry.weight.1.clean.dry.eggs.out.of.oven <- as.numeric(data$dry.weight.1.clean.dry.eggs.out.of.oven)

w <- data$dry.weight.1.clean.dry.eggs.out.of.oven
s <- data$weight.of.subsample.1 
n <- data$number.in.subsample.1
data$fecundity <- n * w / s

ix  <- which(!is.na(data$fecundity) & !is.na(data$carapace.width) & data$carapace.width > 40)
   
library(TMB)
compile("scs_fecundity_2019.cpp")
dyn.load(dynlib("scs_fecundity_2019"))

parameters <- list(beta = 3, log_alpha_primiparous = -1.90, log_alpha_multiparous = -1.55,
                   log_sigma_primiparous = -1.65, log_sigma_multiparous = -1.65, log_sigma_outlier = 1, 
                   logit_p_multiparous = 0, logit_p_outlier = 0)
map <- lapply(parameters, function(x) factor(x *NA))
map$log_sigma_primiparous <- factor(1)
map$log_sigma_multiparous <- factor(1)
#map$log_sigma_outlier     <- factor(1)


map$logit_p_multiparous <- factor(1)
map$logit_p_outlier <- factor(1)

map$log_sigma_outlier <- factor(1)

data$ftow <- match(data$tow.id, unique(data$tow.id)) - 1

tmp<- data

b <- read.csv("data/biological.csv")
data = list(x = data$carapace.width[ix], y = data$fecundity[ix], tow = data$ftow)
parameters$log_sigma_tow_effect = -1
parameters$tow_effect <- rep(0, max(data$tow)+1)

map$tow_effect <- factor(rep(NA, length(parameters$tow_effect)))
map$log_sigma_tow_effect <- factor(NA)

# data = list(x = b$carapace.width[!is.na(b$carapace.width) & !is.na(b$egg.total.number)], y = b$egg.total.number[!is.na(b$carapace.width) & !is.na(b$egg.total.number)])
obj <- MakeADFun(data = data, parameters = parameters, random = "tow_effect", map = map, DLL = "scs_fecundity_2019")
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt
opt$hessian ## <-- FD hessian from optim
obj$he()    ## <-- Analytical hessian
sdreport(obj)
parameters$log_sigma_primiparous <- opt$par[["log_sigma_primiparous"]]
parameters$log_sigma_multiparous <- opt$par[["log_sigma_multiparous"]]
parameters$logit_p_multiparous   <- opt$par[["logit_p_multiparous"]]
parameters$logit_p_outlier       <- opt$par[["logit_p_outlier"]]
parameters$log_sigma_outlier     <- opt$par[["log_sigma_outlier"]]

map$beta <- factor(1)
map$log_alpha_primiparous <- factor(1)
map$log_alpha_multiparous <- factor(1)

obj <- MakeADFun(data = data, map = map, parameters = parameters, random = "tow_effect", DLL = "scs_fecundity_2019")
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt
opt$hessian ## <-- FD hessian from optim
obj$he()    ## <-- Analytical hessian
sdreport(obj)

parameters$beta                  <- opt$par[["beta"]]
parameters$log_alpha_primiparous <- opt$par[["log_alpha_primiparous"]]
parameters$log_alpha_multiparous <- opt$par[["log_alpha_multiparous"]]
parameters$log_sigma_primiparous <- opt$par[["log_sigma_primiparous"]]
parameters$log_sigma_multiparous <- opt$par[["log_sigma_multiparous"]]
parameters$logit_p_multiparous   <- opt$par[["logit_p_multiparous"]]
parameters$logit_p_outlier       <- opt$par[["logit_p_outlier"]]

obj <- MakeADFun(data = data, parameters = parameters, random = "tow_effect", DLL = "scs_fecundity_2019")
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt
opt$hessian ## <-- FD hessian from optim
obj$he()    ## <-- Analytical hessian
sdreport(obj)


parameters$beta                  <- opt$par[["beta"]]
parameters$log_alpha_primiparous <- opt$par[["log_alpha_primiparous"]]
parameters$log_alpha_multiparous <- opt$par[["log_alpha_multiparous"]]
parameters$log_sigma_primiparous <- opt$par[["log_sigma_primiparous"]]
parameters$log_sigma_multiparous <- opt$par[["log_sigma_multiparous"]]
parameters$logit_p_multiparous   <- opt$par[["logit_p_multiparous"]]
parameters$logit_p_outlier       <- opt$par[["logit_p_outlier"]]

plot(log(data$x), log(data$y), pch = 21, bg = "grey", col = "grey45", cex = 0.5, xlim = c(3.7, 4.45), ylim = c(9, 11.5))
abline(parameters$log_alpha_primiparous, parameters$beta, col = "red", lwd = 3)
abline(parameters$log_alpha_primiparous - obj$report()$sigma_primiparous, parameters$beta, col = "red", lwd = 2, lty = "dashed")
abline(parameters$log_alpha_primiparous + obj$report()$sigma_primiparous, parameters$beta, col = "red", lwd = 2, lty = "dashed")
abline(parameters$log_alpha_multiparous, parameters$beta, col = "blue", lwd = 3)
abline(parameters$log_alpha_multiparous - obj$report()$sigma_multiparous, parameters$beta, col = "blue", lwd = 2, lty = "dashed")
abline(parameters$log_alpha_multiparous + obj$report()$sigma_multiparous, parameters$beta, col = "blue", lwd = 2, lty = "dashed")

s <- read.scsset(2019, valid = 1, survey = "regular")
tmp$tow.id <- gsub(" ", "", tmp$tow.id)
ix <- match(tmp$tow.id, s$tow.id)

z <- obj$report()$tow_effect

iz <- z[tmp$ftow+1] > 0
plot(lon(s)[ix][iz], lat(s)[ix][iz], cex = 100*sqrt(z[tmp$ftow+1][iz]))
points(lon(s)[ix][!iz], lat(s)[ix][!iz], cex = 100*sqrt(-z[tmp$ftow+1][!iz]), col = "red")
map("coast")



