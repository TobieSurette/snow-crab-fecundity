library(jpeg)

files <- dir(path = "data/photos", full.names = TRUE)

files <- files[-grep("DSC", files)]
files <- files[-grep("Flat", files)]
files <- files[-grep("GP", files)]



x <- readJPEG(files[1], native = FALSE)

plot(c(0, dim(x)[2]), c(0, dim(x)[1]), xaxs = "i", yaxs = "i")

rasterImage(x, 0, 0, dim(x)[2], dim(x)[1])

for (i in 1:length(files)){
   print(files[i])
   x <- readJPEG(files[i], native = FALSE)
   print(dim(x))
}

x <- readJPEG(files[1], native = FALSE)
y <- readJPEG(files[3], native = FALSE)
   
xx <- as.vector(x[,,3])
yy <- as.vector(y[,,3])
index <- sample(1:length(xx), 100000)
xx <- xx[index]
yy <- xx[index]

for (i in 1:3){
print(mean(as.vector(x[,,i])[index]))
print(mean(as.vector(y[,,i])[index]))
print(sd(as.vector(x[,,i])[index]))
print(sd(as.vector(y[,,i])[index]))
}

logit <- function(p) log(p / (1-p))
plot(logit(xx), logit(yy))

plot(c(0.0, 1), c(0.0, 1), xaxs = "i", yaxs = "i", cex = 0.05)
for (i in 1:1){
   x <- readJPEG(files[i], native = FALSE)
   w <- mean(diff(sort(unique(as.vector(x[,,2])))))
   
   xx <- jitter(x[,,1], amount = w/2)
   yy <- jitter(x[,,3], amount = w/2)
   
   index <- sample(1:length(xx), 25000)
   
   points(xx[index], yy[index], cex = 0.05, col = rainbow(length(files))[i])
}  

p <- as.vector(x[,,2])
hist(log(p / (1-p)), n = 1000, col = "grey", ylim = c(0, 1000000))


