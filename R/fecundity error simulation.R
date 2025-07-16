fecundity <- 50000     # Number of egg in a single female.
mu.egg.weight <- 60  # Average egg weight (micrograms).
sigma.egg.weight <- 2
count.precision <- 0.01  

sub.count <- 200
weight.precision <- 1
k <- 5000  # Number of simulations per case.

clg()
png(file = "figures/Fecundity error simulation.png", width = 7, height = 10, res = 600, units = "in")
m <- rbind(0, cbind(0, kronecker(matrix(1:6, ncol = 2), matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))

for (weight.precision in c(1, 0.1)){
   for (sub.count in c(100, 200, 500)){
      rr <- NULL
      for (i in 1:k){
         egg.weights <- rnorm(fecundity, mu.egg.weight, sigma.egg.weight) / 1000000 # Simulate weights.
         
         total.weight <- 1000 * sum(egg.weights) # Total clutch weight in milligrams.
         
         sub.weight <- 1000 * sum(sample(egg.weights, sub.count)) # Subsample weights in milligrams.
         
         fhat <- total.weight / (sub.weight / sub.count)
         
         mean.egg.weight <- rnorm(1, sub.weight, weight.precision / 2) / rnorm(1, sub.count, count.precision * count / 2)
         
         fhat2 <- rnorm(1, total.weight, weight.precision / 2) / mean.egg.weight
         
         rr <- rbind(rr, c(mean.egg.weight.sample = 1000 * sub.weight / sub.count,
                           mean.egg.weight.obs = 1000 * mean.egg.weight,
                           fecundity.sample = fhat, 
                           fecundity.obs = fhat2))
      }
      
      a <- round(sd(rr[,"fecundity.obs"]) / 5)
      t <- table(round(rr[,"fecundity.obs"] / a) * a)
      t <- t / sum(t)
      gbarplot(t, xlim = c(40000, 60000), ylim = c(0, 0.1), xaxs = "i", col = fade("grey50"), lwd = 0.5, xaxt = "n", yaxt = "n")
      grid()
      tt <- table(round(rr[,"fecundity.sample"] / 100) * 100)
      tt <- max(t) * tt / max(tt)
      lines(as.numeric(names(tt)), tt, col = fade("blue"), lwd = 2)
      vline(fecundity, col = fade("red"), lty = "dashed", lwd = 2)

      if (sub.count == 100 & weight.precision == 1){
         legend("topleft",
                legend = c("Observations with error", "Observations w/o error", "True value"),
                pch = c(22, NA, NA),
                pt.bg = c(fade("grey50"), NA, NA),
                lwd = c(NA, 2, 2),
                col = c("grey30", fade("blue"), fade("red")),
                lty = c("solid", "solid", "dashed"),
                pt.cex = c(3, 1, 1), box.col = NA,
                bg = NA)
      } 

      legend("topright",
             legend = c(#paste0("weight precision = ", weight.precision, " mg"),
                        paste0("count error rate = ", 100 * count.precision, "%"),
                        #paste0("# eggs in subsample = ", sub.count),
                        paste0("Mean egg weight = ", mu.egg.weight, " micro-g"),
                        paste0("Std. err. egg weight = ", sigma.egg.weight, " micro-g"),
                        paste0("Fecundity CV = ", round(100 * sqrt(sum((rr[, "fecundity.obs"] - fecundity)^2) / k) / fecundity, 1), " %") 
                        ),
             cex = 0.80,
             bg = NA,
             box.col = NA)
 
      if (weight.precision == 1 & sub.count %in% c(200, 500)) axis(2, at = seq(0, 0.08, by = 0.02))
      if (weight.precision == 1 & sub.count %in% c(100)) axis(2)
      if (sub.count == 500 & weight.precision == 1){
         axis(1, at = seq(40000, 55000, by = 5000))
         mtext("Calculated fecundity", 1, 2.75, at = 60000, font = 2, cex = 1.25)
      } 
      if (sub.count == 200 & weight.precision == 1){
         mtext("Relative frequency", 2, 2.5, font = 2, cex = 1.25)
      }
      if (sub.count == 500 & weight.precision == 0.1) axis(1)
      
      if (weight.precision == 0.1) mtext(paste0("Sample count = ", sub.count, " eggs"), 4, 1.0, font = 2)
      if (sub.count == 100)        mtext(paste0("Weight precision = ", weight.precision, " mg"), 3, 0.5, font = 2)
      
      box(col = "grey50")
   }
}
dev.off()


vars <- c("year", "month", "location")
tmp <- aggregate(list(w = 1000000 * x$sample.egg.weight / x$sample.egg.count), by = x[vars], mean, na.rm = TRUE)
ix <- match(x[vars], tmp[vars])
x$egg.weight.average <- tmp$w[ix]
f <- 1000000 * x$egg.weight / x$egg.weight.average

plot(x$fecundity, f)

hist(100 * (x$fecundity - f) / x$fecundity, n = 100)
quantile(100 * (x$fecundity - f) / x$fecundity, p = c(0.025, 0.975))


