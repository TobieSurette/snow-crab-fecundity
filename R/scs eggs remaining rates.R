

b <- read.scsbio(2000:2024, survey = "regular")
b <- b[which(b$sex == 2), ]
b <- b[which(b$eggs.remaining %in% 1:4), ]

table(b$eggs.remaining)

table(b$shell.condition, b$eggs.remaining)

ix <- which(b$shell.condition == 3)

t <- table(round(b$carapace.width[ix]), b$eggs.remaining[ix])

t <- t / repvec(apply(t, 1, sum), ncol = 4)

gbarplot(t, y, 
         col = fade(c("red", "yellow", "green", "blue")), 
         legend = FALSE, ylim = c(0, 0.1), xlim = c(40, 80))

