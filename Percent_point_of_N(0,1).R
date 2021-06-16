#========================#
# CDF and qnorm of normal 
#========================#

set.seed(15-06-2021)

xseq <- seq(-4, 4, 0.01)

desity <- dnorm(xseq, mean = 0, sd = 1)
cumulative <- pnorm(xseq, mean = 0, sd = 1)
rddeviate <- rnorm(1000, 0, 1)

par(mfrow = c(1, 3), mar = c(3, 4, 4, 2))

plot(xseq, desity, col = "darkgreen", type = "l", lwd = 2, cex = 2,
     xlab = "", ylab = "Cummulative probability", cex.axis = .8,
     main = "PDF of std Normal")

plot(xseq, cumulative, col = "darkorange", type = "l",
     lwd = 2, cex = 2, xlab = "", ylab = "Cumulative probability",
     cex.axis = .8, main = "CDF of std Normal")

hist(rddeviate, main = "Random draws from std Normal",
     cex.axis = .8, xlim = c(-4, 4))



#------------#
# More on CDF
#------------#

xseq2 <- seq(0, 1, 0.01)
length(xseq2) # 101

cummu2 <- pnorm(xseq2, mean = 0, sd = 1)
cummu3 <- qnorm(xseq2, mean = 0, sd = 1)
# what are the possible percentage points Zp values
# if the lower area value p is within (0, 1)


plot(xseq, cumulative, col = "darkgreen", type = "l", lwd = 2, 
     cex = 2, cex.axis = .8, xlab = "", ylab = "Cumulative probability",
     main = "CDF of std Normal")

plot(xseq, cummu2, col = "darkorange", type = "l", lwd = 2, 
     cex = 2, cex.axis = .8, xlab = "", ylab = "Cumulative probabilty",
     main = "CDF of N(-1, 4)")

plot(xseq2, cummu3, col = "darkblue", type = "l", lwd = 2, 
     cex = 2, cex.axis = .8, xlab = "", ylab = "Percentage point Zp of N(0,1)",
     main  = "qnorm of (0, 1)")

hist(cummu3, probability = T) # follows N(0, 1)
#curve(dnorm(x, mean = mean(cummu3), sd = sd(cummu3)),
#      add = T, col = "darkblue", lwd = 2)


