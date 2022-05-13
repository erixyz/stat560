# Exercise 7.1
# part a
geyser <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter7/DATA for Exercise 7.1.csv", sep="")
hist(geyser$times, prob = T)
lines(density(geyser$times))

# part b
hist(geyser$times, breaks = seq(50,100, by = 5), prob = T)
lines(density(geyser$times, kernel = "epanechnikov"), col = 'red')
lines(density(geyser$times, kernel = "gaussian"), col = 'cornflowerblue')
lines(density(geyser$times, kernel = "triangular"), col = 'orange')

# Exercise 7.2
# part a
heights <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter7/DATA for Exercise 7.2.csv")
heights$diff = heights$husband - heights$wife
hist(heights$diff)

# part b
hist(heights$diff, breaks = seq(-5,20, by = 3), prob = T)
lines(density(heights$diff, kernel = "epanechnikov"), col = 'red')
lines(density(heights$diff, kernel = "gaussian"), col = 'cornflowerblue')
lines(density(heights$diff, kernel = "triangular"), col = 'orange')

# Exercise 7.3
# part a
leaf <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter7/DATA for Exercise 7.3.csv")
hist(leaf$length)

# part b
hist(leaf$length, breaks = seq(0,8, by = 0.5), prob = T)
lines(density(leaf$length, kernel = "epanechnikov"), col = 'red')
lines(density(leaf$length, kernel = "gaussian"), col = 'cornflowerblue')
lines(density(leaf$length, kernel = "triangular"), col = 'orange')

# Exercise 8.1
library(bootstrap)
theta <- jackknife(geyser$times, mean) 
lci <- mean(geyser$times)-theta$jack.bias + qt(0.025,length(geyser$times)-1)*theta$jack.se
uci <- mean(geyser$times)-theta$jack.bias - qt(0.025,length(geyser$times)-1)*theta$jack.se
c(lci, uci)

# Exercise 8.2
theta <- function(x,xdata){ cor(xdata[x,1],xdata[x,2], method = 'spearman') }
results <- jackknife(1:dim(heights)[1], theta, heights)

lci <- cor(heights$husband, heights$wife, method = 'spearman') - results$jack.bias + qt(0.025,dim(heights)[1]-1)*results$jack.se
uci <- cor(heights$husband, heights$wife, method = 'spearman') - results$jack.bias - qt(0.025,dim(heights)[1]-1)*results$jack.se

c(lci, uci)

# Exercise 8.3
gilk <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.1.csv")
results <- jackknife(1:dim(gilk)[1], theta, gilk)

lci <- cor(gilk$pmilk, gilk$pgas, method = 'spearman') - results$jack.bias + qt(0.025,dim(gilk)[1]-1)*results$jack.se
uci <- cor(gilk$pmilk, gilk$pgas, method = 'spearman') - results$jack.bias - qt(0.025,dim(gilk)[1]-1)*results$jack.se

c(lci, uci)

# Exercise 8.4
results <- jackknife(leaf$length, var)

lci <- var(leaf$length) - results$jack.bias + qt(0.025,dim(leaf)[1]-1)*results$jack.se
uci <- var(leaf$length) - results$jack.bias - qt(0.025,dim(leaf)[1]-1)*results$jack.se

c(lci,uci)

# Exercise 8.5
set.seed(1)
boot.results <- bootstrap(geyser$times, 1000, mean)
quantile(boot.results$thetastar, c(.025, .975))

# Exercise 8.6
set.seed(1)
boot.results <- bootstrap(1:dim(heights)[1], 1000, theta, heights)
quantile(boot.results$thetastar, c(.005, .995))

# Exercise 8.7
set.seed(1)
boot.results <- bootstrap(1:dim(gilk)[1], 1000, theta, gilk)
quantile(boot.results$thetastar, c(.025, .975))

# Exercise 8.8
set.seed(1)
boot.results <- bootstrap(leaf$length, 1000, var)
quantile(boot.results$thetastar, c(.05, .95))       
