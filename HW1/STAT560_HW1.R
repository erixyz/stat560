# Exercise 1.1

binom.test(3,10, alternative = 'less')

# Exercise 1.2

binom.test(5,7, alternative = 'greater')

# Exercise 1.3

binom.test(4,5)

# Exercise 1.4
library(exactRankTests)
start <- c(22.125, 23.5, 23.5, 25.875, 26.375,
           21.375,28.875,24.625,23.125,25)
twowks <- c(23.375,23.125,24.75,25.75,26.75,
            22.625,29,24,24.125,27.25)
wilcox.exact(start, twowks, paired = TRUE, alternative = 'less')

# Exercise 1.5
interv <- c(36,22,10,12,28,12,23,6)
control <- c(17,15,-8,-11,14,20,24,6)
wilcox.exact(interv, control, paired = T, alternative = 'greater')
length(interv)

# Exercise 1.6
week1 <- c(405.65,400.51,408.25,401.34,409.09)
week2 <- c(403.02,399.49,396.10,403.59,405.68)
wilcox.exact(week1,week2, paired = T)

# Exercise 1.7
tx <- c(5,4,6,4,3,4,4,3,5,5)
cx <- c(7,8,12,10,8,9,10)

wilcox.exact(tx,cx, paired = F, alternative = 'less')

# Exercise 1.8
ado <- c(15,8,8,10,6,9,7,8)
adu <- c(13,17,10,12,13,17,15,17,17,19)

wilcox.exact(ado,adu, paired = F, alternative = 'less')

# Exercise 1.9
fem <- c(10,7,11,8,5,12,13)
men <- c(7,6,8,5,3,6,7,6,3,2)

wilcox.exact(fem,men,paired = F, alternative = 'two.sided')

# Exercise 1.10
work <- c(1,4,14,7,11,1,8,10)
home <- c(1,3,5,5,4,3,4,5)

wilcox.exact(work,home,paired=F,alternative = 'two.sided')
ansari.exact(work,home,alternative = 'less')

# Exercise 1.11
ucla <- c(27,37,40,63,31,81,63,57,90,94)
const <- c(56,78,60,55,67,68,64)

wilcox.exact(ucla,const,paired=F,alternative = "two.sided")
ansari.exact(ucla,const,alternative = 'less')
