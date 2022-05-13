library(exactRankTests)

# Exercise 1.12
tx <- c(2.1,1.6,3.8,3.2,4.0)
cx <- c(3.7,7.2,2.8,5.3,8.6)

ks.test(tx,cx, alternative = 'greater')

# Exercise 1.13
tx <- c(2.5,2.4,2.1,3.4,4.2,1.1,1.9)
cx <- c(-0.9,1.5,2.3,-1.6,-3.4,0.3,2.0,-1.1,1.6)

ks.test(tx,cx, alternative = 'less')

# Exercise 1.14
ucla <- c(37,27,40,62,31,81,63,57,90,94)
const <- c(56,78,60,55,67,68,64)

ks.test(ucla,const)

# Exercise 2.1
rnames <- c('s1','s2','s3','s4','s5','s6','s7')
cnames <- c('m1', 'm2', 'm3', 'm4')
books <- c(2,4,3,4,0,1,3,4,4,5,4,7,
           3,3,4,3,0,0,1,3,4,3,5,5,
           5,5,4,2)
data <- matrix(books, nrow = 7, byrow = T, 
               dimnames = list(rnames,cnames))
friedman.test(data)

# Exercise 2.2
letter <- c(0.4,0.8,0.5,0.7,0.6,0.6,0.6,0.7)
phone <- c(0.3,0.4,0.4,0.6,0.3,0.5,0.4,0.6)
text <- c(0.1,0.3,0.1,0.2,0.2,0.4,0.3,0.2)

dat.mat <- matrix(data = cbind(letter,phone,text), ncol = 3)
friedman.test(dat.mat)

wilcox.exact(letter,phone,paired=TRUE,alternative='two.sided')
wilcox.exact(letter,text,paired=TRUE,alternative='two.sided')
wilcox.exact(text,phone,paired=TRUE,alternative='two.sided')

# Exercise 2.3
pondA <- c(3,4,4,5,7,8)
pondB <- c(10,11,11,12,15,18)
pondC <- c(4,5,6,6,9,10)
pondsData <- list(pondA,pondB,pondC)

kruskal.test(pondsData)

wilcox.exact(pondA,pondB,paired=F,alternative="two.sided")
wilcox.exact(pondB,pondC,paired=F,alternative="two.sided")
wilcox.exact(pondA,pondC,paired=F,alternative="two.sided")

# Exercise 2.4 
c24 <- c(88,54,65,55)
c28 <- c(67,72,76,80)
c32 <- c(93,82,84,78)
c36 <- c(86,87,81,73)

germData <- list(c24,c28,c32,c36)
kruskal.test(germData)

# Exercise 3.1
gas <- c(1.78,2.11,2.01,2.17,2.45,2.76,3.12,3.24,3.56,
         3.70,3.42,3.24)
milk <- c(1.30,1.70,1.88,2.15,2.20,2.25,2.19,2.45,2.87,
          2.99,3.15,3.06)

cor.test(gas,milk,method="spearman",alternative="greater")

# Exercise 3.2
education <- c(2,3,2,2,3,3,3,2,1,3,1,1,2,3,1)
violence <- c(1,3,3,2,1,1,3,3,3,1,3,1,2,2,2)
cor.test(education,violence,method='spearman',alternative='less')

# Exercise 3.3
years <- 2000:2010
points <- c(1938,2019,2461,1557,1819,2832,2430,
            2323,2201,1970,2078)
cor.test(years,points,method = 'spearman',alternative = 'two.sided')

# Exercise 3.4
valve <- c(5,8,2,10,11,14,
           13,4,8,12,3,0)
v.mat <- matrix(valve, ncol=4)
fisher.test(v.mat)

# Exercise 3.5
schoo <- c(12, 8, 11,
           2, 4, 13)
schoo.mat <- matrix(schoo, ncol = 3)
fisher.test(schoo.mat)

# Exercise 3.6
penName <- c(6,9,
             4,11)
pen.mat <- matrix(penName, ncol=2)
fisher.test(pen.mat)

# Exercise 3.7
tribe <- c(9,6,7,3,2,0,
           4,3,5,2,3,3,
           2,6,5)
tribe.mat <- matrix(tribe,nrow=3)
tribe.mat
fisher.test(tribe.mat)
