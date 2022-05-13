# exercise 4.1
# part A

e4_1 <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/e4_1.csv")
e4_1$population = e4_1$population/100000000
e4_1$totalcrimes = e4_1$totalcrimes/1000000
plot(e4_1$population, e4_1$totalcrimes)

# part B

library(ggplot2)
library(dplyr)
library(fANCOVA)

scatter1 <- e4_1 %>% ggplot(aes(x = population, y = totalcrimes)) +
  geom_point(color = 'red') + theme_classic() + 
  labs(x='population', y = 'total crime')

smooth1 <- loess.as(e4_1$population, e4_1$totalcrimes, degree = 1,
                    criterion = 'aicc')

# optimal smoothing parameter in R

r.aic <- smooth1$pars$span

scatter1 +
  ggtitle('Loess Curve for Degree 1', subtitle = 'Smoothing Parameter = 0.1792') +
  stat_smooth(geom = 'smooth', method = 'loess', span = r.aic, color = 'blue')

# predicting crimes for 2010

opt1 <- loess.as(e4_1$population, e4_1$totalcrimes, degree = 1,
                 user.span = r.aic)
predcrimes <- predict(opt1, data.frame(e4_1$population))
predcrimes[51]


# part C
# fit quadratic local polynomials.

smooth2 <- loess.as(e4_1$population, e4_1$totalcrimes, degree = 2,
                    criterion = 'aicc')
r.aic <- smooth2$pars$span

scatter1 +
  ggtitle('Loess Curve for Degree 2', subtitle = 'Smoothing Parameter = 0.3938') +
  stat_smooth(geom = 'smooth', method = 'loess', span = r.aic, color = 'blue')


# exercise 4.3

# part A
library(rgl)

fruit <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/e4_3.csv")
attach(fruit)

plot3d(sodas,fries,fruits)
fruit.loess <- loess(fruits~sodas+fries, degree = 1)
fruit.fit <- expand.grid(list(sodas=seq(min(sodas), max(sodas), 1),fries=seq(min(fries), max(fries), 1)))
fruit.predict <- predict(fruit.loess, newdata=fruit.fit)
persp3d(fruit.predict,col = rainbow(1000))

# part B
predict(fruit.loess, data.frame(sodas = 14, fries = 2))
detach(fruit)

# exercise 4.4
library(fields)

crim <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/e4_1.csv")
crim$pop = crim$population/100000000
crim$tot = crim$totalcrimes/1000000
attach(crim)

par(mfrow = c(2,2))
# m = 2
plot(pop,tot, col = 'red')
rate.tps <- Tps(pop, tot, m=2)
pop.grid <- seq(min(pop), max(pop), 1)
ratep<-predict(rate.tps, newdata=pop.grid)
se<-predictSE(rate.tps, newdata=pop.grid)
lines(pop, ratep-1.96*se, lty=2)
lines(pop, ratep+1.96*se, lty=2)
lines(pop, ratep, lty=1)

# m = 3
plot(pop,tot, col = 'red')
rate.tps <- Tps(pop, tot, m=3)
pop.grid <- seq(min(pop), max(pop), 1)
ratep<-predict(rate.tps, newdata=pop.grid)
se<-predictSE(rate.tps, newdata=pop.grid)
lines(pop, ratep-1.96*se, lty=2)
lines(pop, ratep+1.96*se, lty=2)
lines(pop, ratep, lty=1)

# m = 4
plot(pop,tot, col = 'red')
rate.tps <- Tps(pop, tot, m=4)
pop.grid <- seq(min(pop), max(pop), 1)
ratep<-predict(rate.tps, newdata=pop.grid)
se<-predictSE(rate.tps, newdata=pop.grid)
lines(pop, ratep-1.96*se, lty=2)
lines(pop, ratep+1.96*se, lty=2)
lines(pop, ratep, lty=1)

# exercise 4.5
boot <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/e4_2.csv")
boot$month <- 1:dim(boot)[1]
boot <- subset(boot, select = -c(X,X.1))
boot <- boot[complete.cases(boot),]
attach(boot)

par(mfrow = c(2,2))

# m = 2
plot(month,revenue, col = 'red')
rate.tps <- Tps(month, revenue, m=2)
month.grid <- seq(min(month), max(month), 1)
ratep<-predict(rate.tps, newdata=month.grid)
se<-predictSE(rate.tps, newdata=month.grid)
lines(month, ratep-1.96*se, lty=2)
lines(month, ratep+1.96*se, lty=2)
lines(month, ratep, lty=1)

# m = 3
plot(month,revenue, col = 'red')
rate.tps <- Tps(month, revenue, m=3)
month.grid <- seq(min(month), max(month), 1)
ratep<-predict(rate.tps, newdata=month.grid)
se<-predictSE(rate.tps, newdata=month.grid)
lines(month, ratep-1.96*se, lty=2)
lines(month, ratep+1.96*se, lty=2)
lines(month, ratep, lty=1)

# m = 4
plot(month,revenue, col = 'red')
rate.tps <- Tps(month, revenue, m=4)
month.grid <- seq(min(month), max(month), 1)
ratep<-predict(rate.tps, newdata=month.grid)
se<-predictSE(rate.tps, newdata=month.grid)
lines(month, ratep-1.96*se, lty=2)
lines(month, ratep+1.96*se, lty=2)
lines(month, ratep, lty=1)

# m = 5
plot(month,revenue, col = 'red')
rate.tps <- Tps(month, revenue, m=5)
month.grid <- seq(min(month), max(month), 1)
ratep<-predict(rate.tps, newdata=month.grid)
se<-predictSE(rate.tps, newdata=month.grid)
lines(month, ratep-1.96*se, lty=2)
lines(month, ratep+1.96*se, lty=2)
lines(month, ratep, lty=1)

# exercise 4.6

# part a
fruit <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter4/e4_3.csv")
attach(fruit)
plot3d(fries, sodas, fruits)

#fitting the tps model
fruit.tps<-Tps(cbind(fries, sodas), fruits, m=2, method="GCV.model")

#plotting fitted surface
fruit.fit<-expand.grid(list(seq(min(fries), max(fries), 1), 
                            seq(min(sodas), max(sodas), 1)))
fruit.predict<- predict(fruit.tps, fruit.fit)
persp3d(x = seq(min(fries), max(fries), 1), y = seq(min(sodas), max(sodas), 1), 
        fruit.predict, col= rainbow(1000))

# part b
predict(fruit.tps, data.frame(fries = 2, sodas = 14))

# exercise 4.7

# part a
library(gam)
library(plotly)
library(dplyr)

fruit.mod <- gam(fruits ~ sodas + s(fries), data = fruit,
                 family = gaussian)
attach(fruit)

fruit.grid <- expand.grid(sodas = seq(min(sodas), max(sodas), 1),
                          fries = seq(min(fries), max(fries), 1))
p_fruits <- predict(fruit.mod, fruit.grid)

fruit_surface <- plot_ly(z = ~p_fruits)
fruit_surface <- fruit_surface %>% add_surface()
fruit_surface %>% layout(scene = list(aspectmode = 'cube'))

# part b
fruit_pred <- data.frame(sodas = 14, fries = 2)
predict(fruit.mod, fruit_pred)

# exercise 5.1

# part a
library(gam)
med1 <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.1.csv")
med1$won <- ifelse(med1$won == "yes", 1, 0)

mod1 <- gam(won~lo(margin), data = med1, family = binomial(link = "logit"))
coefficients(summary.glm(mod1))
summary(mod1)

# part b
forPred <- data.frame(margin = -11)
predict(mod1, forPred, type = 'response')

# part c
marginVals <- sort(med1$margin)
predVals <- sort(mod1$fitted.values)
plot(marginVals, predVals, type = 'b', 
     xlab = 'margin', ylab = 'P_Win')
lines(marginVals, predVals, col = "purple")

# part d
# A REDO
mod2 <- gam(won~s(margin), data = med1, family = binomial(link = 'logit'))
coefficients(summary.glm(mod2))
summary(mod2)

# B REDO
predict(mod2, forPred, type = 'response')

# exercise 5.2

# part a
adh <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.2.csv")
adh$adherence <- ifelse(adh$adherence == "yes", 1, 0)
adh <- adh[complete.cases(adh),]

mod3 <- gam(adherence~gender+intervention+s(age), data = adh, 
            family = binomial(link = 'logit'))
coefficients(summary.glm(mod3))
summary(mod3)

# part b
adh$predict <- mod3$fitted.values
par(mfrow=c(2,2))

x1 <- adh[((adh$gender == 'M') & (adh$intervention == 'yes')),]$age
y1 <- adh[(adh$gender == 'M') & (adh$intervention == 'yes'),]$predict
plot(x1,y1, main = 'Gender = M & Interv = Yes')

x2 <- adh[((adh$gender == 'M') & (adh$intervention == 'no')),]$age
y2 <- adh[(adh$gender == 'M') & (adh$intervention == 'no'),]$predict
plot(x2,y2, main = 'Gender = M & Interv = No')

x3 <- adh[((adh$gender == 'F') & (adh$intervention == 'yes')),]$age
y3 <- adh[(adh$gender == 'F') & (adh$intervention == 'yes'),]$predict
plot(x3,y3, main = 'Gender = F & Interv = Yes')

x4 <- adh[((adh$gender == 'F') & (adh$intervention == 'no')),]$age
y4 <- adh[(adh$gender == 'F') & (adh$intervention == 'no'),]$predict
plot(x4,y4, main = 'Gender = F & Interv = No')

# exercise 5.3
library(mgcv)
ptsd <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.3.csv")
ptsd <- ptsd[complete.cases(ptsd),]
ptsd$ptsdBin <- ifelse(ptsd$score >= 50, 1, 0)

# part a
mod4 <- gam(ptsdBin ~ gender + injury + s(deployment,age, k = 10, bs = 'tp'), 
            data = ptsd, family = binomial(link = 'logit'))
coefficients(mod4)
summary(mod4)
pred4 <- data.frame(gender = 'M', age = 25, deployment = 12, injury = 'yes')
predict(mod4, pred4, type = 'response')

# part b
mod5 <- gam(ptsdBin ~ gender + injury + s(deployment) + s(age), 
            data = ptsd, family = binomial(link = 'logit'))
summary(mod5)
predict(mod5, pred4, type = 'response')

# exercise 5.4
library(gam)
acc <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.4.csv")
acc <- subset(acc, select = -c(X, X.1))
acc <- acc[complete.cases(acc),]

# part a
mod6 <- gam(naccidents ~  time_bound + lo(brighness), data = acc,
            family = poisson(link = 'log'))
coefficients(summary.glm(mod6))

# part b
mod7 <- gam(naccidents ~  time_bound + s(brighness), data = acc,
            family = poisson(link = 'log')) 
coefficients(summary.glm(mod7))

# exercise 5.5
errors <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter5/DATA for Exercise 5.5.csv")
errors <- subset(errors, select = -c(X, X.1))
errors <- errors[complete.cases(errors),]

# part A
mod8 <- gam(ntypos ~ s(cost) + s(circulation), data = errors,
            family = poisson(link = "log"))
coefficients(summary.glm(mod8))

# part B
PPred <- data.frame(circulation = 13.5, cost = 1.75)
predict(mod8, PPred, type = 'response')

# part C
library(rgl)
attach(errors)
grid<- expand.grid(cost = seq(min(cost), max(cost), 1),
                   circulation = seq(min(circulation), max(circulation), 1))

poissonThing <- predict(mod8, grid)

x_cost <- seq(min(cost), max(cost), 1)
y_circulation <- seq(min(circulation), max(circulation), 1)
persp3d(x_cost, y_circulation, poissonThing, theta = 150, phi = 20)

# part D
library(mgcv)

# part a REDO
mod9 <- gam(ntypos ~ s(cost, circulation, k=10, bs = 'tp'), data = errors,
            family = poisson(link = "log"))

coefficients(summary.glm(mod8))

# part b REDO
predict(mod9, PPred, type = 'response')

# part c REDO
grid<- expand.grid(cost = seq(min(cost), max(cost), 1),
                   circulation = seq(min(circulation), max(circulation), 1))

poissonThing <- predict(mod9, grid)

x_cost <- seq(min(cost), max(cost), 1)
y_circulation <- seq(min(circulation), max(circulation), 1)
persp3d(x_cost, y_circulation, poissonThing, theta = 150, phi = 20)
