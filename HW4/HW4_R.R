# HW 4
library(survival)

# Exercise 6.1

# estimates survival function
weeks <- c(3,4,4,4,8,8,16,22,24,30)
event <- c(0,0,0,1,0,1,0,0,1,1)

weeks.surv <- survfit(Surv(weeks, event==0)~ 1, conf.type="none", se.fit=FALSE)
summary(weeks.surv)

# plots survival function
plot(weeks.surv, mark.time = TRUE, 
     pch = 1, col = 4, 
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Weeks', 
     ylab = 'Survival Distribution Function')

# Exercise 6.2

# part a -- estimating survival function
year <- c(1.1,2.6,2.8,3.1,3.4,3.5,3.5,3.6,3.7,3.8,3.8,4.0,4.1,5.6)
cens <- rep(0, length(year))

years.surv <- survfit(Surv(year, cens==0)~ 1, conf.type="none", se.fit=FALSE)
summary(years.surv)

# part b -- plotting survival function
plot(years.surv, mark.time = TRUE, 
     pch = 1, col = 4, 
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Years', 
     ylab = 'Survival Distribution Function')

# Exercise 6.3

# part a of c
years.alive <- c(4.2,5.0,5.3,6.7,8.2,10,10,
                 3.6,6.7,8.2,9.3,10,10,10,10)
censored <- c(0,0,0,0,0,1,1,
              0,0,0,0,1,1,1,1)
sex <- rep(c("M","W"), times = c(7,8))

years.surv <- survfit(Surv(years.alive, censored==0)~sex, conf.type="none", se.fit=FALSE)
summary(years.surv)

plot(years.surv, mark.time = TRUE, 
     pch = 1, col = c('blue','pink'), 
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Years', 
     ylab = 'Survival Distribution Function')

legend('topright', legend = c('Men', 'Women'), 
       text.col = c('blue', 'pink'))

# part b of c
survdiff(Surv(years.alive, censored==0)~ sex)

# Exercise 6.4
# part a

years <- c(1.1,1.6,2.1,2.4,2.7,3.6,4.7,4.8,5.1,
           3.6,4.5,4.6,4.8,5.7,5.8,6.7,7.8,10.5,11.3,12.6)
censored <- c(0,0,0,0,0,0,0,1,1,
              0,0,1,0,0,0,0,0,1,0,1)
status <- rep(c('S','N'), times = c(9,11))

years.surv <- survfit(Surv(years, censored==0)~status, conf.type="none", se.fit=FALSE)
summary(years.surv)

plot(years.surv, mark.time = TRUE, 
     pch = 1, col = c('red','blue'), 
     main = 'Kaplan-Meier Survival Curve', 
     xlab = 'Years', 
     ylab = 'Survival Distribution Function')

legend('topright', legend = c('Smokers', 'Non-Smokers'), 
       text.col = c('red', 'blue'))

# part b
survdiff(Surv(years, censored==0)~status)

# Exercise 6.5
# part a
pres <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter6/DATA for Exercise 6.5.csv")
pres.model<-coxph(Surv(pres$lifespan, pres$assassinated==0) ~ pres$age+pres$yearstart
                 +pres$yearsinoffice)

summary(pres.model)

# part b
pres.model1 <- coxph(Surv(pres$lifespan, pres$assassinated==0) ~ pres$age)
summary(pres.model1)

# Exercise 6.6
# part a
library(fastDummies)
heart <- read.csv("C:/Users/casti/OneDrive/Desktop/School_Stuff/Masters_Stuff/STAT 560/STUDY_MATERIALS_NEW/STUDY_MATERIALS/CSV_Data_Exercises/Chapter6/DATA for Exercise 6.6.csv")

heart.dum <- dummy_cols(heart, select_columns = c('NYHAclass', 'gender'), 
                        remove_first_dummy = T, remove_selected_columns = T) 
heart.dum.model <- coxph(Surv(heart.dum$duration, heart.dum$censored==0) ~ heart.dum$age + heart.dum$diameter +
                             heart.dum$NYHAclass_II + heart.dum$NYHAclass_III + heart.dum$NYHAclass_IV +
                             heart.dum$gender_M)
summary(heart.dum.model)

# part d

heart.model <- coxph(Surv(duration, censored == 0) ~ age + diameter +
                             NYHAclass + gender, data = heart)
summary(heart.model)
