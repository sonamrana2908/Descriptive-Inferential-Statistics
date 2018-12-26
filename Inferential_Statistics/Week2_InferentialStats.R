#PreLab
library(SDSFoundations)
bull <- BullRiders
bull

USAbull <- bull[bull$Country == 'USA',]
mean(USAbull$Weight)
sd(USAbull$Weight)

hist(USAbull$Weight,main='Histogram of USA Null Riders')


t.test(USAbull$Weight,mu=190)

sqrt(37)*(153.11-190)/13.02

#Lab
View(bull)
Bull14 <-bull[bull$Events14>=5,]

mean(Bull14$RidePer14)
sd(Bull14$RidePer14)
hist(Bull14$RidePer14,main='Histogram of Riders having atleast 5 events in 2014')


t.test(Bull14$RidePer14,mu=0.5)

#Question Set1-4

earnings_per <- bull$Earnings12/bull$Events12
hist(earnings_per,main='Histogram of Avg Earnings per event in 2012')
mean(earnings_per)

bull$LogAvgEarningsPerEvent12 <-log(earnings_per)
hist(bull$LogAvgEarningsPerEvent12)


abc <- log(earnings_per)
abc <- na.omit(abc)
abc[is.na(abc)] <- 0
abc
mean(abc)
sd(abc)
hist(abc)

bull$LogAvgEarningsPerEvent12[is.na(bull$LogAvgEarningsPerEvent12)] <- 0
mean(bull$LogAvgEarningsPerEvent12)
bull$LogAvgEarningsPerEvent12

t.test(abc,mu=8.85)

(2.71828)^8.572169
(2.71828)^9.120605

weight <- c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)
mean(weight)
sd(weight)

t.test(weight,mu=28.5)

42.6 - 1.796*5.3/sqrt(12)
