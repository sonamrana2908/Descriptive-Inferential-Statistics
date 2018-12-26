world[world$IncomeGroup =="Low income","Country"]

world[world$Country =="Aruba" & world$year == "1970","rural.population"]

world[world$Country =="Australia" & world$mobile.users > 0,"year"]

usdata <- world[world$Country =="United States" ,]
usdata90 <- usdata[usdata$year >= 1990 & usdata$year <= 1999,]

usdata90

plot(usdata90$year,usdata90$internet.users)
usdata90int <- usdata90$internet.users/1000000
usdata90year <- usdata90$year - 1990
expFit(usdata90year,usdata90int)
expFitPred(usdata90year,usdata90int,16)

logisticFit(usdata90year,usdata90int)
logisticFitPred(usdata90year,usdata90int,16)

tripleFit(usdata90year,usdata90int)

3756.413 - usdata[usdata$year == '2006',"internet.users"]/1000000


127.477 - usdata[usdata$year == '2006',"internet.users"]/1000000
---
  lab
---
  world$Country == 'Denmark'
denmark <- world[world$Country == 'Denmark',]

denmark$internet.users/denmark$population
den.pop.per <- denmark$internet.users/denmark$population

denmark.1990 <- denmark[denmark$year >= 1990,]
den.pop.per.1990 <- denmark.1990$internet.users/denmark.1990$population

yearssince1990 <- denmark.1990$year - 1990
expFit(yearssince1990,den.pop.per.1990)

logisticFit(yearssince1990,den.pop.per.1990)

0.7 = 0.0056 * (1.3467)

log(125)/log(1.3467)

exp model = 16.22 year means 2006

1 + 308.8322 * (1.7312)t = 0.8967/0.7
(0.8967/0.7 - 1)/308.8322
log(0.00091)/log(1.7312)
=12.76 for logistic growth

denmark.1990
den.pop.per.1990
---
Question Set1

brazil95 <- world[(world$Country == 'Brazil') & (world$year >= 1995),]

Brazilyears1995 <- brazil95$year - 1995
Brazilmobileusers <- brazil95$mobile.users/1000000


linFit(Brazilyears1995,Brazilmobileusers)
expFit(Brazilyears1995,Brazilmobileusers)
logisticFit(Brazilyears1995,Brazilmobileusers)
tripleFit(Brazilyears1995,Brazilmobileusers)
plot(Brazilyears1995,Brazilmobileusers)

logisticFitPred(Brazilyears1995,Brazilmobileusers,30)

brazil95[brazil95$year == 2000,"mobile.users"]
brazil95[brazil95$mobile.users >= 100000000,"year"]

brazil <- world[(world$Country == 'Brazil'), c("year", "mobile.users")]
tripleFit(brazil$year - 1960,brazil$mobile.users/1000000)



expFit(brazil$year - 1960,brazil$mobile.users/1000000)
logisticFit(brazil$year - 1960,brazil$mobile.users/1000000)


Question Set2

367/257
day <- c(0,1,2,3,4,5,6,7,8,9)
flu <- c(73,105,137,257,367,658,898,1085,1490,1893)
expFit(day,flu)
expFitPred(day,flu,14)
1.46*1.46*1.46*1.46*1.46*1.46*1.46*1.46*1.46*1.46*1.46*1.46*1.46*1.46
76.64*199.9586

logisticFit(day,flu)
logisticFitPred(day,flu,14)
tripleFit(day,flu)

3273.31/1.079
1.57^-14
0.00181*43.59

15325-4379
4379-3034

Question set 3

wolfyear <- c(1,3)
wolfnum <- c(25,45)
linFit(wolfyear,wolfnum)
expFit(wolfyear,wolfnum)
linFitPred(wolfyear,wolfnum,7)
expFitPred(wolfyear,wolfnum,7)
logisticFitPred(wolfyear,wolfnum,7)

325/18.63
log(17.445)/log(1.342)

Question set 4
log(152.1)/log(2.17)
2.17^-9
0.00094*152.1 +1
2000/1.143
77 + 1.96*13/6


yogurt <- c(180,200,190,230,80,160,170,130,140,220,110,120,100,170)
mean(yogurt)
1.96*48.5/sqrt(14)
