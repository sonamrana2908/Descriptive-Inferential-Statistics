library(SDSFoundations)
film <- FilmData
View(film)

#Pre-Lab
film[film$Film == 'Titanic',]
film[film$Studio=='Uni.',]

table(film$Rating)
aggregate(Budget~Rating,film,mean)
aggregate(Budget~Rating,film,sd)


boxplot(film$Budget~film$Rating,main='Budget vs Rating',xlab='Rating',ylab='Budget')

budrating <- aov(film$Budget~film$Rating)
summary(budrating)

TukeyHSD(budrating)


aggregate(IMDB~Rating,film,mean)
aggregate(IMDB~Rating,film,sd)


boxplot(film$IMDB~film$Rating,main='IMDB vs Rating',xlab='Rating',ylab='IMDB')

IMDBrating <- aov(film$IMDB~film$Rating)
summary(IMDBrating)

TukeyHSD(IMDBrating)

#Lab

table(film$Studio)

aggregate(Days~Studio,film,mean)
aggregate(Days~Studio,film,sd)

boxplot(film$Days~film$Studio,main='No of Days vs Studio',xlab='Studio',ylab='Days')

daystudio <- aov(film$Days~film$Studio)
summary(daystudio)

TukeyHSD(daystudio)

aggregate(Pct.Dom~Studio,film,mean)
aggregate(Pct.Dom~Studio,film,sd)

boxplot(film$Pct.Dom~film$Studio,main='Pct.Dom vs Studio',xlab='Studio',ylab='Pct.Dom')

perdomstudio <- aov(film$Pct.Dom~film$Studio)
summary(perdomstudio)

TukeyHSD(perdomstudio)

#Questionset1-4
film[film$Budget < 100,]
film$Budget_Cat <- 0
film$Budget_Cat[film$Budget < 100] <- 'low-budget'
film$Budget_Cat[film$Budget >= 100 & film$Budget < 150] <- 'medium-budget'
film$Budget_Cat[film$Budget >= 150] <- 'high-budget'

filmdup <- filmdup[-c(98),]
View(filmdup)

table(filmdup$Budget_Cat)

aggregate(Pct.Dom~Budget_Cat,filmdup,mean)
aggregate(Pct.Dom~Budget_Cat,filmdup,sd)

boxplot(filmdup$Pct.Dom~filmdup$Budget_Cat,main='Pct.Dom vs Budget',xlab='Budget',ylab='Pct.Dom')

pctdombudget <- aov(filmdup$Pct.Dom~filmdup$Budget_Cat)
summary(pctdombudget)

TukeyHSD(pctdombudget)
