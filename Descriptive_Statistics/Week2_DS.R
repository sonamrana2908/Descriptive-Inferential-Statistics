table(animaldata$Outcome.Type[1:10])
table(animaldata$Outcome.Type == 'Adoption')
animaldata[animaldata$Outcome.Type == 'Adoption',]
table(animaldata$Intake.Type == 'Owner Surrender' & animaldata$Neutered.Status =='Neutered')

adopted <- animaldata[animaldata$Outcome.Type == 'Adoption',]
daystoadopt <- adopted$Days.Shelter

hist(daystoadopt)
max(daystoadopt)
fivenum(daystoadopt)
mean(daystoadopt)
median(daystoadopt)
mode(daystoadopt)
animaldata[which(animaldata$Days.Shelter==max(daystoadopt)),]
sd(daystoadopt)

z <- (max(daystoadopt) - mean(daystoadopt))/sd(daystoadopt)

hist(animaldata$Weight[which(animaldata$Animal.Type == 'Cat' & animaldata$Age.Intake >= 1)])
hist(animaldata$Weight[which(animaldata$Animal.Type == 'Dog' & animaldata$Age.Intake >= 1)])

fivenum(animaldata$Weight[which(animaldata$Animal.Type == 'Cat' & animaldata$Age.Intake >= 1)])
(13-8.6)/1.9
1-pnorm(2.3)

animaldata$Days.Shelter[animaldata$Intake.Type == 'Owner Surrender' & animaldata$Animal.Type == 'Dog' & animaldata$Outcome.Type == 'Return to Owner']
table(animaldata$Intake.Type[animaldata$Animal.Type == 'Dog'])
81/291
(5.38-6.7)/1.1
pnorm(-1.2)

(8.79-6.7)/1.1
pnorm(1.9)-pnorm(-1.2)
