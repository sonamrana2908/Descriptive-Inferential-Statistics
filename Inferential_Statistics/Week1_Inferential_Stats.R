#PreLab

library(SDSFoundations)
survey <- StudentSurvey
survey
survey[1:10,"name_letters"]
survey[survey$happy < 40,"name_letters"]

hist(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)
fivenum(survey$name_letters)

samplestats5 <- rep(NA,1000)
for (i in 1:1000){
  mysample <- sample(survey$name_letters,size=5)
  samplestats5[i] <- mean(mysample)
}
hist(samplestats5)
mean(samplestats5)
sd(samplestats5)


samplestats15 <- rep(NA,1000)
for (i in 1:1000){
  mysample <- sample(survey$name_letters,size=15)
  samplestats15[i] <- mean(mysample)
}
hist(samplestats15)
mean(samplestats15)
sd(samplestats15)


samplestats25 <- rep(NA,1000)
for (i in 1:1000){
  mysample <- sample(survey$name_letters,size=25)
  samplestats25[i] <- mean(mysample)
}
hist(samplestats25)
mean(samplestats25)
sd(samplestats25)

sd(survey$name_letters)/sqrt(25)

#Lab

hist(survey$happy)
mean(survey$happy)
sd(survey$happy)

happy5 <- rep(NA,1000)
for (i in 1:1000){
  mysample <- sample(survey$happy,size=5)
  happy5[i] <- mean(mysample)
}
hist(happy5)
mean(happy5)
sd(happy5)

sd(survey$happy)/sqrt(5)
sd(survey$happy)/sqrt(15)
sd(survey$happy)/sqrt(25)

#QuestionSet1-4
hist(survey$austin)
mean(survey$austin)
sd(survey$austin)

sd(survey$austin)/sqrt(10)

austin10 <- rep(NA,1000)
for (i in 1:1000){
  mysample <- sample(survey$austin,size=10)
  austin10[i] <- mean(mysample)
}
hist(austin10)
mean(austin10)
sd(austin10)


11/sqrt(23)
sqrt(23)*7.1/11
1.5/sqrt(15)
1.96*1.5/sqrt(15)

471.46+1.96*1.5/sqrt(15)
471.46 - 1.96*1.5/sqrt(15)


