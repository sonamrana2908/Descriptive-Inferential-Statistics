View(BikeData)
BikeData[7,'age']
BikeData[1:10,]
table(BikeData$cyc_freq == 'Daily')
BikeData[(BikeData$cyc_freq == 'Daily'),]
BikeData$speed[BikeData$cyc_freq == 'Less than once a month' & BikeData$gender == 'F'][1]
student <- BikeData[BikeData$student == 1,]
mean(student$distance)
table(student$cyc_freq)
distance <-student$distance
table(BikeData$cyc_freq=='Daily')
DailyRiders <- BikeData[BikeData$cyc_freq=='Daily',]
table(DailyRiders$gender)
mean(DailyRiders$age)
Female <- DailyRiders[DailyRiders$gender == 'F',]
Male <- DailyRiders[DailyRiders$gender == 'M',]
mean(Male$age)
table(Male$age >= 30)
