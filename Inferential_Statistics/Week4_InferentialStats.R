library(SDSFoundations)
acl <- AustinCityLimits
View(acl)

#Pre-Lab

acl[acl$Artist == 'Allen Toussaint',c("Year","Age")]
acl[acl$Artist == 'Allen Toussaint',]

prop.table(acl$Gender)
gender <- table(acl$Gender)
gender
expcount <- c(1/2,1/2)
chisq.test(gender,p=expcount)$expected
chisq.test(gender,p=expcount)

gender_hit <- table(acl$Gender,acl$BB.wk.top10)
gender_hit
prop.table(gender_hit,1)
chisq.test(gender_hit,correct = FALSE)$expected
chisq.test(gender_hit,correct=FALSE)

table(acl$Season)

#LAB

table(acl$Genre)
prop.table(table(acl$Genre))
genre <- table(acl$Genre)
exp <- c(1/4,1/4,1/4,1/4)

chisq.test(genre,p=exp)$expected
chisq.test(genre,p=exp)


table(acl$Genre,acl$Twitter.100k)
genre_twi100 <- table(acl$Genre,acl$Twitter.100k)
prop.table(genre_twi100,1)
chisq.test(genre_twi100)$expected
chisq.test(genre_twi100)

#questionset 1-4

acl$Recent[acl$Year < 2012] <- 0
acl$Recent[acl$Year >= 2012] <- 1

gender_recent <- table(acl$Gender,acl$Recent)

chisq.test(gender_recent)$expected
chisq.test(gender_recent,correct = FALSE)

internet_survey <- data.frame(matrix(ncol=2,nrow=123))
x <- c("Area","InternetUser")
colnames(internet_survey) <- x
internet_survey
internet_survey$Area[1:28] <- 'Rural'
internet_survey$Area[29:70] <- 'Sub-urban'
internet_survey$Area[71:123] <- 'Urban'
internet_survey$InternetUser[1:15] <- 0
internet_survey$InternetUser[16:28] <- 1
internet_survey$InternetUser[29:35] <- 0
internet_survey$InternetUser[36:70] <- 1
internet_survey$InternetUser[71:73] <- 0
internet_survey$InternetUser[74:123] <- 1

table(internet_survey)
prop.table(table(internet_survey),1)
table(internet_survey$InternetUser)
prop.table(table(internet_survey$InternetUser))

inter_area <- table(internet_survey$Area,internet_survey$InternetUser)
inter_area
chisq.test(inter_area,correct=FALSE)$expected
chisq.test(inter_area,correct =FALSE)
