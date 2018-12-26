library(SDSFoundations)
post <- PostSurvey
View(post)
#PreLab
post[post$gender=='Male',"classification"]

table(post[1:10,"live_campus"])

#Independent T Test

underclass <- post$happy[post$classification=='Freshman'|post$classification=='Sophomore']
upperclass <- post$happy[post$classification=='Junior'|post$classification=='Senior']

hist(underclass)
hist(upperclass)

t.test(underclass,upperclass)

mean(underclass)
mean(upperclass)
t.test(underclass,upperclass)


post$diff_happy <- post$happy-post$post_happy

mean(post$diff_happy)
hist(post$diff_happy)

t.test(post$happy,post$post_happy,paired = T)

#Lab

hw_college <- post$hw_hours_college
hw_hs <- post$hw_hours_HS

hist(hw_college)
hist(hw_hs)


mean(hw_college)-mean(hw_hs)
mean(hw_college-hw_hs)

t.test(hw_college,hw_hs,paired=T,alternative = 'less')

greek_sleepSat <- post$sleep_Sat[post$greek == 'yes']

nongreek_sleepSat <- post$sleep_Sat[post$greek == 'no']

hist(greek_sleepSat)
hist(nongreek_sleepSat)

mean(greek_sleepSat) - mean(nongreek_sleepSat)

t.test(greek_sleepSat,nongreek_sleepSat,alternative = 'less')


hist(hw_college-hw_hs)

#question set 1-3
post$diff_hours <- post$hw_hours_college-post$hw_hours_HS

nursing <- post$diff_hours[post$major=='Nursing']

biology <- post$diff_hours[post$major=='Biology']

hist(nursing)
hist(biology)

t.test(nursing,biology)

sqrt(5/26 + 36/32)

6/1.44
cpleft <- c(16.3,4.8,10.7,14,15.7,9.9,29.3,20.4,15.7,7.6,16.2,14.7,15,8.4,23.3,17.7)
cpright <-c(11.5,3.5,12.8,7.9,15.2,9.8,24,14.9,12.6,8.2,8.4,11,12.5,9.2,17.5,11.1)

mean(cpleft) - mean(cpright)

sd(cpleft - cpright)/sqrt(15)

t.test(cpleft,cpright,paired=T)
