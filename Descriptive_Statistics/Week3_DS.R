Bull2013

Bull2013 <- Bull[Bull$Events13>=1,]

plot(Bull2013$Rides13,Bull2013$Top10_13,xlab="No of times Rider was on Bull for full 8 Seconds",ylab='No of times Rider was in Top 10',main='Scatter Plot')


plot(Bull$Rides13,Bull$Top10_13)

abline(lm(Bull2013$Top10_13~Bull2013$Rides13))
cor(x=Bull2013$Rides13,y=Bull2013$Top10_13)

vars <- c("Top10_13", "Rides13")
cor(Bull2013[,vars])

#Subset for riders that participated in at least one event in 2013
Bull2013 <- Bull[Bull$Events13  > 0 ,]

# Visualize and describe the first variable of interest
hist(Bull2013$Rides13)
fivenum(Bull2013$Rides13)
mean(Bull2013$Rides13)
sd(new_bull$Rides13)

# Visualize and describe the second variable of interest
hist(Bull2013$Top10_13)
fivenum(Bull2013$Top10_13)
mean(new_bull$Top10_13)
sd(new_bull$Top10_13)

# Create a scatterplot
plot(new_bull$Rides13,new_bull$Top10_13)

# Add line of best fit
abline(lm(new_bull$Top10_13~new_bull$Rides13))

# Calculate the correlation coefficient
cor(new_bull$Rides13,new_bull$Top10_13)

# Create a correlation matrix 
vars <- c("Top10_13", "Rides13")
cor(new_bull[,vars])

Bull2013[which(Bull2013$Top10_13==2 & Bull2013$Rides13==22),]

plot(Bull$RidePer12,Bull$Earnings12)
plot(Bull$CupPoints12,Bull$Earnings12)

newbull12 <- Bull[Bull$Events12>0,]
hist(newbull12$Earnings12)
fivenum(newbull12$Earnings12)
mean(newbull12$Earnings12)
sd(newbull12$Earnings12)
var2 <- c('Earnings12','RidePer12','CupPoints12')
cor(newbull12[,var2])

which(newbull12$RidePer12>0.6)
newbull12[4,]
max(newbull12$Earnings12)

#Subset the data
nooutlier <- newbull12[newbull12$Earnings12 < 1000000 ,]
plot(nooutlier$RidePer12,nooutlier$Earnings12)
plot(nooutlier$CupPoints12,nooutlier$Earnings12)
cor(nooutlier[,var2])


newbull <- Bull[Bull$Rides14>1,]
RidesPerEvent14 <- newbull$Rides14/newbull$Events14
hist(RidesPerEvent14)
fivenum(RidesPerEvent14)
plot(RidesPerEvent14,newbull$Rank14)
abline(lm(newbull$Rank14~RidesPerEvent14))
cor(newbull$Rank14,RidesPerEvent14)

Minutes Spent Studying	Exam Grade
30	74
45	68
180	87
95	90
130	94
140	84
30	92
80	88
60	82
110	93
0	65
80	90

Minutes <- c(30,45,180,95,130,140,30,80,60,110,0,80)
Grade <- c(74,68,87,90,94,84,92,88,82,93,65,90)
newdata <- data.frame(Minutes,Grade)
cor(newdata$Minutes,newdata$Grade)
0.597 * 0.597
plot(newdata$Minutes,newdata$Grade)
max(newdata$Grade)
newdata[which(newdata$Grade > 90 & newdata$Minutes < 50),]
newdata <- newdata[-c(7),]
0.7