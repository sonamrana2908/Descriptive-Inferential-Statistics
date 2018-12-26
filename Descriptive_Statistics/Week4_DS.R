acl[1:10]
which(acl$Grammy =='Y')

acl$Genre[which(acl$Gender =='F' & acl$Age > 60)]

#Create a subset of the data for artists age 30 or older
artist30 <- acl[acl$Age > 29,]

#Create a table to show the marginal distribution for each variable

genre <- table(artist30$Genre)
gender <- table(artist30$Gender)
gender
#Create a contingency table to show the conditional distribution for gender and genre.
twoway <- table(artist30$Gender,artist30$Genre)

#Make a bar chart to better visualize how many male and female artists played in each genre
barplot(twoway,legend = T, beside = T)

#Calculate P(A):  the probability of each type of music (genre) being played
prop.table(genre)

#Calculate P(A|B): the probability of each genre being played, given the artist's gender
prop.table(twoway,1)

prop.table(twoway,2)
gender
genre
twoway
7/35
6/35
genre
prop.table(genre)


#Subset the data (males only)

male <- acl[acl$Gender == 'M',]

#Create a table to show the marginal distributions for Genre and Grammy.
Grammy <- table(male$Grammy)
Genremale <- table(male$Genre)
Genremale

#Create a contingency table to show the conditional distribution for Genre and Grammy.
maleGenreGrammy <- table(male$Grammy,male$Genre)

#Make a bar chart to better visualize how many artists in each Genre received a Grammy.
barplot(maleGenreGrammy,legend =T,beside =T)

#Calculate P(A):  the probability of winning a Grammy.
prop.table(Grammy)

#Calculate P(A|B): the probability of winning Grammy, given the artist's Genre.
prop.table(maleGenreGrammy,1)

#Generate a table to show the number of artists that are "popular" and those that are not.
popular <- acl[acl$Facebook.100k == 1,]
PopAge <- table(acl$Facebook.100k,acl$Age.Group)
prop.table(PopAge,2)


33/128
50/128
10/128
1-1/13
24/128
2/23
34/130
6/50
