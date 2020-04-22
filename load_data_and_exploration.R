library(data.table)
library(car)
install.packages('tm')
library(tm)
install.packages("Matrix")
library(Matrix)
install.packages('slam')
library(slam)
install.packages("babynames")
library(babynames) # data package
install.packages("dplyr")
library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("lubridate")
library(lubridate)
library(Amelia)
library(stringr)
install.packages("plotly")
library(plotly)
install.packages("recommenderlab")
library(recommenderlab)

# Row names and column names to dataframes
# rownames(usersdf) <- c(1:nrow(usersdf))
# rownames(moviesdf) <- c(1:nrow(moviesdf))
# rownames(ratingsdf) <- c(1:nrow(ratingsdf))
# colnames(usersdf) <- c("UserID","Gender","Age","Occupation","Zip-code")
# colnames(moviesdf) <- c("MovieID","Title","Genres")
# colnames(ratingsdf) <- c("UserID","MovieID","Rating","Timestamp")

# read in movies files
movies_raw = readLines('movies.dat')
movies_raw = gsub(pattern = "::", replacement = ";", movies_raw)
writeLines(movies_raw, "movies.csv")
closeAllConnections()
movies = fread('movies.csv', sep = ";", header = F)
rm(movies_raw)
colnames(movies) = c('MovieID', 'Title', 'Genres')

# read in ratings file
ratings_raw = readLines('ratings.dat')
ratings_raw = gsub("::", ";", ratings_raw)
writeLines(ratings_raw, 'ratings.csv')
closeAllConnections()
ratings = fread('ratings.csv', sep = ";", header = F)
rm(ratings_raw)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# read in tags data
users_raw = readLines('users.dat')
users_raw = gsub("::", ";", users_raw)
writeLines(users_raw, 'users.csv')
users = read.csv('users.csv', sep = ";", header = F)
rm(users_raw)
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip')


# colnames(movies) <- c("MovieID", "Title", "Genres")
# colnames(ratings) <- c("UserID", "MovieID", "Rating", "Timestamp")
# colnames(users) <- c("UserID", "Gender", "Age", "Occupation", "Zip-code")


View(movies)
View(users)
View(ratings)


str(ratings)
ratings[, DateTime := as.POSIXct(Timestamp, origin = '1970-01-01')]
ratings[, Date := as.Date(DateTime)]
ratings[, Timestamp := NULL]

sum(is.na(ratings))



str(movies)
movies[, Year := str_sub(Title,-5,-2)]
movies[, Year := as.integer(Year)]
head(movies)

movies$Title <- gsub("\\(\\d+\\)", "", movies$Title)
#movies$Title <- str_trim(as.character(movies$Title), side = "both")
head(movies)

str(users)
occupation.encode <- function(x){
  switch(as.character(x), "0"="other or notspecified"
         ,"1"="academic/educator"
         ,"2"="artist"
         ,"3"="clerical/admin"
         ,"4"="college/gradstudent"
         ,"5"="customerservice"
         ,"6"="doctor/healthcare"
         ,"7"="executive/managerial"
         ,"8"="farmer"
         ,"9"="homemaker"
         ,"10"="K-12student"
         ,"11"="lawyer"
         ,"12"="programmer"
         ,"13"="retired"
         ,"14"="sales/marketing"
         ,"15"="scientist"
         ,"16"="self-employed"
         ,"17"="technician/engineer"
         ,"18"="tradesman/craftsman"
         ,"19"="unemployed"
         ,"20"="writer")
}

Age.encoding <- function(x){
  switch(as.character(x), "1"="Under 18"
         ,"18"="18-24"
         ,"25"="25-34"
         ,"35"="35-44"
         ,"45"="45-49"
         ,"50"="50-55"
         ,"56"="56+")
}

users$Occupation <-  sapply(users$Occupation, occupation.encode)
users$Age.Grouped <- sapply(users$Age, Age.encoding)

head(users)
any(is.na(users))

#Data Exploration

#Best movies of all time based on ratings.

movies.ratings <- inner_join(ratings, movies, by = 'MovieID')
movies.ratings <- rename(movies.ratings, ratings.date = Date)
str(movies.ratings)
head(movies.ratings)

best.ratings <- movies.ratings %>% select(Rating, Title, UserID) %>% group_by(Title) %>% summarise(avg_rating = mean(Rating), number_of_ratings = n()) %>% arrange(desc(avg_rating))  
head(best.ratings)
ggplot(best.ratings[1:10,], aes(x = Title, y = avg_rating)) +
  geom_col(fill = "blue") +
  #coord_flip() +
  ylab('Movie Title') + xlab('Average Rating') + ggtitle('Best movies by ratings')+ 
  theme_bw()  

### top ten movies
best.ratings.filtered <- best.ratings %>% filter(number_of_ratings > 100) %>% arrange(desc(avg_rating))

best.ratings.filtered$Title<- factor(best.ratings.filtered$Title, levels = rev(unique(as.character(best.ratings.filtered$Title))))

ggplot(best.ratings.filtered[1:10,], aes(x = Title, y = avg_rating)) + geom_col(aes(fill = number_of_ratings)) + ggtitle("Top 10 movies by ratings") +
  #coord_flip() +
  ylab('Average Rating') + theme_bw()

################## movies were produced per year

movies.per.year <- movies %>% group_by(Year) %>% summarise(count = n()) %>% arrange(desc(Year))

head(movies.per.year)

movies.perYear.plot <- ggplot(movies.per.year, aes(x = Year, y = count)) + geom_line(col = 'blue', linetype = 'solid', size = 1) + theme_bw() + ggtitle('Number of Movies by Year') + ylab('Number of Movies')

print(movies.perYear.plot)

##################### most popular movie genres year by year

genresByYear <- movies %>% separate_rows(Genres, sep = "\\|") %>% select(MovieID, Year, Genres) %>% group_by(Year, Genres) %>% summarise(count = n()) %>% arrange(desc(Year))

head(genresByYear)

ggplot(genresByYear, aes(x = Year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge') + theme_bw() + ylab('Number of Movies') + ggtitle('Popularity per year by Genre')

year.discretize <- function(x){
  if(x >= 1920 && x <= 1940){
    return('1920-1940')
  }
  else if(x >= 1941 && x <= 1960){
    return('1941-1960')
  }
  else if(x >= 1961 && x <= 1980){
    return('1961-1980')
  }
  else{
    return('1981-2000')
  }
}

genresByYear$grouped.year <- sapply(genresByYear$Year, year.discretize)

ggplot(genresByYear, aes(x = grouped.year, y = count)) + geom_col(aes(fill = Genres), 
position = 'dodge') + ylab('Number of Movies') + xlab('Year') + ggtitle('Popularity per year by Genre') + theme_bw()

genresByYear %>% filter(Genres %in% c('Action', 'Drama', 'Comedy', 'Romance')) %>% ggplot(aes(x = grouped.year, y = count)) + geom_col(aes(fill = Genres), position = 'dodge') + ylab('Number of Movies') + xlab('Year') + ggtitle('Popularity per year by Genre') + theme_bw()

genresByYear %>% filter(Genres %in% c('Action', 'Drama', 'Comedy', 'Romance')) %>% ggplot(aes(x = Year, y = count)) + geom_line(aes(col = Genres), size = 1) + ylab('Number of Movies') + xlab('Year') + ggtitle('Popularity per year by Genre') + theme_bw()

#Distribution of users age

ggplot(users, aes(x = Age.Grouped)) + geom_bar(aes(fill = ..count..)) + 
  ggtitle('Distribution of users ages') + theme_bw()

#Distribution of users occupations

occupation.sorted <- as.data.frame(sort(table(users$Occupation), decreasing = TRUE))
colnames(occupation.sorted) <- c('Occupation', 'Frequency')
occupation.sorted$Occupation <- factor(occupation.sorted$Occupation, levels = rev(unique(as.character(occupation.sorted$Occupation))))


ggplot(occupation.sorted, aes(x = Occupation, y = Frequency)) + geom_col(aes(fill = Frequency)) + ggtitle('Distribution of Users Occupations') + coord_flip() + theme_bw()
