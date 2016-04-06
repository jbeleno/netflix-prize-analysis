# Exploratory data analysis

# Load libraries
library(ggplot2)

# Defining movies genres
genres <- c("Action", "Adventure", "Animation", "Biography",
            "Comedy", "Crime", "Documentary", "Drama", "Family",
            "Fantasy", "Film-Noir", "History", "Horror", "Music",
            "Musical", "Mystery", "Romance", "Sci-Fi", "Sport",
            "Thriller", "War", "Western")

# Defining the file name
fileNameCleaned <- "./data/movie_titles_clean.txt"
fileNameActors <- "./data/movie_with_actors.txt"

# Reading movie information
movies <- read.csv2(fileNameCleaned, 
                    header = FALSE, col.names = c("id", "year", "title", "director",
                                                  "genre", "language", "country"), 
                    colClasses = c("integer", "character", "character", "factor", 
                                   "character", "character", "character"), 
                    comment.char = "", na.strings = "N/A")

# Some movies has a NULL year so it's important to convert this data in integer
# omitting the NA values
movies$year <- as.integer(movies$year)

# Histogram of movies per year
qplot(movies$year, 
      geom="histogram", 
      binwidth = 1,  
      main = "Histogram of movies per year", 
      xlab = "Year",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1890,2009))

# Movies that match with Action genre
actionMovies <- subset(movies, grepl(genres[1], genre))

# OMG I'm so proud about starting using apply family to calculate
# the frecuency of each genre in the movies available in Netflix
NumMoviesPerGenre <- sapply(genres,function(x)sum(grepl(x, movies$genre)))

# Numbre of movies directed by each person
NumMoviesPerDirector <- sort(table(movies$director), decreasing=TRUE)

# Reading movie data with actors
moviesCompleteByActor <- read.csv2(fileNameActors, 
                    header = FALSE, col.names = c("id", "year", "title", "director",
                                                  "genre", "language", "country", "actor"), 
                    colClasses = c("integer", "character", "character", "factor",
                                   "character", "character", "character", "factor"), 
                    comment.char = "", na.strings = "N/A")

# Check the kind of data in the dataset
head(moviesCompleteByActor)

# Check quick information about the dataset, especially what are the people who
# directed/acted most of the movies in the dataset
summary(moviesCompleteByActor)

# Check the number of levels in factor columns
str(moviesCompleteByActor)

# Number of movies per actor
NumMoviesPerActor <- sort(table(moviesCompleteByActor$actor), decreasing=TRUE)