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

# We need to check the amount of NULL data that we have after looking for genres
# in IMDB database
NullMovies <- movies[movies$genre == "NULL",]
NullMovies <- NullMovies[!is.na(NullMovies$id), ] # I don't know why but the last 
                                                  # line included some NA values
nrow(NullMovies) # 5.175
nrow(movies)
percentageNullData <- nrow(NullMovies)*100/nrow(movies) # 29.12%
head(NullMovies$title, 15)

# Movies with IMDB data
NotNullMovies <- subset(movies, genre != "NULL") # This fixes the last problem
                                                 # But the sum of NotNull and Null
                                                 # was different from expected
                                                 # due to some NA values 
                                                 # 12.499 values = 70.33%
# M <- subset(movies, genre == "NULL" | genre != "NULL")
# M <- subset(movies, !(id %in% M$id))

# Histogram of movies per year
h <- qplot(movies$year, 
          geom="histogram", 
          binwidth = 1,  
          main = "Histogram of movies per year", 
          xlab = "Year",  
          fill=I("blue"), 
          col=I("red"), 
          alpha=I(.2),
          xlim=c(1890,2009))
h

# Movies that match with Action genre
actionMovies <- subset(movies, grepl(genres[1], genre))

# OMG I'm so proud about starting using apply family to calculate
# the frecuency of each genre in the movies available in Netflix
NumMoviesPerGenre <- sapply(genres,function(x)sum(grepl(x, movies$genre)))

# Barplot of genre and # of movies
moviesVector <- NumMoviesPerGenre
genresVector <- names(NumMoviesPerGenre)
df <- data.frame(genres = genresVector, movies = moviesVector)

g <- ggplot(data = df, aes(x = genres, y = movies))
g <- g + geom_bar(fill="deepskyblue3", stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
g <- g + ggtitle("# of movies per genre")
g <- g + labs(x="Genres",y="# of Movies")
g <- g + geom_text(
    aes(x = genres, y = movies + 100, label = movies), 
    size = 3, hjust=0.5)
g


# Number of movies directed by each person
NumMoviesPerDirector <- sort(table(movies$director), decreasing=TRUE)
NumMoviesPerDirector <- NumMoviesPerDirector[names(NumMoviesPerDirector) != "NULL"]

# Plot of the n directors with most movies
n <- 10

nMoviesVector <- head(NumMoviesPerDirector, n)
directorsVector <- names(head(NumMoviesPerDirector, n))
df <- data.frame(directors = directorsVector, movies = nMoviesVector)

g <- ggplot(data = df, aes(x = directors, y = movies))
g <- g + geom_bar(fill="deepskyblue3", stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
g <- g + ggtitle("Directors with most movies in the dataset")
g <- g + labs(x="Directors",y="# of Movies")
g <- g + geom_text(
            aes(x = directors, y = movies + 1, label = movies), 
            size = 3, hjust=0.5)
g

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

# Plot of the n actors with most movies
n <- 10

nMoviesVector <- head(NumMoviesPerActor, n)
actorsVector <- names(head(NumMoviesPerActor, n))
df <- data.frame(actors = actorsVector, movies = nMoviesVector)

g <- ggplot(data = df, aes(x = actors, y = movies))
g <- g + geom_bar(fill="deepskyblue3", stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
g <- g + ggtitle("Actors with most movies in the dataset")
g <- g + labs(x="Actors",y="# of Movies")
g <- g + geom_text(
    aes(x = actors, y = movies + 1, label = movies), 
    size = 3, hjust=0.5)
g



# Movies rating files
file.names<-paste0('data/training_set/mv_',sprintf('%07d',NotNullMovies$id),'.txt')

# Number of lines per file, the minus 1 is because the first line is for
# the movie id
nRowsMoviesFiles <- sapply( file.names, function(f){length(count.fields(f)) - 1} )

# Re-arrange the number of rows data to know the files with more and less lines
nRowsMoviesFilesOrdered <- sort(nRowsMoviesFiles, decreasing=TRUE)
head(nRowsMoviesFilesOrdered)
tail(nRowsMoviesFilesOrdered)

# Sum it up everything to know the real amount of data we are handling with
sum(nRowsMoviesFilesOrdered) # 89.666.002 lines

# A function to sum it up all the ratings values for each file
countMovieRatings <- function(f){
    content <- read.csv(f, skip = 1, header = FALSE, nrows = 232944,
                        colClasses = c("integer", "integer", "character"),
                        col.names = c("id", "rating", "date"))
    
    sum(content$rating)
}

sumMovieRatings <- sapply( file.names, function(f){ countMovieRatings(f) })

# Average rating per movie
avgRatingMovie <- sumMovieRatings/nRowsMoviesFiles

# Ordered average rating per movie
ratingMovieDf <- data.frame(movie = NotNullMovies$title, 
                            rating = avgRatingMovie, 
                            votes = nRowsMoviesFiles)
ratingMovieDf <- ratingMovieDf[order(ratingMovieDf$rating, decreasing=TRUE),]

# Best movies graphic by rating
g <- ggplot(data = head(ratingMovieDf, 10), aes(x = movie, y = rating))
g <- g + geom_bar(fill="deepskyblue3", stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
g <- g + ggtitle("Best movies according to the rating given by Netflix users")
g <- g + labs(x="Movie Title",y="Average Rating")
g <- g + geom_text(
    aes(x = movie, y = rating + 0.15, label = format(rating, digits = 3)), 
    size = 3, hjust=0.5)
g


# Worst movies graphic by rating
g <- ggplot(data = tail(ratingMovieDf, 10), aes(x = movie, y = rating))
g <- g + geom_bar(fill="deepskyblue3", stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
g <- g + ggtitle("Worst movies according to the rating given by Netflix users")
g <- g + labs(x="Movie Title",y="Average Rating")
g <- g + geom_text(
    aes(x = movie, y = rating + 0.05, label = format(rating, digits = 3)), 
    size = 3, hjust=0.5)
g

# This code is for saving data in a different way to easy exchange with 
# relational databases
moviesDataFile <- data.frame(id = movies$id, 
                             year = movies$year,
                             title = movies$title)
write.table(moviesDataFile, file = "./data/movie_titles_reformated.txt",
            sep = ";", row.names = F)


# Separating genres in a different file for relational databases compatibility
appendGenresInMovies <- function(x, output) {
    idMovie <- x["id"]
    genresString <- x["genre"] # I don't know why this doesn't accept $ notation
    
    for(genre in genres){
        #print(genresString)
        if(grepl(genre, genresString)){
            write(paste0(idMovie, ";",  genre), file = output, append = TRUE)
        }
    }
}

apply(movies, 1, appendGenresInMovies, output = './data/genres.txt')


# Moving the rating data to a local MySQL database
library(RMySQL)

# Defining the files to be used
total.file.names<-paste0('data/training_set/mv_',sprintf('%07d',movies$id),'.txt')
total.file.names.df <- data.frame(id_movie = movies$id, file_name = total.file.names)

addingRatingsToSQL <- function(f){
    # Create connection to the database
    netflixDb <- dbConnect(MySQL(), user="root",
                            host="127.0.0.1", db="netflix")
    # Reading data in the file
    content <- read.csv(f["file_name"], skip = 1, header = FALSE, nrows = 232944,
                        colClasses = c("integer", "integer", "character"),
                        col.names = c("id_user", "rating", "date"))
    
    for(x in seq(1:nrow(content))){
        # Creating a insert query with this
        # query <- paste("INSERT INTO ratings VALUES(", paste(content[1,], collapse = ", "), ")")
        query <- paste("INSERT INTO ratings VALUES(",content[x, "id_user"],
                       ",", f["id_movie"], ",", content[x, "rating"], 
                       ", STR_TO_DATE(", paste0("'", trimws(content[x, "date"]), "'"), 
                       ", '%Y-%m-%d'))")
        
        dbGetQuery(netflixDb, query)
    }
    
    print(query)
    
    # Disconnect the database
    dbDisconnect(netflixDb)
}

# Migrating ratings to a local MySQL database
apply( total.file.names.df, 1, function(f){addingRatingsToSQL(f)} )

# Create a new bunch of files with movies, rating per user

# Boxplot-Histogram of rating data in movies


# Worst and best genres

# Getting the results of how many people where active at rating movies
# (This is better to do with a relational database)
