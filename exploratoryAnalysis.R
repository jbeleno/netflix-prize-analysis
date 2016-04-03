# Exploratory data analysis

# Load libraries
library(ggplot2)

# Defining the file name
fileNameCleaned <- "./data/movie_titles_clean.txt"
fileNameActors <- "./data/movie_with_actors.txt"

# Reading movie information
movies <- read.csv2(fileNameCleaned, 
                    header = FALSE, col.names = c("id", "year", "title", "genre", "language", "country"), 
                    colClasses = c("integer", "character", "character", "character", "character", "character"), 
                    comment.char = "", na.strings = "NA")

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

# Reading movie data with actors
actors <- read.csv2(fileNameActors, 
                    header = FALSE, col.names = c("id", "year", "title", "genre", "language", "country", "actor"), 
                    colClasses = c("integer", "character", "character", "character", "character", "character", "character"), 
                    comment.char = "", na.strings = "NA")