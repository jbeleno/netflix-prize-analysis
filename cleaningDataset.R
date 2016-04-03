# Pre-processing the movie information to avoid problems with comma

# Defining the file name
fileName <- "./data/movie_titles.txt"
fileNameCleaned <- "./data/movie_titles_clean.txt"

# Loading one line per iteration, changing the value and writting the
# result in a new file, source = http://stackoverflow.com/a/28281908
con  <- file(fileName, open = "r")
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    movie_title <- gsub("^([^,]*,[^,]*),", "", oneLine)
    movie_data_without_title <- sub("^([^,]*,[^,]*,).*", "\\1", oneLine)
    
    movie_data_without_title <- gsub(",", ";", movie_data_without_title)
    
    movie_data <- paste0(movie_data_without_title, movie_title)
    
    write(movie_data, file = fileNameCleaned, append = TRUE)
} 
close(con)