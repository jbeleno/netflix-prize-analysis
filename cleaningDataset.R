# Pre-processing the movie information to avoid problems with comma
# And adding some extra data

# Load library
library(httr)

# Defining the file name
fileName <- "./data/movie_titles.txt"
fileNameCleaned <- "./data/movie_titles_clean.txt"
fileNameActors <- "./data/movie_with_actors.txt"

# Loading one line per iteration, changing the value and writting the
# result in a new file, source = http://stackoverflow.com/a/28281908
con  <- file(fileName, open = "r")
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    
    # Wait 100ms
    Sys.sleep(0.1)
    
    # Using gsub to separate the title from other data per movie
    movie_title <- gsub("^([^,]*,[^,]*),", "", oneLine)
    movie_data_without_title <- sub("^([^,]*,[^,]*,).*", "\\1", oneLine)
    
    # Replacing comma for semicolon as separator
    movie_data_without_title <- gsub(",", ";", movie_data_without_title)
    
    
    
    # Adding data about genre, language and country in the movie
    
    # Request execution
    requestMovieData <- GET(
        "http://www.omdbapi.com/?", 
        query = list(t = movie_title)
    )
    
    # Getting the content in json format
    movieDataWeb <- content(requestMovieData, "parsed", encoding = "UTF-8")
    
    if(requestMovieData$status_code == 200){
        if(movieDataWeb$Response == "True"){
        
            # Join all the data using semicolon
            movie_data <- paste0(movie_data_without_title, 
                                 movie_title, ";",
                                 movieDataWeb$Director, ";",
                                 movieDataWeb$Genre, ";",
                                 movieDataWeb$Language, ";",
                                 movieDataWeb$Country)
            
            # Writting the final data in a file
            write(movie_data, file = fileNameCleaned, append = TRUE)
            
            
            # Getting actors data and parsing it
            if(movieDataWeb$Actors != "N/A"){
                actors <- strsplit(movieDataWeb$Actors, ",")[[1]]
                
                # For loop with actors
                if(length(actors) > 0){
                    for(i in 1:length(actors)){
                        movie_data2 <- paste0(movie_data, ";", trimws(actors[i]))
                        
                        # Writting the final data in a different file
                        write(movie_data2, file = fileNameActors, append = TRUE)
                    }
                }
            }
        
        }else{
            # Join all the data using semicolon
            movie_data <- paste0(movie_data_without_title, 
                                 movie_title, ";", 
                                 "NULL;",
                                 "NULL;",
                                 "NULL;",
                                 "NULL")
            
            # Writting the final data in a file
            write(movie_data, file = fileNameCleaned, append = TRUE)
        }
    }
    
} 
close(con)