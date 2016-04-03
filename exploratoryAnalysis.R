# Exploratory data analysis

# Load libraries
library(ggplot2)

# Reading movie information
movies <- read.csv2(fileNameCleaned, header = FALSE, col.names = c("id", "year", "title"), 
                    colClasses = c("integer", "character", "character"), comment.char = "",
                    na.strings = "NA")

# Some movies has a NULL year so it's important to convert this data in integer
# omitting the NA values
movies$year <- as.integer(movies$year)

# Histogram of movies per year
qplot(movies$year, geom="histogram", binwidth = 1,  
      main = "Histogram for year of the movie", 
      xlab = "Year",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1890,2009))