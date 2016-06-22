rm(list=ls())

# You will need to install the following packages (and their dependencies)
library(Rcpp)
library(ggmap)
library(RJSONIO)
distQueryCheck()
####
## 
# Part 1: Simple Function to Return a Distance (from ggmaps package)
##
####
mapdist(from="42.168619, -72.34139",to="42.07061, -72.62029")

####
##
# Part 2: To apply this to a full dataset you could do the following
##
####

# Simple Dataset with a "from" column and a "to" column, with coordinates for each. 
geocodeXX = as.data.frame(rbind(
c("42.168619, -72.34139" ,"42.07061, -72.62029"),
c("42.168619, -72.34139","42.07061, -72.62029"),
c("42.266037, -71.795684", "42.07061, -72.62029"),
c("41.629309, -73.226015", "42.07061, -72.62029")))
names(geocodeXX) = c("from","to")

# Run the Distances
distances = do.call(rbind,apply(geocodeXX[,c("from","to")],1,function(x) mapdist(from=x[1],to=x[2])))
# > distances
# from                  to      m      km    miles seconds  minutes     hours
# 1  42.168619, -72.34139 42.07061, -72.62029  38169  38.169 23.71822    1832 30.53333 0.5088889
# 2  42.168619, -72.34139 42.07061, -72.62029  38169  38.169 23.71822    1832 30.53333 0.5088889
# 3 42.266037, -71.795684 42.07061, -72.62029  91749  91.749 57.01283    3491 58.18333 0.9697222
# 4 41.629309, -73.226015 42.07061, -72.62029 117094 117.094 72.76221    4883 81.38333 1.3563889

####
##
# Part 3: Get Around the 2500 queries / day limit. NOTE: This requires an API key and a 
# billing account!  You may be charged $0.50 / 1000 queries, and there is a limit of 100,000 
# queries per day!
##
####

####
##
# Function To Get Distance
getdistkey = function (from, to, mode = c("driving", "walking", "bicycling"), 
                       output = c("simple", "all"), messaging = FALSE, sensor = FALSE, 
                       language = "en-EN", override_limit = FALSE,api_key=api_key) 
{
  message("by using this function you are agreeing to the terms at :")
  message("http://code.google.com/apis/maps/documentation/distancematrix/\n")
  if (is.numeric(from) && length(from) == 2) 
    from <- revgeocode(from)
  stopifnot(is.character(from))
  if (is.numeric(to) && length(to) == 2) 
    to <- revgeocode(to)
  stopifnot(is.character(to))
  from_to_df <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  origins <- from_to_df$from
  destinations <- from_to_df$to
  mode <- match.arg(mode)
  output <- match.arg(output)
  stopifnot(is.logical(messaging))
  stopifnot(is.logical(sensor))
  getdists <- function(df) {
    origin <- df$from[1]
    origin <- gsub(",", "", origin)
    origin <- gsub(" ", "+", origin)
    origin <- paste("origins=", origin, sep = "")
    destinations <- df$to
    destinations <- gsub(",", "", destinations)
    destinations <- gsub(" ", "+", destinations)
    destinations <- paste("destinations=", paste(destinations, 
                                                 collapse = "|"), sep = "")
    mode4url <- paste("mode=", mode, sep = "")
    lang4url <- paste("language=", language, sep = "")
    sensor4url <- paste("sensor=", tolower(as.character(sensor)), 
                        sep = "")
    
    key <- paste("key=",api_key,sep="")
    posturl <- paste(origin, destinations, mode4url, sensor4url, key,
                     sep = "&")
    url_string <- paste("https://maps.googleapis.com/maps/api/distancematrix/json?", 
                        posturl, sep = "")
    url_string <- URLencode(url_string)
    if (nchar(url_string) >= 2048) {
      n <- nrow(df)
      half_df <- floor(n/2)
      return(rbind(getdists(df[half_df, ]), getdists(df[(half_df + 
                                                           1):n, ])))
    }
    #check_dist_query_limit(url_string, elems = nrow(df), 
    #              override = override_limit, messaging = messaging)
    if (messaging) 
      message("trying url ", url_string)
    connect <- url(url_string)
    tree <- fromJSON(paste(readLines(connect), collapse = ""))
    close(connect)
    message(paste0("Information from URL : ", url_string))
    if (length(df$to) != length(tree$destination_addresses)) {
      message("matching was not perfect, returning what was found.")
      names(tree$rows[[c(1, 1)]]) <- tree$destination_addresses
      output <<- "all"
    }
    else {
      names(tree$rows[[c(1, 1)]]) <- df$to
    }
    tree$rows[[c(1, 1)]]
  }
  out <- dlply(from_to_df, "from", getdists)
  if (output == "all") 
    return(out)
  out <- ldply(out, function(oneFromList) {
    ldply(oneFromList, function(oneToList) {
      data.frame(m = oneToList$distance$value, km = oneToList$distance$value/1000, 
                 miles = 0.0006214 * oneToList$distance$value, 
                 seconds = oneToList$duration$value, minutes = oneToList$duration$value/60, 
                 hours = oneToList$duration$value/3600)
    })
  })
  names(out) <- c("from", "to", names(out)[3:ncol(out)])
  suppressMessages(join(from_to_df, out))
}


api_key = "<paste your key here>"

# For a Single Distance
getdistkey(from="42.168619, -72.34139",to="42.07061, -72.62029",api_key=api_key)

# For a Matrix / Dataset of Distances
distances2 = do.call(rbind,apply(geocodeXX[,c("from","to")],1,function(x) getdistkey(from=x[1],to=x[2],api_key=api_key)))
