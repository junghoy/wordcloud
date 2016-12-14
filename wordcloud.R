# Importing necessary libraries
# Needs installation if they have not been installed before
library(SnowballC)
library(tm)
library(RCurl)
library(XML)

# Reading text data 
# If the data is not in english, set international locale
parse_file <- function(filePath, english = TRUE){
  if(!english) {Sys.setlocale('LC_ALL','C')}
  temp <- try(readLines(filePath, warn=FALSE))
  return(temp)
}

# Reading html 
parse_url <- function(url, english = TRUE){
  if(!english) {Sys.setlocale('LC_ALL','C')}
  xData <- getURL(url)
  doc <- htmlParse(xData)
  temp <- xpathSApply(doc, "//p", xmlValue)
  return(temp)
}

# Split text into words, remove blanks and "words" containing numbers, and convert
# to lower case.  Allow hyphenated words.
tokenizer <- function(temp) {
  re1 <- "[^[:alpha:]][-][^[:alpha:]]"
  re2 <- "([[:punct:]]+)|([[:space:]])"
  re <- paste0("(", re1, "|", re2, ")")
  text <- strsplit(temp, re)
  text <- tolower(unlist(text))
  text <- text[nchar(text) > 1]
  text <- grep("[0-9]", text, invert=TRUE, value=TRUE)
  return(text)
}

# filter stopwords, default is stopwords("english)
filter_stopwords <- function(text, stopWords = stopwords("english")){
  text <- text[!(text %in% stopWords)]
  return(text)
}

# Implement stemming by using tm library's stemDocument function
stem <- function(text, language = "english"){
  text <- wordStem(text, language = language)
  return(text)
}

# Weight by counts
weight_by_counts <- function(text, stem = FALSE){
  if(stem){text = stem(text)}
  count_by_word <- data.frame(sort(table(text), decreasing=TRUE))
  return(count_by_word)
}

# Scale the counts into cex size 
scale <- function(corpus, scale = c(4,0.5)){
  max_freq <- max(corpus$Freq)
  corpus$cex <- (scale[1] - scale[2]) * corpus$Freq/max_freq + scale[2]
  return(corpus)
}

# Bounding box
# If the word includes the tail alphabets, increase the height by 20%
bounding_box <- function(word, cex.scale = 1, font = NULL){
  tails <- "g|j|p|q|y"
  width <- strwidth(word, cex = cex.scale, font = font)
  height <- strheight(word, cex = cex.scale, font = font)
  if (grepl(tails, word)) {height <- height * 1.2}
  return(c(width, height))
}

# Define coordinate of a text
# Start centered
coordinate <- function(word, cex.scale = 1, vertical = FALSE){
  width <- bounding_box(word, cex = cex.scale)[1]
  height <- bounding_box(word, cex = cex.scale)[2]
  x <- 0.5
  y <- 0.5
  
  maxx <- x + width/2
  minx <- x - width/2
  maxy <- y + height/2
  miny <- y - height/2
  
  if(vertical){
    maxx <- x + height/2
    minx <- x - height/2
    maxy <- (y + width/2)*1.05 
    miny <- (y - width/2)*0.95
  }
  coord <- list(text = word, minx = minx, maxx = maxx, miny = miny, 
                maxy = maxy, x_coord = x, y_coord = y)
  return(coord)
}

# Checks intersection of two coordinates
intersect <- function(a,b){
  #minx(b) > maxx(a) | miny(b) > maxy(a) | minx(a) > maxx(b) | miny(a) > maxy(b)
  if((b$minx > a$maxx) | (b$miny > a$maxy) | (a$minx > b$maxx) | (a$miny > b$maxy)){
    return(FALSE)
  } else
    return(TRUE)
}

# Checks intersection of one coordinate to a list of coordinates
intersect_any <- function(a, boxes){
  for(i in 1:length(boxes)){
    if(intersect(a, boxes[[i]])){
      return(TRUE)
      break
    }
  }
  return(FALSE)
}

# Plotting wordcloud on display pane
wordcloud <- function(file, stem = FALSE, stopWords = stopwords("english"), 
                      language = "english", min.freq = 3, max.words = 100, 
                      rot.per = 0.3, output.png = TRUE, ...){
  
  if(grepl(pattern = ".txt$", file)){text <- parse_file(file)}
  else if(grepl(pattern = "^http[s]?://", file)){text <- parse_url(file)}
  text <- tokenizer(text)
  text <- filter_stopwords(text, stopWords = stopWords)
  if(stem){text <- stem(text, language = language)}
  corpus <- weight_by_counts(text)
  corpus <- scale(corpus)
  corpus <- corpus[corpus$Freq >= min.freq, ]
  
  # Plotting part
  if(output.png){png("wordcloud.png", width = 600, height = 514)}
  plot.new()
  par(mar = c(0,0,0,0))
  list_color = c("orange", "darkgreen", "purple", "red", "blue")
  
  # Write the first word on the plot
  first.word <- coordinate(as.character(corpus[1,1]), cex.scale = corpus[1,3])
  text(first.word$x_coord, first.word$y_coord, labels = first.word$text, 
       cex = corpus[1,3], col = sample(list_color, 1))
  boxes <- list()
  boxes[[1]] <- first.word
  
  # For each iteration, rStep increases by 0.05
  # thetaStep increases by 0.1
  rStep <- 0.05
  thetaStep <- 0.1
  
  if(nrow(corpus) < max.words){max.words <- nrow(corpus)}
  
  for(i in 2:max.words){
    rotWord <- runif(1) < rot.per
    coord <- coordinate(as.character(corpus[i,1]), cex.scale = corpus[i,3], vertical = rotWord)
    temp <- coord
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    inside_box <- TRUE
    
    while(intersect_any(temp, boxes)){
      # If the radius is too far away, throw in a warning sign
      if (r > sqrt(0.5)) {
        warning(paste(temp$text, "could not be fit on page. It will not be plotted."))
        inside_box <- FALSE
      }
      # Update the values 
      theta <- theta + thetaStep
      r <- r + rStep * thetaStep/(2 * pi)
      x_change <- r * cos(theta)
      y_change <- r * sin(theta)
      temp$x_coord <- coord$x_coord + x_change
      temp$y_coord <- coord$y_coord + y_change
      temp$minx <- coord$minx + x_change
      temp$maxx <- coord$maxx + x_change
      temp$miny <- coord$miny + y_change
      temp$maxy <- coord$maxy + y_change
    }
    # If the box does not intersect, add the box to the list
    boxes[[i]] <- temp
    # Plot texts on the display
    text(temp$x, temp$y, temp$text, srt = 90 * rotWord, cex = corpus[i,3], col = sample(list_color,1))
  }
  if(output.png){dev.off()}
}