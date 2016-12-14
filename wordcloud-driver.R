#!/usr/bin/env Rscript

source("/Users/jungyeom/Dropbox/wordcloud.R")
args <- commandArgs(trailingOnly = TRUE)

## Collect arguments
args <- commandArgs(TRUE)

## Default setting when no arguments passed
if(length(args) < 1) {
  args <- c("--help")
}

## Help section
if("--help" %in% args) {
  cat("
      The R Script
      
      Arguments:
      --file=filePath        - string
      --stem=stem            - boolean
      --stopwords=stopWords  - string
      --language=language    - string
      --minfreq=min.freq     - numeric
      --maxwords=max.words   - numeric
      --rotper=rot.per       - numeric
      --output=output.png    - boolean
      
      Example:
      ./wordcloud-driver.R --file='http://www.textfiles.com/drugs/bor&drug.txt' --stem=FALSE --rotper=0.4 \n\n")
  
  q(save="no")
}

## Parse arguments (we expect the form --arg=value)
parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))
names(argsL) <- argsDF$V1

## If no file, write error
if(is.null(argsL$file)) {
  cat("No file detected")
}

## Stem default to false
if(is.null(argsL$stem)) {
  argsL$stem = FALSE
}

## Stopwords default to english
if(is.null(argsL$stopwords)) {
  argsL$stopwords = stopwords("english")
}

## Language default to english
if(is.null(argsL$language)) {
  argsL$language = "english"
}

## Mininum frequency default to 3
if(is.null(argsL$minfreq)) {
  argsL$minfreq = 3
}

## Maximum words default to 100
if(is.null(argsL$maxwords)) {
  argsL$maxwords = 100
}

## Rotation percentage default to 0.35
if(is.null(argsL$rotper)) {
  argsL$rotper = 0.35
}

## Output png default to TRUE
if(is.null(argsL$output)) {
  argsL$output = TRUE
}

wordcloud(file = argsL$file, stem = argsL$stem, stopWords = argsL$stopwords, 
          language = argsL$language, min.freq = argsL$minfreq, max.words = argsL$maxwords,
          rot.per = argsL$rotper, output.png = argsL$output)