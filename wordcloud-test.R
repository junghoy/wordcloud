#!/usr/bin/env Rscript

source("/Users/jungyeom/Dropbox/wordcloud.r")
library(testthat)

#example file and url
textfile <- "http://textfiles.com/politics/1945-ger.txt"
textfile2 <- "http://textfiles.com/games/1samuslv.txt"
url <- "http://www.huffingtonpost.com/2011/01/17/i-have-a-dream-speech-text_n_809993.html"
url2 <- "https://en.wikipedia.org/wiki/Donald_Trump"

#Unit tests for the function
# Tests parse_file
test_that('Parsing text file does not return error', {
  expect_error(parse_file(textfile, english = TRUE), NA)
  expect_error(parse_file(textfile2, english = TRUE), NA)
})

# Tests parse_url
test_that('Parsing url does not return error', {
  expect_error(parse_url(url, english = TRUE), NA)
  expect_error(parse_url(url2, english = TRUE), NA)
})

# Tests tokenizer
test_that('Tokenizer returns a vector of words', {
  expect_true(is.vector(tokenizer(parse_file(textfile))))
  expect_true(is.character(tokenizer(parse_file(textfile))))
  expect_true(is.vector(tokenizer(parse_file(url))))
  expect_true(is.character(tokenizer(parse_file(url))))
})

# Tests filter_stopwords
test_that('filter_stopwords filters stopwords', {
  text <- c("kevin", "is", "awesome", "who", "is", "that")
  stopwords <- c("is", "who", "that")
  expect_equal(filter_stopwords(text, stopwords), c("kevin", "awesome"))
})

# Since I imported stem function from the library, stemming should work by itself

# Weight by counts 
test_that('weight by counts', {
  text <- c("kevin", "kevin", "kevin", "john", "kevin", "john")
  expect_equal(weight_by_counts(text)$Freq, c(4,2))
})

# Scale function
test_that('scale function returns correct values', {
  text <- c("kevin", "kevin", "kevin", "john", "kevin", "john")
  # Test that all scaled sizes are greater than zero
  expect_true(all(scale(weight_by_counts(text))$cex > 0))
  # Test that the largest scaled size is 4.00
  expect_equal(max(scale(weight_by_counts(text))$cex), 4)
})

# Intersect function
test_that('intersect function works correctly', {
  plot.new()
  coord_a <- coordinate('kevin', cex.scale = 1)
  coord_b <- coordinate('kevin', cex.scale = 1, vertical = TRUE)
  # Intersect(a, a) should return TRUE
  expect_true(intersect(coord_a, coord_a))
  # Intersect(a, vertical(a)) should return TRUE
  expect_true(intersect(coord_a, coord_b))
  
  # Boxes located in two corners should not overlap
  coord_a$minx <- 0
  coord_a$maxx <- 0.2
  coord_a$miny <- 0
  coord_a$maxy <- 0.2
  coord_b$minx <- 0.8
  coord_b$maxx <- 1
  coord_b$miny <- 0.8
  coord_b$maxy <- 1
  expect_false(intersect(coord_a, coord_b))
})