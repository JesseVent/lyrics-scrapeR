cat("Loading getArtist function for retrieving list of artists")
getArtists <- function(){
  library(rvest)
  library(dplyr)
  require(doSNOW)
  require(doParallel)
  library(stringr)
  webpage <- read_html("https://web.archive.org/web/20170330154255/https://www.azlyrics.com/")
  attributes <- html_nodes(webpage, ".btn-menu") %>% html_attr('href')
  attributes <- as.character(attributes)
  range <- 1:as.numeric(length(attributes))
  cluster = makeCluster(4, type = "SOCK")
  registerDoSNOW(cluster)
  artistLoop <-
    function(attributes) {
      library(rvest)
      library(purrr)
      library(tibble)
      page <- read_html(attributes)
      nodes <- html_nodes(page, ".artist-col a")
      artist <- c(html_text(nodes))
      link <- c(html_attrs(nodes))
      artists <- tibble(artist = artist, link = link)
      return(artists)
    }
  artists <- foreach(i=1:length(attributes), .combine = rbind) %dopar% artistLoop(attributes[i])
  stopCluster(cluster)
  return(artists)
}

cat("Loading ask function for prompting user for the artist name")
ask <- function(){
  ANSWER <- readline("Enter Artist Name: ")
  search <- toupper(ANSWER)
  return(search)
}

cat("Loading getLyrics function to scrape all the lyrics for selected artist.")
getLyrics <- function(url){
  library(rvest)
  library(dplyr)
  require(doSNOW)
  require(doParallel)
  library(stringr)
  webpage <- read_html(url)
  songs <- c(html_nodes(webpage, "#listAlbum") %>% html_nodes("a") %>%
               html_attr('href') %>% na.omit())
  songs <- subset(songs, nchar(songs) < 99)
  attributes <- paste0("https://web.archive.org",songs) %>% as.character()
  range <- 1:as.numeric(length(attributes))
  cluster = makeCluster(4, type = "SOCK")
  registerDoSNOW(cluster)
  pb <- txtProgressBar(max = as.numeric(length(attributes)), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  abstracts <-
    function(attributes) {
      library(rvest)
      library(magrittr)
      library(purrr)
      library(tibble)
      page <- safely(read_html, "0")(attributes)
      lyrics <- ifelse (page$result == "0"," ",c(page$result %>% html_nodes("br+ div") %>% html_text(trim = TRUE)  %>% .[. != ""]))
      abstracts <- tibble(lyrics)
      return(abstracts)
    }
  results <- foreach(i = range, .options.snow = opts, .combine = rbind, .packages = "rvest") %dopar% abstracts(attributes[i])
  close(pb)
  stopCluster(cluster)
  lyrics <- unique(results)
  return(lyrics)
}

library(DT)
cat("Commence scraping artist list and display in datatable")
ARTISTS <- getArtists()
datatable(ARTISTS, style = "default")
stop("Use table to search for an artist, then run function ask()")

cat("Search for artist")
search <- ask()

# Start scraping your favourite Artist’s lyrics ---------------------------

cat("Retrieving all lyrics for artist")
slice <- subset(ARTISTS, search == artist)
artist.page <- slice$link
url <- paste0("https://web.archive.org/web/20170330154255/https://www.azlyrics.com/",artist.page)
lyrics <- getLyrics(url)

library(stringr)
library(qdap)
library(dplyr)
library(tibble)
cleaned <- gsub("\\[[^\\]]*\\]", " ", lyrics$lyrics, perl=TRUE)
write.csv(cleaned, file = "/Users/macbookpro/Development/drake/FIFTY-Lyrics.txt", col.names = FALSE, row.names = FALSE)
