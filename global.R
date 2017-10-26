library(shiny)
library(shinySignals)
library(dplyr)
library(bubbles)
library(stringr)
library(quanteda)
library(readtext)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(lubridate)
library(stylo)
library(tidytext)
library(tm)
library(wordcloud)
library(xlsx)
library(gdata)
library(readxl)
library(htmlwidgets)
library(httr)
source("bloomfilter.R")

getFBID <- function(fburl){
   return(unlist(strsplit(httr::POST(url='https://findmyfbid.com',body=list(url = fburl), encode="json")$headers$`amp-redirect-to`,'/'))[5])
}

workdir <- "/home/cdesantana/DataSCOUT/Objectiva/PapoCorreria/dashboard"

getIndiceDeSentimentoReactions <- function(reactions){
   reacoes <- toupper(reactions)
   allreacoes <- c("LOVE","HAHA","ANGRY","SAD");
   ml <- length(which(sentimentos==allsentimentos[1]));#Positivo
   mh <- length(which(sentimentos==allsentimentos[2]));#Positivo
   ma <- length(which(sentimentos==allsentimentos[3]));#Negativo
   ms <- length(which(sentimentos==allsentimentos[4]));#Negativo
   mt <- ml + mh + ma + ms;#Total
   
   indicesentimento <- as.numeric((ml + mh - ma - ms)/mt)
   
   return(indicesentimento)
}

#workdir <- "/srv/shiny-server/cns/BadogueExcel"
#workdir <- "/home/cdesantana/DataSCOUT/Objectiva/BadogueExcel"
workdir <- system("cat myworkdir",intern=TRUE)
badwords <- c("scontent.xx.fbcdn.net","https","oh","oe","pra"," v ","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","é","sr","governador","rui","costa","senhor")

getTidySentimentos <- function(file){
   polaridade <- toupper(file$Polaridade)
   text <- file$Conteúdo
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   tokenInfo <- summary(myCorpus)
   kwic(myCorpus, "gestor")
   myStemMat <- dfm(myCorpus, remove = stopwords("portuguese"), stem = TRUE, remove_punct = TRUE)
   byPolaridadeDfm <- dfm(myCorpus, groups = polaridade, remove = c(stopwords("portuguese"),badwords), remove_punct = TRUE)
   ap_td <- tidy(byPolaridadeDfm)
   names(ap_td) <- c("sentimento","term","count")
   return(ap_td);
}

getDFMatrix <- function(text){
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   tokenInfo <- summary(myCorpus)
   kwic(myCorpus, "gestor")
   myStemMat <- dfm(myCorpus, remove = stopwords("portuguese"), stem = TRUE, remove_punct = TRUE)
   mydfm <- dfm(myCorpus, remove = c(stopwords("portuguese"),badwords), remove_punct = TRUE, remove_numbers= TRUE)
   return(mydfm)
   #   ap_td <- tidy(mydfm)
   #   names(ap_td) <- c("sentimento","term","count")
   #   return(ap_td);
}

options(shiny.fullstacktrace = TRUE)



# An empty prototype of the data frame we want to create
prototype <- data.frame(date = character(), time = character(),
  size = numeric(), r_version = character(), r_arch = character(),
  r_os = character(), package = character(), version = character(),
  country = character(), ip_id = character(), received = numeric())

# Connects to streaming log data for cran.rstudio.com and
# returns a reactive expression that serves up the cumulative
# results as a data frame
packageStream <- function(session) {
  # Connect to data source
  sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
  # Clean up when session is over
  session$onSessionEnded(function() {
    close(sock)
  })

  # Returns new lines
  newLines <- reactive({
    invalidateLater(1000, session)
    readLines(sock)
  })

  # Parses newLines() into data frame
  reactive({
    if (length(newLines()) == 0)
      return()
    read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
      col.names = names(prototype)
    ) %>% mutate(received = as.numeric(Sys.time()))
  })
}

# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
packageData <- function(pkgStream, timeWindow) {
  shinySignals::reducePast(pkgStream, function(memo, value) {
    rbind(memo, value) %>%
      filter(received > as.numeric(Sys.time()) - timeWindow)
  }, prototype)
}

# Count the total nrows of pkgStream
downloadCount <- function(pkgStream) {
  shinySignals::reducePast(pkgStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    memo + nrow(df)
  }, 0)
}

# Use a bloom filter to probabilistically track the number of unique
# users we have seen; using bloom filter means we will not have a
# perfectly accurate count, but the memory usage will be bounded.
userCount <- function(pkgStream) {
  # These parameters estimate that with 5000 unique users added to
  # the filter, we'll have a 1% chance of false positive on the next
  # user to be queried.
  bloomFilter <- BloomFilter$new(5000, 0.01)
  total <- 0
  reactive({
    df <- pkgStream()
    if (!is.null(df) && nrow(df) > 0) {
      # ip_id is only unique on a per-day basis. To make them unique
      # across days, include the date. And call unique() to make sure
      # we don't double-count dupes in the current data frame.
      ids <- paste(df$date, df$ip_id) %>% unique()
      # Get indices of IDs we haven't seen before
      newIds <- !sapply(ids, bloomFilter$has)
      # Add the count of new IDs
      total <<- total + length(newIds)
      # Add the new IDs so we know for next time
      sapply(ids[newIds], bloomFilter$set)
    }
    total
  })
}
