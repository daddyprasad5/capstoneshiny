#functions and data load for capstone ngram shiny app
library(ggplot2)
library(data.table)
library(gtools)
library(plyr)
library(dplyr)
library(NLP) 
library(openNLP)
library(tm)
library(quanteda)

#gets the last word from an ngram
getLastWord <- function (ngramIn) {
        places <- gregexpr(pattern ='_',ngramIn)[[1]]
        lastWord <- substring(ngramIn, places[length(places)]+1, nchar(ngramIn))
        return(lastWord)
}

#getst the last word from an ngram
getLeader <- function (ngramIn) {
        if(!is.null(ngramIn)) {
                places <- gregexpr(pattern ='_',ngramIn)[[1]]
                leader <- substring(ngramIn, 1, places[length(places)]-1)
        }
        else {leader <- NULL}
        return(leader)
}


getLastNTokens <- function (ngramIn, n) {
        if (!is.null(ngramIn)) {
                places <- gregexpr(pattern ='_',ngramIn)[[1]]
                start <- places[length(places)-(n-1)] + 1
                lastN <- substring(ngramIn, start, nchar(ngramIn))
        }
        else {lastN = NULL}
        return(lastN)
}

countFreq <- function(k, v = NULL) {
        return(sum(v==k))
}

#this function will return the subset of a distribution's answers that start with a character string
getStartsWithLastWords <- function (distroIn, prefix, count) {
        logicStartsWith <- startsWith(distroIn$lastWord, prefix)
        return(head(distroIn[logicStartsWith,]$lastWord,count))
}

#this function will return the last n words of a line of text as a ngram
#with separator separating the words
getLastNGram <- function (line, n, separator = "_") {
        if (is.null(line)) {return(NULL)}
        tokens <- tokenize(tolower(line), what=c("word"), 
                           removeNumbers = TRUE,removePunct = TRUE,
                           removeSymbols = TRUE, removeSeparators = TRUE, 
                           removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                           ngrams = n, skip = 0L,concatenator = separator, simplify = TRUE, 
                           verbose = FALSE)
        return(tokens[length(tokens)])
}

isEndOfWord <- function (ch) {
        #takes a single letter in and tests for end of word markers
        return(ch %in% c(" ", "?", ".", "!", ",", ";", ":"))
}

#the workhorse - gives the "answer" - a distribution of possible last words
getDistro <- function(line, nGramIn, triGramsIn, biGramsIn, uniGramsIn, dict, POStriplets) {
        
        #this line lets the user pass in any ngram size larger than unigram
        biGramIn <- paste0(getLastWord(getLeader(nGramIn)), "_", getLastWord(nGramIn))
        
        #get tri-gram matches
        matches <- triGramsIn[biGramIn == triGramsIn$leader,]
        
        #apply standard discount .5
        preFreqSum <- sum(as.integer(matches$freq))
        matches$freq <- matches$freq - .5
        postFreqSum <- sum(as.numeric(matches$freq))
        D = preFreqSum - postFreqSum # the probability mass available for lower level ngrams
        
        #Get bigram matches     
        bimatches <- biGramsIn[getLastWord(biGramIn) == biGramsIn$leader,]
        bimatches <- bimatches[!(bimatches$lastWord %in% matches$lastWord), ]
        bic = sum(bimatches$freq)
        if (D > 0) {
                bimatches$freq <- (bimatches$freq/bic) * D
        }
        matches <- rbind(matches, bimatches)
        
        if (nrow(matches) < 10) {
                addUnis <- uniGramsIn
                POSUnigram <- POSFilter(line, uniGramsIn$lastWord, POSDictionary, POStriplets) 
                addUnis <- uniGramsIn[POSUnigram,]
                addUnis <- addUnis[!(addUnis$lastWord %in% matches$lastWord), ]
                #addUnis <- head(addUnis, 10-nrow(matches))
                
                #apply standard discount .25
                #                 preFreqSum <- sum(as.integer(matches$freq))
                #                 matches$freq <- matches$freq - .25
                #                 postFreqSum <- sum(as.numeric(matches$freq))
                #                 D = preFreqSum - postFreqSum # the probability mass available for lower level ngrams
                #                 
                #                 uic = sum(addUnis$freq)
                #                 if (D > 0) {
                #                         addUnis$freq <- (addUnis$freq/uic) * D
                #                 }
                
                matches <- rbind(matches, addUnis)
        }
        
        #add a frequency percent column
        c = sum(as.numeric(matches$freq))
        matches$freq_per <- as.numeric(matches$freq)/c
        
        return(matches[order(matches$freq_per, decreasing = TRUE),])
}

source("POS.R")

##load ngram tables if the objects are available on disk
ngramFile <- "ngramtables.robj"
load(ngramFile)

##replace unigram with unigram based on the number of unique bigrams the unigram completes
#uniGram <- getUnigramDistro(biGram$lastWord)

#load the POSDictionary from disk
dictFile <- "POSDictionary.robj"
load(dictFile)

#load the Triplets from disk
tripFile <- "triplets.robj"
load(tripFile)
