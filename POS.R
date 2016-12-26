#part of speech tagging
##
##
## my plan is to figure out what parts of speech generally follow the parts of speech of the 
## preceding two words.  Then use that to reduce the list of possible words in the unigram 
## case. 

## will need tripfreq and POSDictionary to be loaded to the shiny site

library ( plyr )
library ( dplyr )
library ( NLP )
library ( openNLP )
#library ( openNLPmodels.en )
library ( tm )
library ( stringr )
library ( gsubfn )
library(data.table)
library(quanteda)

#<explain...
getNextPOSDistro <- function(POSDistro, line) {
        POSline <- as.character(getPOS(line)$tags)
        lineLen <- length(POSline)
        POSleader <- paste0(POSline[lineLen - 1], "_", POSline[lineLen])
        #POSdistro[POSdistro$leader == paste(getPOS(POCLeader)$tags, collapse = "_"), ]
        POSDistro[POSDistro$leader == POSleader, ]
}

#returns the words in sequence and the POS of each word
getPOS <- function(x) {
        x <- as.String(paste(x, collapse = ' '))
        sent_token_annotator <- Maxent_Sent_Token_Annotator ()
        word_token_annotator <- Maxent_Word_Token_Annotator()
        pos_tag_annotator <- Maxent_POS_Tag_Annotator ()
        y1 <- NLP::annotate (x , list ( sent_token_annotator, word_token_annotator ) )
        y2 <- NLP::annotate (x , pos_tag_annotator , y1 )
        y2w <- subset ( y2 , type == "word")
        tags <- sapply ( y2w$features , '[[', "POS")
        words <- as.character(tolower(x[y2w]))
        tagged_words <- data.frame(words = words, tags = tags)
        return(tagged_words)
}

#gives the frequency of each POS triplet
getPOSTripletFreq <- function(x, trials, num_chunks) {
        
        print("building triplet distro...")
        start.time <- Sys.time()
        
        procChunk <- function(chunk, chunk_size, filestem, x) {
                
                #get chunk
                chunk_range <- getChunkRange(chunk_size, chunk)
                x < x[chunk_range$start:chunk_range$end]
                
                #process chunk
                tagged_words <- getPOS(x)
                tagged_words <- paste(tagged_words$tags, collapse = ' ')
                triplets <- as.data.frame(rev(sort(table(
                        tokenize(tagged_words, what=c("word"), 
                                 removeNumbers = TRUE,removePunct = TRUE,
                                 removeSymbols = TRUE, removeSeparators = TRUE, 
                                 removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                                 ngrams = 3L, skip = 0L,concatenator = "_", simplify = TRUE, 
                                 verbose = FALSE)
                ))), stringsAsFactors = FALSE)
                colnames(triplets) <- c("POCtriplet", "freq")
                
                save(triplets, file = paste0(filestem, chunk, ".robj"))
                
                
                print(paste("saved chunk", chunk))
                return(chunk)
        }
        
        aggTrips <- function(agg, next_chunk, filestem) {
                load(paste0(filestem, next_chunk, ".robj")) #loads object triplets
                agg <- rbind(agg, triplets)
                agg <- aggregate(freq~.,agg,sum) 
                print(paste("aggregated chunk", chunk))
                return(agg)
        }
        
        #create sample
        x <- x[sample(length(x), size = trials)]
        
        filestem <- "tripletstemp/triplets"
        
        #create & store chunks
        chunk_size <- as.integer(length(x) / num_chunks)
        sapply(1:num_chunks, procChunk, chunk_size = chunk_size, x=x, filestem = filestem)
        
        #aggregate chunks
        load(paste0(filestem, "1", ".robj")) #loads object triplets with first chunk
        for (chunk in 1:num_chunks) {
                triplets <- aggTrips(triplets, chunk, filestem)
        }
        
        print("adding lastPOC")
        triplets$lastPOC <- unlist(lapply(triplets$POCtriplet,getLastWord))
        print("adding leader")
        triplets$leader <- unlist(lapply(triplets$POCtriplet,getLeader))
        
        print("finished building triplet distro...")
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print(time.taken)
        
        save(triplets, file = "triplets.robj")
        
        return(triplets)
}

#returns the frequency of each word & POS combination in the given character vector
#intended to be passed the training data
#intended to be used to weight potential last-words by how often they are the correct part of speach
getPOSdict <- function(x) {
        #x <- x[sample(length(x), samplesize * length(x))]
        tagged_words <- getPOS(x)
        tw_freq <- ddply(tagged_words, .(words, tags), count)
        tw_freq$key <- paste0(tw_freq$words, "_", tw_freq$tags)
        return(tw_freq)
}

buildPOSdict <- function(x, num_chunks) {
        print("building dictionary iteratively...")
        start.time <- Sys.time()
        procChunk <- function(chunk, chunk_size, x) {
                chunk_range <- getChunkRange(chunk_size, chunk)
                dict_chunk <- getPOSdict(x[chunk_range$start: chunk_range$end])
                save(dict_chunk, file = paste0("POStemp/POSDict", chunk, ".robj"))
                print(paste("saved chunk", chunk))
                return(chunk)
        }
        
        aggDict <- function(agg, next_chunk, filestem) {
                load(paste0(filestem, next_chunk, ".robj")) #loads object dict_chunk
                agg <- rbind(agg, dict_chunk)
                agg <- aggregate(n~.,agg,sum) 
                print(paste("aggregated chunk", chunk))
                return(agg)
        }
        
        #create & store chunks
        chunk_size <- as.integer(length(x) / num_chunks)
        sapply(1:num_chunks, procChunk, chunk_size = chunk_size, x=x)
        
        #aggregate chunks
        filestem <- "POStemp/POSDict"
        load(paste0(filestem, "1", ".robj")) #loads object dict_chunk with first chunk
        POSDictionary <- dict_chunk
        for (chunk in 1:num_chunks) {
                POSDictionary <- aggDict(POSDictionary, chunk, filestem)
        }
        
        #wrap-up
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print("dictionary build complete")
        print(time.taken)
        save(POSDictionary, file = "POSDictionary.robj")
        return(POSDictionary)
}


POSFilter <- function(line, guesses, dict, tripFreq) {
        #line is a single-item character vector, whose next word we are trying to guess
        #guesses is a charactor vector with one item per last word guess
        #dictionary is the dictonary of words tagged with the frequency of their use 
        # as different parts of speech
        #tripFreq is a frequency distribution of POS triplets
        
        #matches <- logical(length(guesses))
        
        #POS for each guess, and frequency of each guess's use as that POS
        #guessesPOS <-dict[dict$words %in% guesses,]
        gDF <- data.frame(guesses)
        fdict <- dict[dict$words %in% guesses,]
        guessesPOS <- merge(gDF, fdict, by.x="guesses", by.y="words", sort = FALSE, all.x = TRUE, all.y = FALSE)
        levels(guessesPOS$tags)[length(levels(guessesPOS$tags))+1] <-"MIS"
        #add "MIS" for words with no part of speech in the dictionary
        if (length(guessesPOS[is.na(guessesPOS$tags),]$tags) >0) {
                guessesPOS[is.na(guessesPOS$tags),]$tags <- "MIS"
        }
        
        #last word part of speech possibilities...
        #...in descending likelihood order
        #...filtered to cover 80% of the distribution
        nextPOSDistro <- getNextPOSDistro(tripFreq, line)
        nextPOSDistro <- arrange(nextPOSDistro, desc(freq)) %>%
                mutate(cumsum = cumsum(freq),freq_perc = round(freq / sum(freq), 20),cum_freq = cumsum(freq_perc))
        nextPOSDistro <- nextPOSDistro[(nextPOSDistro$cum_freq-nextPOSDistro$freq_per) < .50,]
        
        #match!
        OK_POC <- c(nextPOSDistro$lastPOC,"MIS")
        matches <- guessesPOS[guessesPOS$tags %in% OK_POC,]$guesses
        logicalMatches <- guesses %in% matches
        
        return(logicalMatches)
}
