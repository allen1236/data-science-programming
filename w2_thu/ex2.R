library('dplyr')
library('tm')
library('tmcn')
library('NLP')
library('jiebaRD')
library('jiebaR')
library('RColorBrewer')
library('wordcloud')
library('Matrix')

setwd( '~/Documents/DSP/Data_Science_Programming/w2_thu/' )
    
get_tdm <- function( doc ) {
    # cut the words
    cutter <- worker()
    new_words <- read.csv( './data/new_user_word.csv', header=T )
    new_user_word( cutter, as.character(new_words$word), as.character(new_words$pos) )
    segments <- function(d) { 
        seg <- segment( d[[1]], cutter ) 
        chosen <- which( nchar(seg) > 1 )
        seg[chosen] %>% return
    }
    segments <- lapply( doc, segments )
     
    # count tdm and turn it into matrix
    tokens = function( word ) { as.data.frame( table( word ) ) }
    tokens <- lapply( segments, tokens )
    tdm <- tokens[[1]]
    n <- length( segments )
    for( id in 2:n) {
        tdm <- merge( tdm, tokens[[id]], by='word', all=T ) 
    }
    tdm[is.na(tdm)] <- 0
    row_name <- tdm$word
    col_name <- names(doc)
    tdm <- as.matrix( tdm[,2:(n+1)] )
    colnames( tdm ) <- col_name
    rownames( tdm ) <- row_name
    return( tdm )
}
get_tfidf <- function( tdm ) {
    # inplement tfidf
    n <- ncol(tdm)
    tf <- apply( tdm, 2, function(c){c/sum(c)} )
    idf <- function( word ) {
        log2( (n+1) / nnzero(word) )
    }
    idf <- apply( tdm, 1, idf )
    tfidf <- apply( tf, 2, function(r){r*idf} )
    return( tfidf )
}

doc <- Corpus( DirSource('./data/studyabroad') ) %>%
    tm_map( removePunctuation ) %>%
    tm_map( removeNumbers ) %>%
    tm_map( function(word) {
        gsub( "[A-Za-z0-9]", "", word )
    })
tfidf <- get_tdm( doc ) %>% get_tfidf()

colnames( tfidf ) <- gsub( '.txt', '',colnames( tfidf ) )
# grab the top 10 words
top <- function( col ) { rownames(tfidf)[order(-col)][1:10] }
top <- apply( tfidf, 2, top ) %>% print %>%
    as.matrix() %>%
    table() %>%
    as.data.frame()
top <- top[order(-top$Freq),]
top %>% head( 50 )
