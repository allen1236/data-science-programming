lapply( c(
    'dplyr',
    'tm',
    'tmcn',
    'NLP',
    'jiebaRD',
    'jiebaR',
    'RColorBrewer',
    'wordcloud',
    'Matrix'
), library, character.only=T )

setwd( '~/Documents/DSP/Data_Science_Programming/w2_thu/' )
#raw_data <- scan( './data/mistborn_clean.txt', what='character', sep='\n' )
raw_data <- readChar( './data/mistborn.txt', file.info( './data/mistborn.txt' )$size ) %>%
    strsplit( '#' )
raw_data <- raw_data[[1]]
chap_num <- length( raw_data )
    
# cut the words
cutter <- worker(  bylines=T )
new_words <- read.csv( './data/mistborn.csv', header=T )
new_user_word( cutter, as.character(new_words$word), as.character(new_words$pos) )
segments <- segment( raw_data, cutter )
 
# count tdm and turn it into matrix
tokens = function( word ) { as.data.frame( table( word ) ) }
tokens <- lapply(segments, tokens)
tdm <- tokens[[1]]
for( chapter in 2:chap_num) {
    tdm <- merge( tdm, tokens[[chapter]], by='word', all=T ) 
}
tdm[is.na(tdm)] <- 0
row_name <- tdm$word
tdm <- as.matrix( tdm[,2:(chap_num+1)] )
colnames( tdm ) <- 1:chap_num
rownames( tdm ) <- row_name

# inplement tfidf
tf <- apply( tdm, 2, function(c){c/sum(c)} )
idf <- function( word ) {
    log( (chap_num+1) / nnzero(word) )
}
idf <- apply( tdm, 1, idf )
tfidf <- apply( tf, 2, function(r){r*idf} )

# grab the top 10 words
rank <- function( col ) { row_name[order(-col)][1:10] }
rank <- apply( tfidf, 2, rank )


