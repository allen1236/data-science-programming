library('dplyr')
library('tm')
library('tmcn')
library('NLP')
library('jiebaRD')
library('jiebaR')
library('RColorBrewer')
library('wordcloud')
library('Matrix')
library('ggplot2')
library('reshape2')

setwd( '~/Documents/DSP/Data_Science_Programming/w2_thu/' )
    
doc <- Corpus( DirSource('./data/studyabroad') ) %>%
    tm_map( removePunctuation ) %>%
    tm_map( removeNumbers ) %>%
    tm_map( function(word) {
        gsub( "[A-Za-z0-9]", "", word )
    })
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
# inplement tfidf
tf <- apply( tdm, 2, function(c){c/sum(c)} )
idf <- function( word ) {
    log2( (n+0.2) / nnzero(word) )
}
idf <- apply( tdm, 1, idf )
tfidf <- apply( tf, 2, function(r){r*idf} )

colnames( tdm ) <- colnames( tfidf ) <- gsub( '.txt', '',colnames( tfidf ) )

# grab the top 10 words and count the top 8 words
top <- function( col ) { rownames(tfidf)[order(-col)][1:6] }
top <- apply( tfidf, 2, top ) %>% print %>%
    as.matrix() %>%
    table() %>%
    as.data.frame()
top8 <- top[order(-top$Freq),][1:8,1] %>% as.character() %>% print

# arrange months
months <- c( 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' )
#months <- factor( months, levels=month.name )
top8_plot <- tf[top8,months] %>% melt
names( top8_plot ) <- c( 'word', 'month', 'frequency' )


# plot
ggplot( data=top8_plot, aes( x=month, y=frequency, group=word, colour=word ) ) + 
    geom_line() + 
    geom_point()

# time and size
filenames <- as.array( paste0( './data/studyabroad/', months, '.txt' ) )
sizes <- apply( filenames, 1, file.size ) / 1024
size_plot <- data.frame( month=months, size=sizes )
size_plot$month <- factor( size_plot$month, levels=months )
size_plot$month %>% print
ggplot( data=size_plot, aes( x=month, y=size ) ) +
    geom_bar( stat='identity' )

# normalize
top8_norm <- merge( top8_plot, size_plot, by='month' )%>%
    mutate( norm_f=frequency/size )
ggplot( data=top8_norm, aes( x=month, y=norm_f, group=word, colour=word ) ) + 
    geom_line() + 
    geom_point()
