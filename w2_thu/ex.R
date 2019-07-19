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

str <- '
CoMatrix <- tdm %*% t(tdm)
total_occurrences <- rowSums(CoMatrix)
smallid = which(total_occurrences < median(total_occurrences))
co_occurrence_d = CoMatrix / total_occurrences
co_occurrence_s = co_occurrence_d[-as.vector(smallid),-as.vector(smallid)]

require(igraph)
graph <- graph.adjacency(round(co_occurrence_s*10),
                         mode="undirected",
                         diag=FALSE)
plot(graph,
     vertex.label=names(data),
     edge.arrow.mode=0,
     vertex.size=1,
     edge.width=E(graph)$weight,
     layout=layout_with_fr) 
'

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
rank %>% print
