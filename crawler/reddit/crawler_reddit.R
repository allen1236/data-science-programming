library(dplyr)
library(httr)
library(rvest) 
library(rlist)
library(jsonlite)
library(stringr)
library(tidytext)

setwd( '~/Documents/DSP/lan' )

output_dir <- './data/output_reddit.csv'

index_url <- 'https://www.reddit.com/r/AskReddit/hot.json' 
base_url <- 'https://www.reddit.com'

get_comments <- function( replies ) {
    text <- c(character())
    for ( i in 1:length( replies ) ) {
        tryCatch({
            text <- replies[[i]][[2]][[3]]$data$body %>% c( text )
            r = replies[[i]][[2]][[3]]$data$replies[ replies[[i]][[2]][[3]]$data$replies != "" ]
            if( length(r) > 0 ) {
                text <- c( get_comments(r), text )
            } 
        },
        error = function(msg) {
            #print(paste('error', i))
        })
    }
    return( text )
}
get_text <- function( tail_url ) {
    r <- paste( base_url, tail_url, '.json', sep='' ) %>% GET()
    j <-content(r,as="parsed") 
    j %>% print
}
get_urls <- function( index_url ) {
    json <- fromJSON( index_url )
    urls <- json[[2]][[3]]$data$permalink
    return( urls )
}
combine_data = function( text, data ) {
    tibble( text=text ) %>%
        unnest_tokens( word, text ) %>% 
        count( word, sort=TRUE ) %>%
        rbind( data ) %>%
        group_by( word ) %>%
        summarise( n = sum(n) ) %>%
        return
}

comment <- "
data = tibble( word=character(), n=integer() )
for ( tail_url in get_urls( index_url ) ) {
    data <- get_text( tail_url ) %>% combine_data( data )
    print( data )
}
"

tail_url <- '/r/datasets/comments/8c9f4j/i_have_implemented_a_crawler_for_reddit_data/' 
get_text( tail_url )
