library(dplyr)
library(httr)
library(rvest) 
library(stringr)
library(tidytext)

setwd('~/Documents/DSP/Data_Science_Programming/w1_thu/')
output_dir = './data/output.csv'

page_total = 200
index_url = 'https://english.stackexchange.com/questions?tab=Votes&pagesize=50&page=' 
base_url = 'https://english.stackexchange.com'
data = tibble( word=character(), n=integer() )

get_urls <- function( page_num ) {
    repeat {
        r <- paste( index_url, page_num, sep='' ) %>% GET()
        if( http_error( r ) ) {
            if ( status_code( r ) == 429 ) wait() 
            else stop( http_status( r ) )
        }
        else break 
    }
    read_html( r ) %>%
        html_nodes( '#questions a.question-hyperlink' ) %>% 
        html_attr( 'href' ) %>% 
        return
}
get_text = function( url ) {
    #html = read_html( paste( base_url, url, sep='' ) )
    repeat{ 
        r <- GET(paste( base_url, url, sep='' ))
        if( http_error( r ) ) {
            if ( status_code( r ) == 429 ) wait() 
            else stop( http_status( r ) )
        }
        else break 
    }
    html <- read_html( r )
    post_text = html_nodes( html, ".post-text" ) %>% html_text()
    comment_text = html_nodes( html, ".comment-copy" ) %>% html_text()
    c( post_text, comment_text ) %>% 
        str_replace_all( "[[:punct:]_0123456789]", " ")  %>% 
        return
}
combine_data = function( all.text, data ) {
    unnest_tokens( all.text, word, text ) %>% 
        count( word, sort=TRUE ) %>%
        rbind( data ) %>%
        group_by( word ) %>%
        summarise( n = sum(n) ) %>%
        return
}
wait <- function() {
    print( 'retry after 1 minutes...' )
    Sys.sleep( 60 )
}

total_duration <- 0

for( i in 1:page_total ) {
    start.time <- Sys.time() 
    all.text = tibble( text = character() )
    for( tail in get_urls(i) ) {
        all.text = tibble( text=get_text( tail ) )  %>% 
            rbind( all.text )
    }
    data = combine_data( all.text, data )
    total_duration <<- total_duration + ( Sys.time() - start.time )
    cat( 'progress: ', i, '/', page_total, '  estimated: ', total_duration / i * (page_total-i), 'sec\n', sep=''  )
}

data <- data[which(!grepl("[^a-z]+", data$word)),] 
data <- data[order(data$n, decreasing=TRUE),]
write.csv(data,file=output_dir,row.names=FALSE) 
