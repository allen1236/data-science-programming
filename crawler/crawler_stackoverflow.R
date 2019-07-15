library(dplyr)
library(httr)
library(rvest) 
library(stringr)
library(tidytext)

setwd('~/Documents/DSP/Data_Science_Programming/crawler/')

forum <- 'math'
page_total = 5
time_to_wait = 53 #sec
index_url = paste( 'https://', forum, '.stackexchange.com/questions?tab=Votes&pagesize=50&page=', sep='' )
base_url = paste( 'https://', forum,'.stackexchange.com', sep='' )
output_path = paste('./output_crawler/output_', forum, '_', post_num, '.csv', sep='' )

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
        str_replace_all( "[^a-zA-Z']", " ")  %>% print
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
    print( 'retry after 10 sec...' )
    Sys.sleep( 10 )
}

data = tibble( word=character(), n=integer() )
start.time <- proc.time() 
for( i in 1:page_total ) {
    all.text = tibble( text = character() )
    start.time_loop <- proc.time() 
    for( tail in get_urls(i) ) {
        all.text = tibble( text=get_text( tail ) )  %>% 
            rbind( all.text )
    }
    if ( ( time_left <- time_to_wait - (proc.time()-start.time_loop)[3] ) > 0 ) 
        Sys.sleep( time_to_wait - (proc.time()-start.time_loop)[3] )
    data = combine_data( all.text, data )
    cat( 'progress: ', i, '/', page_total, '  estimated: ', ( (proc.time()-start.time) / i * (page_total-i) )[3], 'sec\n', sep=''  )
}

data <- data[which(!grepl("[^a-z']+", data$word)),] 
data <- data[order(data$n, decreasing=TRUE),]
write.csv(data,file=output_path,row.names=FALSE) 
