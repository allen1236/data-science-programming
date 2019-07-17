library(dplyr)
library(httr)
library(rvest) 
library(stringr)
library(tidytext)

setwd('~/Documents/DSP/Data_Science_Programming/crawler/stackexchange')

page_total <- 400
posts_per_page <- 50
time_to_wait <- 44
progress_path <- './output/progress_crawler_stackexchange.txt'
post_num <- page_total * posts_per_page

forums <- 0
current_forum_index <- 0
current_page_index <- 0

data = tibble( word=character(), n=integer() )
index_url <- ''
base_url <- '' 
output_path <- ''

set_strings = function() {
    index_url <<- paste( 'https://', forums[current_forum_index], '.com/questions?tab=Votes&pagesize=50&page=', sep='' )
    base_url <<- paste( 'https://', forums[current_forum_index],'.com', sep='' )
    output_path <<- paste('./output/output_', forums[current_forum_index], '_', post_num, '.csv', sep='' )
}
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
    repeat{ 
        r <- GET(paste( base_url, url, sep='' ))
        if( http_error( r ) ) {
            if ( status_code( r ) == 429 ) wait() 
            else stop( http_status( r ) )
        }
        else break 
    }
    html <- read_html( r )
    code_node <- html_nodes( html, "code" ) 
    math_node <- html_nodes( html, ".math-container" )
    xml_remove( code_node )
    xml_remove( math_node )
    post_text <- html_nodes( html, ".post-text" ) %>% html_text() 
    comment_text <- html_nodes( html, ".comment-copy" ) %>% html_text()
    c( post_text, comment_text ) %>%
        str_replace_all( '\\$[^$]+\\$', ' ' ) %>%
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

load_progress <- function( progress_path ) {
    v <- scan( progress_path, what='character' )
    len <- length(v)
    forums <<- v[1:(len-2)]
    current_forum_index <<- strtoi( v[(len-1)] )
    current_page_index <<- strtoi( v[len] )
    set_strings()
    if( current_page_index != 0 ) {
        input_file <- read.csv( output_path, header=T, sep=',' )
        data <<- tibble( word=input_file$word, n=input_file$n )
    }
}
save_progress <- function( progress_path, new_forum_index, new_page_index ) {
    data <<- data[which(!grepl("[^a-z']+", data$word)),] 
    #arrange
    data <<- data[order(data$n, decreasing=TRUE),]
    write.csv(data,file=output_path,row.names=FALSE) 
    print( 'data saved' )
    if ( new_page_index == page_total ) {
        current_forum_index <<- current_forum_index + 1
        current_page_index <<- 0
        set_strings()
        data <<- tibble( word=character(), n=integer() )
    }
    else {
        current_forum_index <<- new_forum_index
        current_page_index <<- new_page_index
    }
    info = c( forums, current_forum_index, current_page_index )
    print( info )
    output <- file( progress_path )
    writeLines( info, output )
    print( 'info saved' )
}

load_progress( progress_path )
while( current_forum_index <= length(forums) ) {
    for( i in (current_page_index+1):page_total ) {
        all.text <- tibble( text = character() )
        start.time_loop <- proc.time() 
        for( tail in get_urls(i) ) {
            all.text <- tibble( text=get_text( tail ) )  %>% rbind( all.text )
        }
        if ( ( time_left <- time_to_wait - (proc.time()-start.time_loop)[3] ) > 0 ) Sys.sleep( time_to_wait - (proc.time()-start.time_loop)[3] )
        data <- combine_data( all.text, data )
        cat( forums[current_forum_index], '.com   progress: ', i, '/', page_total, '\n', sep='' )
        if( i %% 10 == 0 | i == page_total ) save_progress( progress_path, current_forum_index, i )
    }
}
