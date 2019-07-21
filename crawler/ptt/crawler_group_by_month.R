library(dplyr)
library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(tmcn)
library(NLP)
library(stringr)
setwd('~/Documents/DSP/Data_Science_Programming/crawler/ptt')

boardname <- 'studyabroad'
start_page <- 1
end_page <- 1567
group_by <- 'month'

ptt_prefix <- 'https://www.ptt.cc'
ptt_ntu_prefix <- paste0( 'https://www.ptt.cc/bbs/', boardname, '/index' )
output_path <- paste0( './output/', boardname, '_', start_page, '_', end_page, '.csv' )

dir.create( paste0( './output/group_by_', group_by, '/', boardname ), showWarning=F )
urls <- function( page_num ) {
    paste0( page_num, ' (', start_page, ' to ', end_page, ')' ) %>% print
    paste0( ptt_ntu_prefix, page_num, '.html' ) %>%
        GET( set_cookies( `over18`="1" ) ) %>% htmlParse() %>%
        xpathSApply( "//div[@class='title']/a[@href]", xmlAttrs )
}
urls <- sapply( start_page:end_page, urls ) %>% unlist
get_time <- function( str ) {
    t <- strsplit( str, split=' ', fixed=T )[[1]]
    c(t[5], t[2], t[3], t[4]) %>% return
}
get_post <- function(str) {
    str %>% str_replace_all( '^.+\n', '' ) %>%
    str_replace_all( '\n\n\\-\\-\n(.|\n)*$', '' ) %>% return
}
get_text <- function( url ) {
    html <- paste0( ptt_prefix, url ) %>% GET( set_cookies( `over18`="1" ) ) %>% htmlParse()
    timestamp <- xpathSApply( html, "//span[@class='article-meta-value']", xmlValue )[4]
    timestamp <- gsub( "  ", " 1", timestamp ) %>% get_time %>% print
    content <- xpathSApply( html, "//div[@id='main-content']", xmlValue ) 
    post <- get_post( content )
    comment <- xpathSApply( html, "//span[@class='f3 push-content']", xmlValue ) %>% unlist() %>% paste( collapse='' )
    content <- paste( post, comment )
    hour <- strsplit( timestamp[4], split=':', fixed=T )[[1]][1] %>% print
    month <- timestamp[2]
    file_name <- paste0( './output/group_by_', group_by, '/', boardname, '/', month, '.txt' )
    write( content, file_name, append=T )
}
sapply( urls, get_text )
    
