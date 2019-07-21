library(dplyr)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(tmcn)
library(NLP)
library(stringr)
setwd('~/Documents/DSP/Data_Science_Programming/crawler/ptt')

ptt_prefix <- 'https://www.ptt.cc'
boardname <- 'Tech_Job'
start_page <- 1
end_page <- 1
ptt_ntu_prefix <- paste0( 'https://www.ptt.cc/bbs/', boardname, '/index' )
output_path <- paste0( './output/', boardname, '_', start_page, '_', end_page, '.csv' )

urls <- function( page_num ) {
    paste0( page_num, ' (', start_page, ' to ', end_page, ')' ) %>% print
    paste0( ptt_ntu_prefix, page_num, '.html' ) %>%
        GET() %>% htmlParse() %>%
        xpathSApply( "//div[@class='title']/a[@href]", xmlAttrs )
}
urls <- sapply( start_page:end_page, urls ) %>% unlist

get_time <- function( str ) {
    t <- strsplit( str, split=' ', fixed=T )[[1]]
    c(t[5], t[2], t[3]) %>% return
}
get_post <- function(str) {
    str %>% str_replace_all( '^.+\n', '' ) %>%
    str_replace_all( '\n\n\\-\\-\n(.|\n)*$', '' ) %>% return
}
get_text <- function( url ) {
    html <- paste0( ptt_prefix, url ) %>% GET() %>% htmlParse()
    timestamp <- xpathSApply( html, "//span[@class='article-meta-value']", xmlValue )[4]
    timestamp <- gsub( "  ", " 1", timestamp ) %>% get_time %>% print
    content <- xpathSApply( html, "//div[@id='main-content']", xmlValue ) 
    post <- get_post( content )
    comment <- xpathSApply( html, "//span[@class='f3 push-content']", xmlValue ) %>% unlist() %>% paste( collapse='' )
    return( c(year=timestamp[1], month=timestamp[2], date=timestamp[3], post=post, comment=comment ) )
}
raw_data <- sapply( urls, get_text )
data.frame(
    year=as.numeric(raw_data[1,]),
    month=as.character(raw_data[2,]),
    date=as.numeric(raw_data[3,]),
    post=as.character(raw_data[4,]),
    comment=as.character(raw_data[5,])
    ) %>% 
    write.csv( file=output_path, row.names=F )

    
