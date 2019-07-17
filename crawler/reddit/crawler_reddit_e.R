library(dplyr)
library(RedditExtractoR)
setwd( '~/Documents/DSP/Data_Science_Programming/crawler/reddit' )

#str <- "
posts <- reddit_urls(
    search_terms = NA, 
    regex_filter = "", 
    subreddit = NA,
    cn_threshold = 0, 
    page_threshold = 1, 
    sort_by = 'comments',
    wait_time = 2 
)
posts %>% names %>% print
posts$URL %>% print
#"

post <- reddit_content( URL=posts[1, 'URL'] )
post[1,'id'] %>% print
post[1,'comment'] %>% print
post[1,'post_text'] %>% print
