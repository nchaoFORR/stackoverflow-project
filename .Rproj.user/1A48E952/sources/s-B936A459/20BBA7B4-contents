source("scraping-starter.R")

library(lubridate)

# grab a bunch of data

# 3 pages per month

### function currently takes unix timestamp

# create table of first days of month

date_df <- tibble(
  
  starts = seq(ymd('2018-01-01'),ymd('2019-03-01'), by = 'months'),
  ends = starts
  
) 
# this will convert last day to real last day
day(date_df$ends) <- days_in_month(date_df$starts)

# convert to unix epoch
date_df <- 
  date_df %>% 
  mutate_all(~{
    as.numeric(as.POSIXct(as.character(.)))
  })


### Read in the data

all_list <- map2(date_df$starts, date_df$ends, function(start, end){
  
  Sys.sleep(0.2)
  message(paste0('grabbing ', start))
  gen_request_df(from = start, to = end, page = 1) #%>% 
    #select(tags, view_count, answer_count, score, creation_date, title, body)
  
  
})


### Cleanup into a tibble

# this function will extract data from all_list object,
# then return it in tidy row form.

create_row <- function(item) {
  
  # item = all_list[[1]]
  
  # one-hot-encode tags (currently a list of character vectors)
  tibble(
    
    question_id = item$question_id,
    
    top_answer_id = item$accepted_answer_id,
    
    owner_id = item$owner$user_id,
    
    owner_name = item$owner$display_name,
    
    owner_reputation = item$owner$reputation,
    
    views = item$view_count,
    
    answer_count = item$answer_count,
    
    score = item$score,
    
    date = item$creation_date,
    
    title = item$title,
    
    body = item$body,
    
    tag =
      item$tags %>% 
      map(str_c, collapse = " ") %>% 
      unlist()
    
  )
  
}

full_df <- map_dfr(all_list, create_row)

# sanity check

full_df$question_id %>% unique %>% length

# yayy

