library(tidyverse)
library(twitteR) 
library(jsonlite)
library(purrr)

credentials <- jsonlite::read_json(path = "~/.twitter") %>% pluck("twitter")

consumer_key <- credentials %>% pluck("consumer") %>% pluck("API_key")
consumer_secret <- credentials %>% pluck("consumer") %>% pluck("API_secret_key")
access_token <- credentials %>% pluck("access") %>% pluck("access_token")
access_secret <- credentials %>% pluck("access") %>% pluck("access_token_secret")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tw <- searchTwitter('#dataframedchallenge', n = 100)
d = twListToDF(tw)