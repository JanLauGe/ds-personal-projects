library(tidyverse)
library(magrittr)
library(stringr)

# load raw data
col_names <- c('chin_simple', 'chin_traditional', 'pinyin', 'english')
df <- read_csv('~/local/personal/triolingo/data/vocab_raw.csv') %>%
  select(-X1, -X6, -X7) %>%
  set_colnames(col_names)

# detect rows that contain data vs header rows
df <- mutate(df,
  topic = is.na(chin_traditional) & is.na(pinyin) & is.na(english) &
    !str_detect(chin_simple, pattern = 'Lesson') &
    !str_detect(chin_simple, pattern = '^\n'),
  lesson = is.na(chin_traditional) & is.na(pinyin) & is.na(english) &
    str_detect(chin_simple, pattern = 'Lesson'),
  header = str_detect(chin_simple, pattern = 'Simplified Chinese'),
  linebreak = str_detect(chin_simple, pattern = '^\n'))

# retain topics and lessons in separate column
df <- mutate(df,
  topic = case_when(topic ~ chin_simple, TRUE ~ NA_character_),
  lesson = case_when(lesson ~ chin_simple, TRUE ~ NA_character_)) %>%
  fill(topic) %>% 
  fill(lesson) %>%
  filter(!header, !linebreak, !is.na(english)) %>%
  select(topic, lesson, chin_simple, chin_traditional, pinyin, english)
  
write_csv(df, '~/local/personal/triolingo/data/vocab_processed.csv')

