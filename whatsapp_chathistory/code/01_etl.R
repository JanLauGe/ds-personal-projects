library(here)
library(tidyverse)
library(lubridate)
library(tidytext)

# get data
df_msgs <- rbind(
  read_table(
    file = here::here('data/02_raw_mel/WhatsApp Chat with Melissa Ng UK.txt'), 
    col_names = 'msgs', 
    col_types = 'c'),
  read_table(
    file = here::here('data/02_raw_mel/WhatsApp Chat with Melissa Ng US.txt'), 
    col_names = 'msgs', 
    col_types = 'c'))

# transform
msgs_transform <- function(msgs) {

  msg_header <- '\\d{1,2}\\/\\d{1,2}\\/\\d{1,2}, \\d{2}:\\d{2} - (Laurens Geffert|Melissa Ng):'
  
  msgs <- msgs %>%
    mutate(
      # find messages that are split across rows
      msg_start = msgs %>%
        str_detect(pattern = msg_header) %>%
        replace_na(FALSE),
      # create message id to group by
      msg_id = msg_start %>% 
        as.numeric() %>% 
        cumsum()) %>%
    # aggregate lines belonging to the same message
    group_by(msg_id) %>% 
    nest(msgs)
  
  # combine individual lines into single message
  msgs$msg <- map_chr(
    .x = msgs %>% pull(data),
    .f = function(x) {x %>% unlist %>% paste0(collapse = '\n')})
  
  msgs <- msgs %>%
    select(msg_id, msg) %>%
    # separate date stamp
    separate(col = msg, into = c('date', 'rest'), sep = ', ', extra = 'merge') %>%
    # separate time stamp
    separate(col = rest, into = c('time', 'rest'), sep = ' - ', extra = 'merge') %>%
    # separate sender
    separate(col = rest, into = c('sender', 'text'), sep = ': ', extra = 'merge') %>%
    # change column types
    transmute(
      msg_id = msg_id,
      time = paste(date, time, sep = ' '),
      sender = as_factor(sender),
      text = text)
  
  # make into datetime format
  msgs$time <- as_datetime(msgs$time, format = '%m/%d/%y %H:%M', tz = 'GMT') %>%
    as.POSIXct()
  
  return(msgs)
}

df_msgs <- msgs_transform(df_msgs)

# count 
df_msgs %>%
  pull(text) %>%
  str_detect(pattern = 'I love you') %>%
  as.numeric() %>% sum()

# plot message frequency
df_msgs %>% ggplot(aes(x = time)) + geom_histogram(bins = 200)

# message time of day
df_msgs %>% mutate(daytime = hour(time)) %>%
  ggplot(aes(x = daytime)) + geom_histogram(bins = 24)

# message day of week
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
df_msgs %>% mutate(Weekday = weekdays(time) %>% factor(levels = weekdays)) %>%
  ggplot(aes(x = Weekday)) + geom_histogram(stat = 'count')



# get vector of words
wordlist <- df_msgs %>%
  filter(sender == 'Melissa Ng') %>%
  pull(text) %>%
  paste(collapse = ' ') %>%
  str_split(pattern = ' ') %>%
  unlist()

x <- wordlist %>% table %>% sort(decreasing = TRUE)



data(stop_words)
stop_words <- stop_words %>%
  rbind(data_frame(
    word = c('media', 'omitted'),
    lexicon = c('WhatsApp', 'WhatsApp')
  ))

df_words <- df_msgs %>%
  # tokenise dataset
  unnest_tokens(output = word, input = text) %>%
  # remove common words
  anti_join(stop_words, by = 'word')

# most common words
df_words %>% count(word, sort = TRUE) 


# compare by sender
frequency <- df_words %>%
  select(sender, word) %>%
  # exclude special characters
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(sender, word) %>%
  filter(n > 3) %>%
  group_by(sender) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(sender, proportion) %>% 
  gather(sender, proportion, `Laurens Geffert`:`Melissa Ng`)

library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, 
           y = `Laurens Geffert`, 
           color = abs(`Laurens Geffert` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

