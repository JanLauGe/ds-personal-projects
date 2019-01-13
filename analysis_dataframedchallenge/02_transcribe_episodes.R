
library(tidyverse)
library(jsonlite)
library(aws.iam)
library(aws.s3)
library(aws.transcribe)

# I've downloaded the episode audios
episode_files <- list.files(path = "~/Desktop/DataFramed/", full.names = T)
episodes <- basename(episode_files)

# write episodes to s3
bucket <- "janlauge"
map2(
  .x = episode_files,
  .y = episodes,
  .f = put_object,
  bucket = bucket)

put_object(
  file = episode_files[[1]], 
  object = episodes[[1]], 
  bucket = "janlauge")

# create a "folder" in a bucket
# put_folder("example", bucket = "janlauge")

# use AWS transcribe to generate text from speech
bla <- start_transcription(
  name = "transcribe_e31",
  url = "s3://janlauge/e31.mp3",
  format = "mp3",
  MaxSpeakerLabels = 10,
  ShowSpeakerLabels = TRUE)


x <- fromJSON("~/Desktop/transcribed_e49.json")

x$results$transcripts
