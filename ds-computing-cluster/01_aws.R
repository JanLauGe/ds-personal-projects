
library(tidyverse)
library(aws.ec2)
library(ssh)
library(remoter)
library(future)
library(furrr)
library(randomForest)
source("funs_ec2.R")

# get credentials
aws_access <- aws.signature::locate_credentials(verbose = TRUE)
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = aws_access$key,
  "AWS_SECRET_ACCESS_KEY" = aws_access$secret,
  "AWS_DEFAULT_REGION" = "eu-west-2"
)
# set variables
aws_ami <- "ami-0945d5d8a4d42470b"
aws_sg <- "sg-0d9e246f1d3f2e880"
pwd <- generate_pwd(length = 20)
n_nodes <- 3

# start headnode
headnode <- start_ec2_instance(
  aws_ami = aws_ami,
  aws_type = "c5.large",
  aws_sg = aws_sg
)
headnode_dns <- get_ec2_dns(headnode)

# start cluster
plan(multicore)
workers <- future_map(
  .x = rep(aws_ami, times = n_nodes),
  .f = ~ start_ec2_instance(
    aws_ami = .x,
    aws_type = "c5.large",
    aws_sg = aws_sg) %>%
    values())
workers_dns <- map_chr(
  .x = workers,
  .f = get_ec2_dns)


headnode_dns <- "ec2-3-8-3-178.eu-west-2.compute.amazonaws.com"
workers_dns <- c(
  "ec2-35-176-118-146.eu-west-2.compute.amazonaws.com",
  "ec2-52-56-190-89.eu-west-2.compute.amazonaws.com",
  "ec2-18-130-22-89.eu-west-2.compute.amazonaws.com")

# Launch remoter and connect
connect_remoter(headnode_dns, pwd)


# on remote ====================================================================

# need to (install and) load libraries again
library(tidyverse)
library(future)
library(furrr)
library(nycflights13)
library(randomForest)

# copy workers_dns vector 
# and model function
# to the head node
c2s(workers_dns, "workers_dns")
c2s(train_punctuality_model, "train_punctuality_model")
# plan parallel execution with workers
plan(cluster, workers = workers_dns)

# run modeling
punctuality_models <- future_map(
  .x = airlines[["carrier"]][1:3],
  .f = train_punctuality_model,
  ntree = 10)

# copy results back to local
s2c(punctuality_models, "punctuality_models")

# disconnect remoter
exit()

# shuts down ec2 instance
terminate_instances(headnode)
terminate_instances(workers)




