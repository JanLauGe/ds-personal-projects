
library(aws.ec2)
library(ssh)
library(remoter)
library(tidyverse)

aws_access <- aws.signature::locate_credentials()
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = aws_access$key,
  "AWS_SECRET_ACCESS_KEY" = aws_access$secret,
  "AWS_DEFAULT_REGION" = "eu-west-2"
)

# set parameters
aws_ami <- "ami-06485bfe40a86470d"
aws_describe <- describe_images(aws_ami)
aws_type <- "t2.micro"

ec2inst <- run_instances(
  image = aws_ami, 
  type = aws_type)

# wait for boot, then refresh description
Sys.sleep(10)
ec2inst <- describe_instances(ec2inst)

# get IP address of the instance
ec2inst_ip <- get_instance_public_ip(ec2inst)

# CMD string to start remoter::server on instance
r_cmd_start_remoter <- str_c(
  "sudo Rscript -e ",
  "'remoter::server(",
  "port = 55555, ",
  "showmsg = TRUE)'",
  collapse = "") %>%
  str_replace("%pwd", str_c('"', random_tmp_password, '"'))

# connect and execute
plan(multicore)
x <- future(
  ssh_exec_wait(
    session = con, 
    command = r_cmd_start_remoter))


# TODO check this out
lobstr::ast(aws_instance)