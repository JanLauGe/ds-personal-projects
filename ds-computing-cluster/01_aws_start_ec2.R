
#devtools::install_github("cloudyr/aws.ec2")
library(aws.ec2)
library(future)
library(ssh)
library(remoter)
library(tidyverse)


# spin up instance -------------------------------------------------------------

# get local aws credentials
aws_credentials <- aws.signature::locate_credentials()
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = aws_credentials$key,
  "AWS_SECRET_ACCESS_KEY" = aws_credentials$secret,
  "AWS_DEFAULT_REGION" = "eu-west-2"
)

# set parameters
aws_ami <- "ami-06485bfe40a86470d"
aws_type <- "t2.micro"

# launch machine and retain both instance and securitygroup info
ec2info <- launch_ec2_instance(aws_ami, aws_type)
ec2instance <- ec2info[["instance"]]

# wait for machine to start
ec2instance <- wait_for_ec2_instance(ec2instance)
ec2ip <- get_instance_public_ip(ec2instance)


# connect via ssh --------------------------------------------------------------

# ssh connection
username <- system("whoami", intern = TRUE)
con <- ssh_connect(host = paste(username, ec2ip, sep = "@"))

# start remoter::server on instance
random_tmp_password <- generate_password()
r_cmd_start_remoter <- str_c(
  "sudo Rscript -e ",
  "'remoter::server(",
  "port = 55555, ",
  "password = %pwd, ",
  "showmsg = TRUE)'",
  collapse = "") %>%
  str_replace("%pwd", str_c('"', random_tmp_password, '"'))

# connect and execute
plan(multicore)
x <- future(
  ssh_exec_wait(
    session = con, 
    command = r_cmd_start_remoter))

# connect via remoter
remoter::client(
  addr = ec2ip, 
  port = 55555,
  password = random_tmp_password,
  prompt = "remote")

### YOUR CODE COES HERE...
getwd()


# clean up ---------------------------------------------------------------------
# save all your work before doing this!

# disconnect remoter
exit()
ssh_disconnect(con)
# shuts down ec2 instance and deletes temporary security group
terminate_and_cleanup(ec2info)


