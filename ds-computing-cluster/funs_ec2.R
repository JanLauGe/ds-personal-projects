

#' This function generates a random password
#' that can be used as a temporary password during the current session.
#' It helps with avoiding hard-coded passwords.
generate_pwd <- function(length = 20) {
  # letters
  letters %>% 
    # and numbers
    c(0:9) %>% 
    # randomly combined
    sample(size = length, replace = TRUE) %>% 
    str_c(collapse = '')
}


#' start an ec2 instance and wait for it to boot, 
#' then return the aws.ec2 opject
start_ec2_instance <- function(
  aws_ami,
  aws_type,
  aws_sg
) {
  # start instance
  ec2_instance <- run_instances(
    image = aws_ami,
    type = aws_type,
    sgroup = aws_sg)
  # wait for booting
  Sys.sleep(5)
  ec2_info <- describe_instances(ec2_instance)
  Sys.sleep(5)
  ec2_status <- "checking..."
  while (ec2_status != "initializing") {
    Sys.sleep(5)
    ec2_info <- describe_instances(ec2_instance)
    ec2_status <- instance_status(ec2_instance) %>%
      pluck("item") %>%
      pluck("instanceStatus") %>%
      pluck("status") %>%
      pluck(1)
    if (is.null(ec2_status)) {
      ec2_status <- "unknown"
    }
  }
  return(ec2_instance)
}


#' get dns of an aws.ec2 object
get_ec2_dns <- function(
  ec2_instance
) {
  # get IP address of the instance
  ec2_dns <- ec2_instance %>%
    describe_instances() %>%
    pluck(1) %>% pluck("instancesSet") %>%
    pluck(1) %>% pluck("networkInterfaceSet") %>%
    pluck("privateIpAddressesSet") %>%
    pluck("association") %>%
    pluck("publicDnsName")
  return(ec2_dns)
}


#' launch remoter on a remote server via ssh,
#' then connect to it from local
connect_remoter <- function(ec2_dns, pwd) {
  # ssh connection
  username <- system2("whoami", stdout = TRUE)
  con <- ssh_connect(host = paste(username, ec2_dns, sep = "@"))
  # CMD string to start remoter::server on instance
  r_cmd_start_remoter <- str_c(
    "Rscript -e ",
    "'remoter::server(",
    "port = 55555, ",
    "password = %pwd, ",
    "showmsg = TRUE)'",
    collapse = "") %>%
    str_replace("%pwd", str_c('"', pwd, '"'))
  # trick using future for starting remote::server
  plan(multicore)
  x <- future(
    ssh_exec_wait(
      session = con, 
      command = r_cmd_start_remoter))
  # connect via remoter
  remoter::client(
    addr = ec2_dns, 
    port = 55555,
    password = pwd,
    prompt = "remote")
}


#' run toy analysis on the nycflights13 dataset
train_punctuality_model <- function(
  airline_tag,
  ntree = 50
) {
  # load library (and data) locally
  library(nycflights13)
  # run preprocessing pipeline
  flights_airline <- flights %>%
    # select relevant flights
    filter(carrier == airline_tag) %>%
    # exclude flight time variables
    select(-arr_time, -air_time, -dep_time, -dep_delay) %>%
    # exclude high-cardinality variables
    select(-dest, -tailnum) %>%
    # transform variables for model fitting
    mutate_if(.p = is.character, .f = as.factor) %>%
    drop_na()
  
  X <- select(flights_airline, -arr_delay)
  y <- pluck(flights_airline, "arr_delay") %>% replace_na(0)
  
  rf_model <- randomForest(x = X, y = y, ntree = ntree)
  return(rf_model)
}