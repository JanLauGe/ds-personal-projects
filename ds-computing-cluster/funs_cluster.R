

#' This function sets the aws credentials for the current R session
#' using the aws cli config file (~/.aws)
aws_config <- function() {
  # get local aws credentials
  aws_credentials <- aws.signature::locate_credentials()
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = aws_credentials$key,
    "AWS_SECRET_ACCESS_KEY" = aws_credentials$secret,
    "AWS_DEFAULT_REGION" = "eu-west-2"
  )
}


#' This function generates a random password
#' that can be used as a temporary password during the current session.
#' It helps with avoiding hard-coded passwords.
generate_password <- function(n = 20) {
  # letters
  letters %>% 
    # and numbers
    c(0:9) %>% 
    # randomly combined
    sample(size = n, replace = TRUE) %>% 
    str_c(collapse = '')
}


#' This function spins up a fresh AWS EC2 instance
#' and adds the current local ip to a new permissions group
launch_ec2_instance <- function(
  ami,
  ec2_type = "t2.micro"
) {
  # datetime stamp
  datetime <- Sys.time() %>% str_to_lower() %>% str_replace_all("[-:\\s]", "_")
  # use existing ip address or create a new one
  ips <- describe_ips()
  if (!length(ips)) {
    ips[[1L]] <- allocate_ip("vpc")
  }
  # Check your VPC and Security Group settings
  aws_subnet <- describe_subnets()[[1]]
  aws_sgroup <- try(create_sgroup(
    name = paste0("tmp_ec2_sg_localip", datetime),
    description = paste0("Allow my current IP"),
    vpc = aws_subnets[[1L]]))[[1]]
  
  # Launch the instance using appropriate settings
  aws_instance <- run_instances(
    image = ami, 
    type = ec2_type,
    subnet = aws_subnet, 
    sgroup = aws_sgroup)
  Sys.sleep(20L) # wait for instance to boot
  
  # associate IP address with instance
  instance_ip <- get_instance_public_ip(aws_instance)
  if (is.na(instance_ip)) {
    instance_ip <- associate_ip(aws_instance, ips[[1L]])$publicIp
  }
  # authorize access from this IP
  try(authorize_ingress(aws_sgroup))
  try(authorize_egress(aws_sgroup))
  
  # write and return output list
  ec2info <- list(
    "security_group" = aws_sgroup,
    "ips" = ips[[1L]],
    "instance" = aws_instance
  )
  return(ec2info)
}


#' This function takes a freshly spun up instance object from
#' aws.ec2 and waits until it sees the status switched to running.
wait_for_ec2_instance <- function(aws_instance) {
  instance <- aws.ec2::describe_instances(aws_instance)
  ec2_name <- instance[[1]][["instancesSet"]][[1]][["instanceId"]]
  state <- instance[[1]][["instancesSet"]][[1]][["instanceState"]][["name"]]
  message("current state of ", ec2_name, ": ", state)
  # wait until booted
  while (state != "running") {
    message("waiting...")
    Sys.sleep(5)
    # get instance state from description
    instance <- aws.ec2::describe_instances(aws_instance)
    state <- instance[[1]][["instancesSet"]][[1]][["instanceState"]][["name"]]
  }
  message("instance running!")
  return(instance)
}


#' This function expects an input as generated by `launch_ec2_instance`.
#' It shuts down and terminates the given instances and revokes the temporary
#' security groups that were created.
terminate_and_cleanup <- function(ec2info) {
  
  # stop and terminate the instance
  stop_instances(ec2info[["instance"]])
  terminate_instances(ec2info[["instance"]])
  # revoke access from this IP to security group
  try(revoke_ingress(ec2info[["security_group"]]))
  try(revoke_egress(ec2info[["security_group"]]))
  # release IP address
  release_ip(ec2info[["ips"]])
}







launch_ec2_node <- function(
  ami,
  ec2_type,
  aws_subnet,
  aws_sgroup
) {
  # Launch the instance using appropriate settings
  aws_instance <- run_instances(
    image = ami, 
    type = ec2_type,
    subnet = aws_subnet, 
    sgroup = aws_sgroup)
  Sys.sleep(20L) # wait for instance to boot
  
  # associate IP address with instance
  instance_ip <- get_instance_public_ip(aws_instance)
  if (is.na(instance_ip)) {
    instance_ip <- associate_ip(aws_instance, ips[[1L]])$publicIp
  }
  # authorize access from this IP
  try(authorize_ingress(aws_sgroup))
  try(authorize_egress(aws_sgroup))
  return(aws_instance)
}


#' Function to start nodes in parallel
start_cluster <- function(
  n_nodes,
  ami,
  ec2_type = "t2.micro"
) {
  # datetime stamp
  datetime <- Sys.time() %>% str_to_lower() %>% str_replace_all("[-:\\s]", "_")
  # use existing ip address or create a new one
  ips <- describe_ips()
  if (!length(ips)) {
    ips[[1L]] <- allocate_ip("vpc")
  }
  # Check your VPC and Security Group settings
  aws_subnets <- describe_subnets()
  aws_sgroup <- try(create_sgroup(
    name = paste0("tmp_ec2_sg_localip", datetime),
    description = paste0("Allow my current IP"),
    vpc = aws_subnets[[1L]]))[[1]]
  aws_subnet <- aws_subnets[[1]]
  
  plan(multisession)
  # actually start nodes and return dns info
  cluster <- map(
    .x = rep(ami, times = n_nodes),
    .f = ~ future(
      launch_ec2_node(
        ami = .x,
        ec2_type = ec2_type,
        aws_subnet = aws_subnet,
        aws_sgroup = aws_sgroup
      )
    )
  )
  return(cluster)
}


#' Function for planning use of AWS EC2 cluster
#'
#' good future readmes:
#' - https://andburch.github.io/remoteR/#fnref:1
#' - https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
#' - furrr: https://davisvaughan.github.io/furrr/
use_cluster <- function(
  cluster,
  username
) {
  # get instance info
  cluster_nodes <- map(
    .x = cluster,
    ~ values(.x) %>%
      pluck("instance") %>%
      wait_for_ec2_instance())
  # get dns of instances spun up in the background
  cluster_node_names <- map_chr(
    .x = cluster_nodes,
    .f = ~ .x[[1]]$instancesSet[[1]]$dnsName %>%
      as.character() %>%
      str_c(username, "@", .))
  # prepare to fork out processes to workers
  plan(remote, workers = cluster_node_names)
  return(cluster_node_names)
}


#' Function to copy large data file to each node in the cluster.
#' This avoids having to transfer the file for each furrr operation
copy_to_node <- function(
  ...,
  data,
  type = "scale"
) {
  filename <- paste0("data_", type, ".rds")
  write_rds(x = data, path = filename)
  return(file.exists(filename))
}


#' Function to terminate all worker nodes
stop_cluster <- function(
  worker_names
) {
  for (worker in worker_names) {
    # # ssh in
    # session <- ssh_connect(
    #   host = worker,
    #   verbose = 2)
    # # install furrr
    # ssh_exec_wait(
    #   session = session,
    #   command = "shutdown")
    terminate_and_cleanup(worker)
  }
}

