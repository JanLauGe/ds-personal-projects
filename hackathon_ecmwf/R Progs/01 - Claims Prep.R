#01 - Prep
#Claims data

#Setup
  library(dplyr)
  library(reshape2)
  library(lubridate)
  library(plotly)

  wd <- paste0(getwd(), "/")
  input <- paste0(wd, "Data/")
  output <- paste0(wd, "R Output/")
  
#Import
  #Bus Claims
  filename <- paste0(input, "busClaims.csv")
  claims <- read.csv(filename, stringsAsFactors = FALSE)
  glimpse(claims)

#Data Processing
  #Dates
  claims$accidentDate <- as.Date(dmy(claims$Accident.Date))
  claims$accidentMonthYear <- as.Date(ISOdate(year(claims$accidentDate), month(claims$accidentDate), 1))
  claims$accidentMonth <- month(claims$accidentDate)
  claims$accidentYear <- year(claims$accidentDate)
  
  #Strings in different cases  
  claims$Fault.Descrip <- tolower(claims$Fault.Descrip)

  #Parse route
  claims$routeNo <- substr(claims$Route,1,4)

#Analysis

#Raw Dataset - no cleanup
  #Seasonality
  claimsYear <- claims %>%
    filter(Accident.Descrip == "Collision with") %>%
    group_by(accidentYear) %>%
    summarise(claims=n())
  
  graphData <- claimsYear
  plot_ly(graphData, x=accidentYear, y=claims, type="bar")

  filename <- paste0(output, "claimsYear.csv")
  write.csv(claimsYear, filename)
    
  claimsMonthYear <- claims %>%
    group_by(accidentMonthYear, accidentMonth, accidentYear) %>%
    summarise(claims=n()) %>%
    ungroup() %>%
    group_by(accidentYear) %>%
    mutate(avgMonthlyClaims=mean(claims),
           indexMonthly=claims/avgMonthlyClaims) 

  graphData <- claimsMonthYear
  plot_ly(graphData, x=accidentMonthYear, y=claims, type="scatter")
  
  filename <- paste0(output, "claimsMonthYear.csv")
  write.csv(claimsMonthYear, filename)
  
  
  graphData <- claimsMonthYear
  plot_ly(graphData, x=accidentMonth, y=claims, group=accidentYear, type="scatter")
  
  graphData <- claimsMonthYear
  plot_ly(graphData, x=accidentMonth, y=indexMonthly, group=accidentYear, type="scatter")
  
  # claimsDaily <- claims %>%
  #   group_by(accidentDate) %>%
  #   summarise(claims=n())
  # 
  # graphData <- claimsDaily %>%
  #   filter(year(accidentDate) >= 2016)
  # plot_ly(graphData, x=accidentDate, y=claims, type="scatter")
  
#Data from Jan 2015
  
  #claim type
  claimType <- claims %>%
    filter(accidentYear >= 2015) %>%
    group_by(Accident.Descrip) %>%
    summarise(nClaims=n()) %>%
    ungroup() %>%
    arrange(-nClaims) %>%
    mutate(pctClaims=nClaims/sum(nClaims),
           cumPctClaims=cumsum(pctClaims))
  
  graphData <- claimType %>%
    arrange(nClaims)
  plot_ly(graphData, y=Accident.Descrip, x=pctClaims, 
          type="bar", orientation="h") %>%
    layout(yaxis=list(title=""),
           xaxis=list(title="% Claims"),
           margins=list(l=100))

  filename <- paste0(output, "claimType.csv")
  write.csv(claimType, filename)
    
  #Collision faults
  faultType <- claims %>%
    filter(accidentYear >= 2015) %>%
    filter(Accident.Descrip == "Collision with") %>%
    group_by(Fault.Descrip) %>%
    summarise(nClaims=n()) %>%
    ungroup() %>%
    arrange(-nClaims) %>%
    mutate(pctClaims=nClaims/sum(nClaims),
           cumPctClaims=cumsum(pctClaims))
  
  graphData <- faultType %>%
    arrange(nClaims)
  plot_ly(graphData, y=Fault.Descrip, x=pctClaims, 
          type="bar", orientation="h") %>%
    layout(yaxis=list(title=""),
           xaxis=list(title="% Claims"),
           margins=list(l=150))

  filename <- paste0(output, "faultType.csv")
  write.csv(faultType, filename)
  
  
  #Collision description
  collisionType <- claims %>%
    filter(accidentYear >= 2015) %>%
    filter(Accident.Descrip == "Collision with") %>%
    group_by(Collision.Description) %>%
    summarise(nClaims=n()) %>%
    ungroup() %>%
    arrange(-nClaims) %>%
    mutate(pctClaims=nClaims/sum(nClaims),
           cumPctClaims=cumsum(pctClaims))
  
  graphData <- collisionType %>%
    arrange(nClaims)
  plot_ly(graphData, y=Collision.Description, x=pctClaims, 
          type="bar", orientation="h") %>%
    layout(yaxis=list(title=""),
           xaxis=list(title="% Collisions"),
           margins=list(l=150))
  
  filename <- paste0(output, "collisionType.csv")
  write.csv(collisionType, filename)

  #Collision description x fault
  collisionTypeFault <- claims %>%
    filter(accidentYear >= 2015) %>%
    filter(Accident.Descrip == "Collision with") %>%
    group_by(Collision.Description, Fault.Descrip) %>%
    summarise(nClaims=n()) 
  
  graphData <- collisionTypeFault
  plot_ly(graphData, x=nClaims, y=Collision.Description, color=Fault.Descrip, type="bar",
          orientation="h") %>%
    layout(barmode="stack",
           hovermode="closest",
           margins=list(l=150),
           yaxis=list(title=""),
           xaxis=list(title="number of claims"))
  
  filename <- paste0(output, "collisionTypeFault.csv")
  write.csv(collisionTypeFault, filename)
  
  #routes
  routeRecent <- claims %>%
    filter(accidentYear >= 2015) %>%
    group_by(routeNo) %>%
    summarise(nClaims=n()) %>%
    arrange(-nClaims) %>%
    ungroup() %>%
    mutate(rank=row_number(),
           cumPctRoutes=rank/max(rank),
           pctClaims=nClaims/sum(nClaims),
           cumPctClaims=cumsum(pctClaims))

  #sum(routeRecent$nClaims)
    
  graphData <- routeRecent
  plot_ly(graphData, x=cumPctRoutes, y=cumPctClaims, mode="markers")
  
  graphData <- routeRecent  %>%
    filter(rank <= 20) %>%
    mutate(bus=paste0("R", routeNo))
  
  plot_ly(graphData, x=bus, y=nClaims, type="bar") %>%
    layout(xaxis=list(title=""))
  
  filename <- paste0(output, "routeRecent.csv")
  write.csv(routeRecent, filename)
  
  topRoutes <- routeRecent  %>%
    filter(rank <= 20) %>%
    select(routeNo)
  
  #route x fault
  glimpse(claims)
  routeFault <- claims %>%
    filter(accidentYear >= 2015) %>%
    filter(routeNo %in% topRoutes$routeNo) %>%
    group_by(routeNo, Fault.Descrip) %>%
    summarise(nClaims=n())
    
  sum(routeFault$nClaims)
  
  graphData <- routeFault %>%
    mutate(bus=paste0("R", routeNo))
  plot_ly(graphData, x=bus, y=nClaims, color=Fault.Descrip, type="bar") %>%
    layout(barmode="stack",
           xaxis=list(title=""),
           yaxis=list(title="Claims from Jan 2015"))
  
  filename <- paste0(output, "routeFault.csv")
  write.csv(routeFault, filename)
  