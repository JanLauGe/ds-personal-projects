#In this approach I applied some basic data cleaning, munging, and feature engineering. The resulting data set was used in a glmnet model fine-tuned using caret. The result performed well on the leader board, scoring 157th place and an AUC of 0.81818, on par with other entries up to place 81. I would appreciate any comments on the approach in general and the code in particular. Other than that: Bring on a real challenge now :)
library(plyr)
library(dplyr)
library(caret)
library(pROC)
set.seed(1717)

d.init.model <- read.csv("../input/train.csv", 
                         colClasses = c("integer","factor","factor","character","factor","numeric","integer","integer","character","numeric","character","factor"),
                         na.strings = c("NA", ""))
d.init.predict <- read.csv("../input/test.csv", 
                           colClasses = c("integer","factor","character","factor","numeric","integer","integer","character","numeric","character","factor"),
                           na.strings = c("NA", ""))

#I combined the two datasets into one to apply all changes to both sets. With the function GetNames I split the name strings into title, first name, and last name. Titles were used to infer missing age values, Last names were used to group families together, assuming that families are likely to live or die together. From the cabin number I extracted a deck where available. Finally, I added life boat priority for women and children first.
# DATA MUNGING ----
# bind two datasets together to format them the same way
d.all <- cbind(d.init.predict, "Survived" = factor(NA, levels = c(0,1))) %>% rbind(d.init.model)

# Re-format data
GetNames <- function(full.names){
  names.split <- strsplit(full.names, split = ", ")
  names.last <- lapply(names.split, FUN = function(x) {x[[1]]}) %>% unlist()
  names.rest <- lapply(names.split, FUN = function(x) {x[[2]]}) %>% unlist()
  names.split2 <- strsplit(names.rest, split = "\\.")
  names.title <- lapply(names.split2, FUN = function(x) {x[[1]]}) %>% unlist()
  names.title <- revalue(as.factor(names.title), 
                            replace = c("the Countess"="Posh", "Sir"="Posh", "Lady"="Posh",
                                        "Capt"="Posh", "Col"="Posh", "Major"="Posh",
                                        "Jonkheer"="Posh", "Don"="Posh","Dona"="Posh",
                                        "Dr"="Posh", "Rev"="Posh",
                                        "Ms"="Mrs", "Mlle"="Miss", "Mme"="Mrs"))
  names.first <- lapply(names.split2, FUN = function(x) {ifelse(length(x) > 2, paste(x[[2]], x[[3]]), x[[2]])}) %>% unlist()
  split.names <- data.frame("Title" = names.title, "First" = names.first, "Last" = names.last)
  return(split.names)
}
d.all <- cbind(d.all, GetNames(d.all$Name))
attach(d.all)

# fill missing age values with inferred information
d.all[is.na(Embarked),"Embarked"] <- "S"
d.all$Age <- ifelse(!is.na(Age), Age,
       ifelse(is.na(Age) & Title == "Posh", mean(Age[Title=="Posh"], na.rm = T),
              ifelse(is.na(Age) & Title == "Mr", mean(Age[Title=="Mr"], na.rm = T),
                     ifelse(is.na(Age) & Title == "Mrs", mean(Age[Title=="Mrs"], na.rm = T),
                            ifelse(is.na(Age) & Title == "Miss", mean(Age[Title=="Miss"], na.rm = T),
                                   ifelse(is.na(Age) & Title == "Master", mean(Age[Title=="Master"], na.rm = T), NA))))))

# get deck where available
GetDeck <- function(cabin){
  deck <- rep(NA, length(cabin))
  for(letter in c("A","B","C","D","E","F","G")){
    this.deck <- llply(.data = cabin, .fun = function(x) {grep(pattern = letter, x = x)})
    this.deck[is.na(this.deck==0)] <- 0
    deck[this.deck != 0] <- letter
  }
  deck[is.na(deck)] <- "Unknown"
  return(deck)
}
d.all$Deck <- as.factor(GetDeck(as.character(Cabin)))

# convert next of kin info into binary variable
d.all$Family <- as.factor(ifelse(SibSp > 0 | Parch > 0, 'yes', 'no'))

# convert ticket classes into ordered factor
Pclass <- ordered(Pclass)

# add life boat priority
d.all$boatfirst <- ifelse(Sex == 'female' | Age < 18, 1, 0)

# drop unneccessary columns and split back into model and predict set
d.all <- d.all[,c("Survived","Title","Last","Sex","Age","Family","SibSp","Parch","Pclass","Embarked")]
d.model <- d.all[!is.na(Survived),c("Survived","Title","Last","Sex","Age","Family","SibSp","Parch","Pclass","Embarked")]
d.predict <- d.all[is.na(Survived),c("Title","Last","Sex","Age","Family","SibSp","Parch","Pclass","Embarked")]
detach(d.all)

head(d.all)
summary(d.all)

#I ran a basic glmnet with regularisation. Alpha and lambda were optimised using the caret package. I evaluated the performance with each tuning parameter using three-fold crossvalidation.
# MODELLING ----
# create training and test data partition
data <- d.model %>%
    mutate(Survived = as.factor(Survived)) %>%
    mutate(Survived = fct_recode(Survived, 'survived' = '0', 'died' = '1'))
    
# build a glmnet model
car.train.grid <- expand.grid(
    alpha = c(0.1, 0.2, 0.3, 0.5, 0.7, 1), 
    lambda = c(0.00003, 0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1))
car.train.control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    classProbs = TRUE,
    summaryFunction = twoClassSummary)
car.train <- train(
    Survived ~ 1 + .,
    data = data,
    method = "glmnet",
    family = "binomial",
    metric = "ROC",
    tuneGrid = car.train.grid,
    trControl = car.train.control)

car.train
plot(car.train)

#Finally, I used the best fitted model for a prediction on the test dataset.
# PREDICTION ----
out <- predict(car.train, d.predict)
out <- cbind("PassengerId" = d.init.predict$PassengerId, "Survived" = as.numeric(out)-1)
write.csv(out, file = "predict.csv", row.names=F, quote=FALSE)
