# APSTA-GE 2017
# Jacqueline Gutman
# Project Code
# Last Updated: May 9, 2015

require(pander)
require(plyr)
require(glmnet)
require(ROCR)

folder <- "2012_Fluency_Evolution"
subfolder <- "Documentation - Experimentation help success"
file <- "Alldata join helpgiven and lexview, freq, prev, levels.csv"
loc <- paste(".", folder, subfolder, file, sep = "/")
data <- read.csv(loc)

names(data)
dim(data)
str(data, strict.width = "cut")

# Generate a table describing the features of the raw data
data.describe = ldply(data, .id = "feature", function(x) {
  class = class(x)
  length = length(unique(x))
  length = ifelse(length==2, "binary", length)
  missing = ifelse(sum(is.na(x)) > 0, "yes", "no")
  min = ifelse(class != "factor", min(x), 0)
  max = ifelse(class != "factor", max(x), 0)
  range = ifelse(class != "factor", paste(min, max, sep = " to "), "N/A")
  return (c(class=class, num.unique.values=length, 
            range.of.numeric = range, any.missing=missing))
})
data.describe$description[data.describe$feature == "user_id"] = 
  "unique identifier for each student"
data.describe$description[data.describe$feature == "target_word_number"] = 
  "position of the target word in the sentence"
data.describe$description[data.describe$feature == "target_word"] = 
  "target word to be read"
data.describe$description[data.describe$feature == "latency"] = 
  "hesitation in seconds before student begins reading the word"
data.describe$description[data.describe$feature == "helped"] = 
  "binary indicator of whether the student received help for this word in this trial"
data.describe$description[data.describe$feature == "Reading_Level"] = 
  "reading level of the student"
data.describe$description[data.describe$feature == "Story_Level"] = 
  "level of difficulty of the story (students at higher level get harder stories"
data.describe$description[data.describe$feature == "frequency"] = 
  "measure of frequency of the word in the English language (higher is less frequent)"
data.describe$description[data.describe$feature == "Type"] = 
  "type of hint student received on same word in previous encounter (at an earlier date)"
data.describe$description[data.describe$feature == "PREV"] = 
  "how many times the student encountered the word previously"
set.alignment(default = "left")
pander(data.describe, split.table=Inf)

# Let's begin by defining a function that takes a dataset, a variable to average over, and a
# variable to condition on, and then loops through the dataset, computing the conditional 
# mean for each observation. So for example, we can condition on the student (user_id), and 
# then take the average of their latency variable.
# Importantly, we want to return this conditional mean as a vector in the same shape as the 
# original data, and we need to exclude the current observation from the mean to avoid 
# issues with leakage.

############# GET_MEAN_OTHER FUNCTION DEFINITION #############
get_mean_other <- function(levels.var, mean.var, data) {
  # Add a feature that gives the mean and number of trials over all trials in the data 
  # for that particular value of the levels variable
  
  # TODO update get_mean_other function to accommodate list of levels.var
  # i.e. get mean for each interaction of student and reading level variable
  all_trials <- ldply(levels(data[[levels.var]]), function(id) { #loop over each unique student, word, etc.
    subset <- data[[mean.var]][data[[levels.var]] == id] #all trials for a particular student, word, etc. 
    data.frame(mean_all_trials=mean(subset), num_trials=length(subset)) 
  })
  rownames(all_trials) <- levels(data[[levels.var]]) 
  # returns a data frame with a row for each student, word, etc. and a column for the mean 
  # second column for the number of trials, NOT SAME SHAPE AS ORIGINAL DATA
  
  # We need to make sure we exclude the current value for that particular trial from the mean
  num_excluding_current <- all_trials$num_trials[data[[levels.var]]] - 1
  mean_excluding_current <- (all_trials$mean_all_trials[data[[levels.var]]] * 
                               all_trials$num_trials[data[[levels.var]]] - data[[mean.var]]) / 
    num_excluding_current
  return(list(mean = mean_excluding_current, num = num_excluding_current))
  # returns an object containing two vectors, both same dimension as the original data
  # mean over all trials except current trial
  # may contain NAs if there are no other trials for that value of the student, word, etc.
}

############# GET_MEAN_OTHER_MULTICAT FUNCTION DEFINITION #############
get_mean_other_multicat <- function(levels.var, mean.var, data) {
  # this function is just like get_mean_other but it should work
  # when levels.var is a vector of features we want to condition on
  # also should work when mean.var is a vector of features we want to avg over
  
  x <- split(data[, c(levels.var, mean.var)], data[, levels.var])
  y <- sapply(seq(length(x)), function(i) {dim(x[[i]])[1] > 0})
  x <- x[y] # x is a list of dataframes
  # the length of x is the number of non-empty interactions of the levels variable
  # i.e. we drop any combinations of the levels variables that don't contain data 
  
  # create a dataframe with 1 column for each variable in mean.var
  # containing the mean over all other observations
  # in the same subgroup formed by conditioning on the levels variable
  z <- ldply(names(x), function(cat) {
    index <- rownames(x[[cat]])
    target <- x[[cat]][mean.var]
    means <- ldply(seq(dim(target)[1]), function(x) {colMeans(target[-x,])})
    rownames(means) <- index
    return(means)
    # NA in rows for subcategories with only one entry
  })
  return(z)
}

############# FORMAT_DATA FUNCTION DEFINITION #############
# format_data uses get_mean_other functions defined above
format_data <- function(data) {
  # change all words to lowercase
  data$target_word <- factor(tolower(data$target_word)) 
  # regular expression to find the words that are just numbers
  # could be something like "1st", "14th", etc. but not "first", "zero", etc.
  # there's something easier about reading "1000" than "zero"
  data$is.number <- grepl("[0-9]+",as.character(data$target_word))
  
  # Add a feature that gives the length of the word
  data$word_length <- nchar(as.character(data$target_word))
  data$is.letter <- (data$word_length == 1) & !(data$is.number) # 1 if word = a-z, A-Z
  data$is.letter <- as.numeric(data$is.letter)
  data$is.number <- as.numeric(data$is.number)
  
  # Call the trial a success if there is no latency/hesitation and no help given 
  data$success <- ifelse(data$latency == .01, 1, 0)
  data$success <- ifelse(data$helped == 1, 0, data$success)
  
  # Add a feature that gives the mean latency over all other trials in the data for that student
  latency_by_student <- get_mean_other("user_id", "latency", data)
  data$mean_latency <- latency_by_student$mean
  data$num_student_trials <- latency_by_student$num
  
  # Add a feature that gives the mean success rate over all other trials in the data for that student
  success_by_student <- get_mean_other("user_id", "success", data)
  data$mean_success <- success_by_student$mean
  
  # Add a feature that gives the mean latency and success rate over all other trials for that word
  latency_by_word <- get_mean_other("target_word", "latency", data)
  data$mean_word_latency <- latency_by_word$mean
  data$num_word_trials <- latency_by_word$num
  success_by_word <- get_mean_other("target_word", "success", data)
  data$mean_word_success <- success_by_word$mean
  
  # Add a feature that gives the mean latency and success rate over all other trials
  # for the trials of that student at that story level
  latency_success_by_student_story <- get_mean_other_multicat(levels.var = 
                                                                c("user_id", "Story_Level"), mean.var = c("latency", "success"), data=data)
  data$mean_student_level_latency <- latency_success_by_student_story$latency
  data$mean_student_level_success <- latency_success_by_student_story$success
  
  # Any other features we want to generate here?
  return(data)
}

data.2 <- format_data(data)
names(data.2)
dim(data.2)
str(data.2, strict.width = "cut")

# Let's define a function that takes in a dataset, and creates a new data frame, with a row
# for each unique word in the dataset. For each word, we want this new dataframe to tell us
# how many times the word occurs, the number of different help types observed for that word,
# as well as a binary indicator for each help type, indicating whether that help type was
# observed for that particular word. We will also create a factor variable that gives a label
# to each unique subset of help types-- for example, help class 1 may mean that Autophonics 
# and StartsLike are available for this word, and help class 34 may mean that Autophonics, 
# RhymesWith, and Recue are available for this word. 

############# ADD_POSSIBLE_HELP_TYPES FUNCTION DEFINITION #############
add_possible_help_types <- function(data.2) {
  # create a list of lists, where the outer list entries are the unique words in the data
  # and the inner list gives the actual help types observed for that word
  # this is essentially a ragged array with 1-13 entries in each row
  help_types_by_word <- sapply(levels(data.2$target_word), function(word) 
    sort.int(unique(data.2$Type[data.2$target_word == word])))
  help_type_subsets <- unique(help_types_by_word)
  # letters.and.numbers <- unique(data.2$target_word[
  #     (data.2$is.letter == 1) | (data.2$is.number == 1)])
  # help_types_letters_and_numbers <- sapply(letters.and.numbers, function(word) 
  #     sort.int(unique(data.2$Type[data.2$target_word == word])))
  
  # count the number of times each word is observed
  count_trials_by_word <- ldply(levels(data.2$target_word), function(word)
    count(data.2$target_word[data.2$target_word == word]))
  
  # assign a unique label (we'll just use integers) to each observed combination of 
  # help types available. Returns a dataframe containing all the observed 
  # combinations of help types in the data
  types <- ldply(seq(to=length(help_type_subsets)),
                 function(i) {
                   category = help_type_subsets[[i]]
                   size = length(category)
                   types = sapply(levels(category), function(type) type %in% category)
                   c(index=i, len=size, types) })
  
  # dataframe containing 1 entry for each word in the dataset
  # add columns for the observed count of the word, the label for the subset of available
  # help types it belongs to, the length of the word in characters, and binary indicators
  # for each help type -- was that help type observed for this word or not?
  words <- ldply(seq(to=length(help_types_by_word)),
                 function(i) {
                   word = names(help_types_by_word[i])
                   freq = count_trials_by_word$freq[i]
                   word_help = help_types_by_word[[i]]
                   size = length(word_help)
                   type_words = sapply(levels(word_help), function(type) type %in% word_help)
                   a = TRUE
                   for(i in seq(to=length(levels(word_help)))) {
                     b = types[i+2] == type_words[i]
                     a = a & b}
                   index_type = types$index[a]
                   c(word=word, num_occurrences=freq, help_class=index_type, 
                     num_help_types = size, type_words)
                 })
  # convert chr to boolean to numeric : 0 <- "FALSE" and 1 <- "TRUE"
  types = colnames(words)[5:length(words)]
  for(type in types) {
    words[[type]] <- as.numeric(as.logical(words[[type]]))
  }
  # convert numbers back to numeric (all numbers got cast to chr by the c function)
  words$num_occurrences <- as.numeric(words$num_occurrences)
  words$help_class <- as.numeric(words$help_class)
  words$num_help_types <- as.numeric(words$num_help_types)
  return(words)
}

# Let's now run the add_possible_help_types function to categorize each word in the data.
words <- add_possible_help_types(data.2)
# List of words that only appear once in the dataset
singletons <- words$word[words$num_occurrences == 1]
# set the word to be the row index
rownames(words) <- words$word
words$word <- NULL
tail(words)

# Fill in NAs with the average latency and success over all words in singletons
singleton.index <- data.2$target_word %in% singletons
data.2$mean_word_latency[singleton.index] <- mean(data.2$latency[singleton.index])
data.2$mean_word_success[singleton.index] <- mean(data.2$success[singleton.index])

num.imputed.students <- sum(is.na(data.2$mean_latency))
num.imputed.students.by.story.level <- sum(is.na(data.2$mean_student_level_latency))
total.trials <- dim(data.2)[1]

# For these 23 students, let's just impute their missing values with the mean of the mean
# That is, let's take the average of the mean performance of the other 509 students
data.2$mean_latency <- replace(data.2$mean_latency, which(is.na(data.2$mean_latency)), 
                               mean(data.2$mean_latency, na.rm=TRUE))
data.2$mean_success <- replace(data.2$mean_success, which(is.na(data.2$mean_success)), 
                               mean(data.2$mean_success, na.rm=TRUE))

# We also have 396 trials where that trial was the only available trial for that particular
# student at that particular story level. So what should we do with these missing values?
# If we have no data for that student at that story level, then our best guess is probably
# just the overall average performance of that student.

# If mean_student_level_latency is NA, replace with mean_latency
data.2$mean_student_level_latency<- replace(data.2$mean_student_level_latency, 
                which(is.na(data.2$mean_student_level_latency)), data.2$mean_latency)
data.2$mean_student_level_success<- replace(data.2$mean_student_level_success, 
                which(is.na(data.2$mean_student_level_success)), data.2$mean_success)

# Let's double check that we have filled in all the missing data
any.nas <- colSums(is.na(data.2))
sum(any.nas) == 0
any.nas

# In order to use the join function, we need a column with the same name to appear in each dataset
words$target_word <- rownames(words)
data.2 <- join(data.2, words, by="target_word", match="first", type="left")
# Help class is the numeric code indicating which help types are available for that word
# The number itself is meaningless, but it encodes which category the word belongs to
# This feature may end up getting dropped in the analysis, but we'll include it here for now
data.2$help_class <- as.factor(data.2$help_class)
# Indicator of whether the word appears only once in the dataset
# This feature also tells us that the mean_word_latency and mean_word_success columns have been imputed
data.2$is.singleton <- as.numeric(data.2$num_occurrences == 1)

# Let's take another look at the data now that additional features have been added.
str(data.2, strict.width = "cut")

data <- data.2
# identifier for the student, now replaced by mean_success, mean_latency, mean_student_level_latency, 
# mean_student_level_success, and num_student_trials (531 features -> 5 features)
data$user_id <- NULL
# identifier for the word, now replaced by is.letter, is.number, word_length, mean_word_latency, 
# mean_word_success, num_word_trials, is.singleton, plus help types 1-13 (2784 features -> 20 features)
data$target_word <- NULL
data$num_occurrences <- NULL # this one gets discarded because it's nearly identical to num_word_trials
data$help_class <- NULL # 324 levels of this factor variable, might want to circle back to this later

# Let's first define a function that takes a dataset, the name of a target variable, and the name of alternative
# target variables. The function will discard the alternative target variables to prevent issues of data leakage,
# and returns a list, where X is a dataframe of all the predictors, and y is a vector of the target values.

############# split_X_y FUNCTION DEFINITION #############
split_X_y <- function(data, target_var, alternative_targets) {
  # target_var should be a single character string
  # alternative targets can be a single character string or a character vector
  # X will contain all features in data that are not in target_var or in alternative_targets
  target_indices <- names(data) %in% c(target_var, alternative_targets)
  X <- data[!target_indices] # dataframe
  y <- data[[target_var]] # vector
  return(list(X=X, y=y)) # list object
}

# Now we want to be able to take an object in the format returned by the split_X_y function
# and split it into a training set and a test set. Specify the size of training set as a 
# percentage of total dataset size, default size = 80% of total data.

# However, because we are dealing with unbalanced binary data, where successful trials are much more
# common than unsuccessful trials (in this dataset, success = 1 is about three times as common as 
# success = 0, but this function should be applicable for very sparse datasets as well).
# Note that this function will only work properly if target variable is numeric and binary.

############# stratified_train_test_split FUNCTION DEFINITION #############
stratified_train_test_split <- function(data, split.proportion = .80, seed=4850) {
  binary <- length(unique(data$y)) == 2
  numeric <- is.numeric(data$y)
  if (!(binary & numeric)) 
    stop("Target variable is not in correct format. Must be numeric and binary.")
  
  pos.cases <- which(data$y == 1) # indices of all cases where target variable is 1
  neg.cases <- which(data$y == 0) # indices of all cases where target variable is 0
  pos.size <- round(split.proportion * length(pos.cases)) # number of pos cases desired in the training set
  
  set.seed(seed)
  pos.train <- sample(pos.cases, pos.size) # indices of pos cases in the training set
  neg.size <- round(split.proportion * length(neg.cases)) # number of neg cases desired in the training set
  set.seed(seed)
  neg.train <- sample(neg.cases, neg.size) # indices of neg cases in the training set
  train_indices <- c(pos.train, neg.train)
  set.seed(seed)
  train_indices <- sample(train_indices, length(train_indices)) # combine and shuffle pos and neg cases
  trainX <- data$X[train_indices, ]
  testX <- data$X[-(train_indices), ]
  trainY <- data$y[train_indices]
  testY <- data$y[-train_indices ]
  # return a list object with 4 fields containing train and test predictors (as dataframes)
  # and train and test values of target (as binary numeric vectors)
  return(list(train.X = trainX, train.y = trainY, 
              test.X = testX, test.y = testY))
}

# Remember that the success variable was created deterministically from the values of latency and helped.
# So we need to be sure to exclude both of those variables from the set of predictors.
data.3 <- split_X_y(data, "success", c("latency", "helped"))
data.3 <- stratified_train_test_split(data.3) # split proportion is 80% by default
str(data.3, max.level=1)

# Build our logistic regression
train = cbind(data.3$train.X, success = data.3$train.y)
predictors = colnames(data.3$train.X) # use all features in the X dataframe as potential predictors
fit  <- glm(reformulate(predictors, "success"), data = train, family = binomial)
summary(fit)
aic.all.predictors <- fit$aic

# Let's use the logit model to predict probability of success on our test set
test = cbind(data.3$test.X, success = data.3$test.y)
yhat <- predict(fit, newdata = test, type = "response") # predicted target values on the test set
pred <- prediction(yhat, test$success)
perf <- performance(pred, "tpr", "fpr") # build and plot an ROC curve for the test data
plot(perf, main="Performance of Logistic Regression", 
     sub="Baseline Rates for Student and Word Included")
abline(0, 1, lty=2, col=2)
print_perf = paste("AUC =", round(performance(pred, "auc")@y.values[[1]], digits=3))
text(0.8, 0.1, print_perf) # displays the AUC on the ROC plot

# Let's try this on the full dataset, but remember we still need to get rid of helped and latency
data.full <- data
data.full$helped <- NULL
data.full$latency <- NULL
non.target.index <- -which(names(data.full)=="success")
predictors <- names(data.full)[non.target.index]

# In order to use the glmnet package, we need to convert our factor variables, which we can do with the model.matrix function
attach(data.full) # attach the dataset for convenience, we can remember to detach later if we want to clear up the namespace
factor.vars <- c("Type", "Reading_Level", "Story_Level")
factor.indices <- which(names(data.full) %in% factor.vars)
# this will transform our factor variables into a matrix of binary variables
xfactors <- model.matrix(reformulate(factor.vars, "success"))[, -1]
# Let's just check that we have the right number of indicator variables
dim(xfactors)[2] == (length(levels(Type)) - 1 +
                       length(levels(Reading_Level)) - 1 +
                       length(levels(Story_Level)) - 1)
data.formatted <- cbind(subset(data.full, select = -c(factor.indices, 
                                                      -non.target.index)), xfactors)
data.formatted <- as.matrix(data.formatted)
y <- as.factor(success)
detach(data.full)
dim(data.formatted)
length(y)

# Let's fit a GLM based on logistic regression with lasso penalty
# Data is standardized by default (response variable is not standardized)
# By convention, we also standardize the indicator variables, which has been 
# shown to be good/okay for performance but bad for interpretability of coefficients

# We will use cv.glmnet, which does cross-validated logistic regression
# Automatically chooses a range of lambda values, with 10 fold-cross validation
lasso.log.regression <- cv.glmnet(data.formatted, y, family="binomial", 
                                  type.measure= "auc")

best.lambda <- lasso.log.regression$lambda.min
index.best.lambda <- which(lasso.log.regression$lambda == best.lambda)
# This will give us the number of non-zero features in the regression fit
# with the maximum mean AUC across the 10 folds
num.non.zero.features <- lasso.log.regression$nzero[index.best.lambda]
coefficients <- coef(lasso.log.regression, s="lambda.min")
index.non.zero.features <- summary(coefficients)$i
non.zero.features <- rownames(coefficients)[index.non.zero.features]
auc <- lasso.log.regression$cvm[index.best.lambda]
non.zero.features

help.type.index <- grep("Type+", colnames(data.formatted))
help.available.index <- which(colnames(data.formatted) %in% levels(data.full$Type))
data.formatted.minus.help <- data.formatted[, -c(help.type.index, 
                                                 help.available.index)]

lasso.no.help.types <- cv.glmnet(data.formatted.minus.help, y, family="binomial", 
                                 type.measure= "auc")
best.lambda <- lasso.no.help.types$lambda.min
index.best.lambda <- which(lasso.no.help.types$lambda == best.lambda)
# This will give us the number of non-zero features in the regression fit
# with the maximum mean AUC across the 10 folds
num.non.zero.features <- lasso.no.help.types$nzero[index.best.lambda]
coefficients <- coef(lasso.no.help.types, s="lambda.min")
index.non.zero.features <- summary(coefficients)$i
non.zero.features <- rownames(coefficients)[index.non.zero.features]
auc <- lasso.no.help.types$cvm[index.best.lambda]
non.zero.features

intercept <- glm(success ~ 1, data=data.full, family=binomial)
full <- glm(reformulate(predictors, "success"), data=data.full, family=binomial)
total.scope <- list(lower=formula(intercept), upper=formula(full))
backwards <- step(full, direction="backward", trace=0, scope=total.scope)
forwards <- step(intercept, direction="forward", trace=0, scope=total.scope)

backwards
forwards
names(data.full)



