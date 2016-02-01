# APSTA-GE 2017
# Jacqueline Gutman
# Project Code
# Last Updated: May 9, 2015

format_data <- function(data) {
  # change all words to lowercase
  data$target_word <- factor(tolower(data$target_word)) 
  # regular expression to find the words that are just numbers
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

get_mean_other <- function(levels.var, mean.var, data) {
  # Add a feature that gives the mean and number of trials over all trials in the data 
  # for that particular value of the levels variable
  
  # TODO update get_mean_other function to accommodate list of levels.var
  # i.e. get mean for each interaction of student and word variable
  all_trials <- ldply(levels(data[[levels.var]]), function(id) {
    subset <- data[[mean.var]][data[[levels.var]] == id]
    data.frame(mean_all_trials=mean(subset), num_trials=length(subset))
  })
  rownames(all_trials) <- levels(data[[levels.var]])
  
  # We need to make sure we exclude the current value for that particular trial from the mean
  num_excluding_current <- all_trials$num_trials[data[[levels.var]]] - 1
  mean_excluding_current <- (all_trials$mean_all_trials[data[[levels.var]]] * 
                               all_trials$num_trials[data[[levels.var]]] - data[[mean.var]]) / 
    num_excluding_current
  return(list(mean = mean_excluding_current, num = num_excluding_current))
}

split_X_y <- function(data, target_var, alternative_targets) {
  # exclude variables latency, success
  target_indices <- names(data) %in% c(target_var, alternative_targets)
  X <- data[!target_indices]
  y <- data[[target_var]]
  return(list(X=X, y=y))
}

stratified_train_test_split <- function(data, split.proportion) {
  pos.cases <- which(data$y == 1)
  neg.cases <- which(data$y == 0)
  pos.size <- round(split.proportion * length(pos.cases))
  pos.train <- sample(pos.cases, pos.size)
  neg.size <- round(split.proportion * length(neg.cases))
  neg.train <- sample(neg.cases, neg.size)
  train_indices <- c(pos.train, neg.train)
  train_indices <- sample(train_indices, length(train_indices))
  trainX <- data$X[train_indices, ]
  testX <- data$X[-(train_indices), ]
  trainY <- data$y[train_indices]
  testY <- data$y[-train_indices ]
  return(list(train.X = trainX, train.y = trainY, 
              test.X = testX, test.y = testY))
}

add_possible_help_types <- function(data.2) {
  help_types_by_word <- sapply(levels(data.2$target_word), function(word) 
    sort.int(unique(data.2$Type[data.2$target_word == word])))
  help_type_subsets <- unique(help_types_by_word)
  letters.and.numbers <- unique(data.2$target_word[
    (data.2$is.letter == 1) | (data.2$is.number == 1)])
  help_types_letters_and_numbers <- sapply(letters.and.numbers, function(word) 
    sort.int(unique(data.2$Type[data.2$target_word == word])))
  count_trials_by_word <- ldply(levels(data.2$target_word), function(word)
    count(data.2$target_word[data.2$target_word == word]))
  
  types <- ldply(seq(to=length(help_type_subsets)),
                 function(i) {
                   category = help_type_subsets[[i]]
                   size = length(category)
                   types = sapply(levels(category), function(type) type %in% category)
                   c(index=i, len=size, types) 
                 })
  
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
  types = colnames(words)[5:length(words)]
  for(type in types) {
    words[[type]] <- as.numeric(as.logical(words[[type]]))
  }
  words$num_occurrences <- as.numeric(words$num_occurrences)
  words$help_class <- as.numeric(words$help_class)
  words$num_help_types <- as.numeric(words$num_help_types)
  return(words)
}

plot_success_difficulty <- function(data.2) {
  reading.levels <- levels(data.2$Reading_Level)
  reading.level.means <- ldply(reading.levels, function(level) 
    data.frame(mean = mean(data.2$success[data.2$Reading_Level == level])))
  rownames(reading.level.means) <- reading.levels
  barplot(height=reading.level.means$mean, names.arg = reading.levels, 
          main = "Success Rate by Reading Level",
          xlab = "Reading Level of Student",
          ylab = "Average Success Rate")
  
  story.levels <- levels(data.2$Story_Level)
  story.level.means <- ldply(story.levels, function(level) 
    data.frame(mean = mean(data.2$success[data.2$Story_Level == level])))
  rownames(story.level.means) <- story.levels
  barplot(height=story.level.means$mean, names.arg = story.levels,
          main = "Success Rate by Story Level",
          xlab = "Difficulty Level of Story",
          ylab = "Average Success Rate")
}

get_mean_other_multicat <- function(levels.var, mean.var, data) {
  # this function is just like get_mean_other but it should work
  # when levels.var is a vector of features we want to condition on
  
  x <- split(data[, c(levels.var, mean.var)], data[, levels.var])
  y <- sapply(seq(length(x)), function(i) {dim(x[[i]])[1] > 0})
  x <- x[y]
  
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

library(pander)
library(plyr)
library(ROCR)

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

# TODO merge in data from words[3:17] about help type availability
names(words)[1] <- names(data.2)[3]
data.2 <- join(data.2, words, by="target_word", match="first", type="left")
data.2$help_class <- as.factor(data.2$help_class)
###data.2$is.singleton <- as.numeric(data.2$targe)

# TODO drop categorical identifiers for each student and each word
data = data.2
data$user_id <- NULL
data$target_word <- NULL
data$num_occurrences <- NULL
data$help_class <- NULL

data.3 <- split_X_y(data, "success", c("latency", "helped")
data.3 <- stratified_train_test_split(data.3, 0.80)
str(data.3)

# TODO get logistic regression and other models working

# build our logistic regression
train = cbind(data.3$train.X, success = data.3$train.y)
predictors = colnames(data.3$train.X)
# this line hangs, not sure why
fit  <- glm(reformulate(predictors, "success"), data = train, 
            family = binomial)
summary(fit)

# let's use the logit model to predict probability of success on our test set
test = cbind(data.3$test.X, success = data.3$test.y)
yhat <- predict(fit, newdata = test, type = "response")
pred <- prediction(yhat, test$success)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main="Performance of Logistic Regression", 
	sub="Baseline Rates for Student and Word Included")
abline(0, 1, lty=2, col=2)
print_perf = paste("AUC =", round(performance(pred, "auc")@y.values[[1]], digits=3))
text(0.8, 0.1, print_perf)







# Let's try this on the full dataset, but remember we still need to get rid of helped and latency
data.full <- data
data.full$helped <- NULL
data.full$latency <- NULL
non.target.index <- -which(names(data.full)=="success")
predictors <- names(data.full)[non.target.index]
intercept <- glm(success ~ 1, data=data.full, family=binomial)
full <- glm(reformulate(predictors, "success"), data=data.full, family=binomial)
total.scope <- list(lower=formula(intercept), upper=formula(full))
#backwards <- step(full, direction="backward", trace=0, scope=total.scope)
#forwards <- step(intercept, direction="forward", trace=0, scope=total.scope)