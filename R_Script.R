#Enron corporating emails 
#Predicting spam emails using bayes model
library(tidyverse)
install.packages("e1071")
library(e1071)

getwd()
setwd("C:/Users/Admin/Desktop/")

data <- read.csv("C:/Users/Admin/Desktop/email.csv")

summary(data)
str(data)
head(data)

#Message_label will be our predictor.
#Adjusting column message_label to a factor.
data <- data %>% mutate(message_label = as.factor(message_label))

#identifying words that occur often in the email dataset. 
data %>% 
  gather(word,count,-message_index,-message_label)

#Listing the top 10 most common words.

data %>%
  gather(word,count,-message_index,-message_label) %>%
   group_by(word) %>%
     summarize(occurrence = sum(count)) %>%
       arrange(desc(occurrence)) %>%
         slice(1:10)

#Listing the top 10 most common words for spam messages
data %>%
  filter(message_label == 'spam') %>%
    gather(word,count,-message_index,-message_label) %>%
      group_by(word) %>%
        summarize(occurrence = sum(count)) %>%
          arrange(desc(occurrence)) %>%
            slice(1:10)

#Listing the top 10 most common words for "normal" ham messages
data %>%
  filter(message_label == 'ham') %>%
    gather(word,count,-message_index,-message_label) %>%
      group_by(word) %>%
        summarize(occurrence = sum(count)) %>%
          arrange(desc(occurrence)) %>%
            slice(1:10)


#Training dataset, split method
#80:20 training to test ratio
#data ham.49 spam .51
#data_train ham .49 spam .51
#data_test  ham .49 spam . 51

set.seed(1234)
sample_set <- sample(nrow(data), round(nrow(data)*.80), replace = FALSE)
data_train <- data[sample_set,]
data_test <- data[-sample_set,]
round(prop.table(table(select(data,message_label))),2)
round(prop.table(table(select(data_train,message_label))),2)
round(prop.table(table(select(data_test,message_label))),2)


#Learning formula
message_label ~ .message_index

#Training the model
data_model <- naiveBayes(message_label ~ .-message_index, data = data_train, laplace = 1)

#Evaluating performance of trained model. 
data_predict <- predict(data_model, data_test, type = "raw")
head(data_predict)

data_predict_table <- table(data_test$message_label, data_predict)
data_predict_table

#92% predicting accuracy. 
sum(diag(data_predict_table)) /  nrow(data_test)
