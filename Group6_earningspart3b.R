setwd("E:/Desktop/MIS")
install.packages("jiebaR")
install.packages("sentimentr")
install.packages("wordcloud2")
install.packages("ggplot2")
install.packages("tm")
install.packages("xlsx")
install.packages("wordcloud")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("DT")
install.packages("crfsuite")
install.packages("tidytext")
install.packages("textdata")
library(ggplot2)
library(tm)
library(xlsx)
library(jiebaR)
library(sentimentr)
library(wordcloud)
require(data.table)
library(dplyr)
library(DT)
library(tidytext)
library(stringr)
library(sentimentr)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(reticulate)
library(crfsuite)
library(textdata)
library(RColorBrewer)

data = fread("Customer.csv",header = TRUE)

#data cleaning

words <- data %>%
  select(c("Gender", "Reviews", "Ratings on 5", "Positive/Negative", "Topics")) %>%
  unnest_tokens(word, Reviews, drop= FALSE) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

datatable(head(words))

#Numerical data
afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
reviews.afinn <- words %>%
  inner_join(afinn, by = "word")
head(reviews.afinn)

word_summary <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(value), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))
datatable(head(word_summary))

#Ploting Topics v/s Ratings

gg3 <- ggplot(data, aes(x= data$Topics, y=data$`Ratings on 5`)) + 
  geom_bar(stat='identity', aes(fill= data$`Ratings on 5`), width=0.5)  +
  labs(subtitle="Emotions vs Ratings", 
       x ="Emotions",
       y = "Ratings on 5",
       title= "Harry Potter 6")+
       ylim(0, 5)
plot(gg3) 

# File creation for average sentiments
sentrv <- with(data, sentiment_by(Reviews), topic)

# Saving
write.table(sentrv, file = 'Sentiment Analysis.csv',sep = ',',row.names = F)

# Bar plot on the average sentiment
qplot(sentrv$ave_sentiment, geom="histogram", binwidth = 1, xlab = "Avg Sentiment", fill=I("blue"), alpha=I(.8))
ggplot(aes(x = ave_sentiment), data = sentrv) +
  geom_histogram(binwidth = 0.25, color = I("red"), fill = I("#F49045"))+
  xlab("sentiment")

# Word cloud for the feelings
wordcloud(words = word_summary$word, freq = word_summary$count_word, min.freq = 1, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Dark2"))


# Word cloud good
good <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(value), score = max(value), count_word = n()) %>%
  filter(score>0) %>%
  arrange((score))
datatable(head(words))
wordcloud(words = good$word, freq = good$count_word,  min.freq = 1,scale=c(5,0),max.words=100, colors= brewer.pal(8, "Dark2"))


#Word cloud bad

bad <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(value), score = max(value), count_word = n()) %>%
  filter(mean_rating<0) %>%
  arrange(mean_rating)
wordcloud(words = bad$word, freq = bad$count_word, min.freq = 1, scale=c(5,0), max.words=100, colors=brewer.pal(8, "Dark2"))


#Quadrant graph

y_mid = 0
x_mid = 2.5

word_summary %>% 
  mutate(quadrant = case_when(mean_rating > x_mid & score > y_mid   ~ "Positive Review/Postive Sentiment",
                              mean_rating <= x_mid & score > y_mid  ~ "Negative Review/Positive Sentiment",
                              mean_rating <= x_mid & score <= y_mid ~ "Negative Review/Negative Sentiment",
                              TRUE                                      ~ "Positive Review/Negative Sentiment")) %>% 
  ggplot(aes(x = mean_rating, y = score, color = quadrant)) + 
  geom_hline(yintercept=y_mid, color = "black", size=.25) + 
  geom_vline(xintercept=x_mid, color = "black", size=.25) +
  guides(color=FALSE) +
  scale_color_manual(values=c("red", "blue", "green","yellow")) +
  ggtitle("Customer Rating vs Score Rating of Review") +
  ggplot2::annotate("text", x = 4.5, y=3.5,label="Positive Review/Postive Sentiment") +
  ggplot2::annotate("text", x = 2, y=3.0,label="Negative Review/Positive Sentiment") +
  ggplot2::annotate("text", x = 4.33, y=-2.5,label="Positive Review/Negative Sentiment") +
  ggplot2::annotate("text", x = 2, y=-2.0,label="Negative Review/Negative Sentiment") +
  geom_point()

