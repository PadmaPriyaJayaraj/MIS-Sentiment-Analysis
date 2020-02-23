# Load libraries
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(RWeka)
# Text transformations
library(reshape2)
library(radarchart)
install.packages('circlize')
library(circlize)

# Read the data
scripts <- read.csv("Harry6.csv")
# select dialogues
diags <- iconv(scripts$dialogue)

# select characters
chars <- iconv(scripts$Character1)

# how many dialogues in each episode
ndial = length(di0ags)

# set of all unique characters in the script
all_chars <- as.data.frame(sort(table(scripts$Character1), decreasing=TRUE))

# Top 10 characters with more dialogues 
top_chars <- as.data.frame(sort(table(scripts$Character1), decreasing=TRUE))[1:11,]


# how many characters
ntop = nrow(all_chars)
all_chars$Var1

# create empty vector to collect dialogues of all characters
char_diags = rep("", ntop)
# collect dialogues for all characters
for (i in 1:ntop) {
  char_diags[i] = paste(scripts$dialogue[chars == all_chars$Var1[i]], collapse=" ")
}
names(char_diags) = all_chars$Var1

# ==================================================================
# Text mining
# ==================================================================

# get corpus
diag_corpus = Corpus(VectorSource(char_diags))

# apply some text transformations
diag_corpus = tm_map(diag_corpus, tolower)
diag_corpus = tm_map(diag_corpus, removeWords, 
                     c(stopwords("english"),"comlink"))
diag_corpus = tm_map(diag_corpus, removeNumbers)
diag_corpus = tm_map(diag_corpus, removePunctuation)
diag_corpus = tm_map(diag_corpus, stripWhitespace)

# get document-term matrix
diag_dtm = DocumentTermMatrix(diag_corpus)

# inspect diag_dtm
# (90% sparsity, which means a lot of empty cells)
diag_dtm
dim(diag_dtm)

# convert as matrix
diag_mat = as.matrix(diag_dtm)

# get word count
count_terms = colSums(diag_mat)
hist(count_terms, col="gray80")

# ==================================================================
# Get Charecters with most dialogues
# ==================================================================
# Visualization of most dialogues spoken top 10 charecters
ggplot(data=top_chars, aes(x=Var1, y=Freq)) +
  geom_histogram(stat="identity", fill="#FFDB6D", colour="red") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")

# geom_smooth plot for above data (needs more work)
ggplot(data=top_chars , aes(x =Var1, y =Freq)) + 
  geom_point(aes(color = Var1))+ # add points to our plot, color-coded by charecter
  geom_line()+
  geom_smooth(method = "loess")

?geom_smooth()

# ==================================================================
# Get most spoken words
# ==================================================================

# to simplify, we need to get the less sparsed terms
# for instance, let's get terms >= 90% quantile frequency
which_mfw <- count_terms >= quantile(count_terms, probs=0.90)
sum(which_mfw)
# top 20 terms
mfw <- count_terms[which_mfw]
barplot(head(sort(mfw, decreasing=TRUE), 20), 
        border=NA, las=2)
title(c("Most frequent terms in dialogues",
        "(from top characters)"), cex.main=0.9)

#Trail code (needs more work)
mfw1 <- head(sort(count_terms,decreasing=TRUE),20)
# ==================================================================
# Sentiment Analysis ( Tokenize with vector later)
# ==================================================================
# Transform the text to a tidy data structure with one token per row
tokens <- scripts %>%  
  mutate(dialogue=as.character(scripts$dialogue)) %>%
  unnest_tokens(word, dialogue)

tokens <- tokens %>% select(Character1, Character2, word)

# Sentiments and frequency associated with each word  
sentiments <- tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) 

# ==================================================================
#Frequency of each sentiment
# ==================================================================
# Frequency of each sentiment
ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw()

# ==================================================================
# Top 10 terms for each sentiment
# ==================================================================

sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Terms") +
  coord_flip() +
  theme_bw() 

# ==================================================================
# Each sentiments for top 10 charecters
# ==================================================================

tokens %>%
  filter(tokens$Character1 %in% c("Harry","Dumbledore","Slughorn","Ron","Hermoine",
                                  "Snape","Ginny","Bellatrix","Hagrid","Malfoy")) %>%
  group_by(Character1) %>% 
  inner_join(get_sentiments("nrc"), by = 'word') %>%
  count(Character1, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~Character1, scales="free_x") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw()  

# Most relevant words for each character
# Tokens without stopwords
mystopwords <- data_frame(word=c(stopwords("english"), 
                                 c("thats","weve","hes","theres","ive","im",
                                   "will","can","cant","dont","youve","us",
                                   "youre","youll","theyre","whats","didnt")))
top_chars_tokens <- scripts %>%
  mutate(dialogue=as.character(scripts$dialogue)) %>%
  filter(scripts$Character1 %in% c("Harry","Dumbledore","Slughorn","Ron","Hermoine",
                          "Snape","Ginny","Bellatrix","Hagrid","Malfoy")) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(mystopwords, by="word")

top_chars_tokens %>%
  count(Character1, word) %>%
  bind_tf_idf(word, Character1, n) %>%
  group_by(Character1) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, Character1, sep="__"), 
                      levels=rev(paste(word, Character1, sep="__"))))%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=Character1), show.legend=FALSE) +
  facet_wrap(~Character1, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf-idf", x="Sentiment") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()


# ==================================================================
# Relation between charecter and sentiment.
# ==================================================================
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")
grid.col = c("Harry" = my_colors[1], "Dumbledore" = my_colors[2],
             "Slughorn" = my_colors[3], "Ron" = my_colors[4], 
             "Hermoine" = my_colors[5], "anger" = "grey", 
             "anticipation" = "grey", "disgust" = "grey", 
             "fear" = "grey", "joy" = "grey", "sadness" = "grey", 
             "surprise" = "grey", "trust" = "grey", "positive" = "grey", 
             "negative" = "grey")

char_mood <-  top_chars_tokens %>%
  inner_join(get_sentiments("nrc"), by = 'word') %>%
  #filter(!sentiment %in% c("positive", "negative")) %>%
  count(sentiment, Character1) %>%
  group_by(Character1, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

circos.clear()
#Set the gap size
circos.par(gap.after = c(rep(5, length(unique(char_mood[[1]])) - 1), 15,
                         rep(5, length(unique(char_mood[[2]])) - 1), 15))
chordDiagram(char_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Character and Sentiment")

# ==================================================================
#Get the count of words per sentiment per year
# ==================================================================

char_sentiment_nrc <- tokens %>%
  inner_join(get_sentiments("nrc"), by = 'word') %>%
  group_by(Character1, sentiment) %>%
  count(Character1, sentiment) %>%
  select(Character1, sentiment, sentiment_charcount = n)

#Get the total count of sentiment words per year (not distinct)
total_sentiment_year <- tokens %>%
  count(Character1) %>%
  select(Character1, char_total = n)

#Join the two and create a percent field
char_radar_chart <- char_sentiment_nrc %>%
  inner_join(total_sentiment_year, by = "Character1") %>%
  mutate(percent = sentiment_year_count / year_total * 100 ) %>%
  filter(char %in% c("Harry","Dumbledore","Ron")) %>%
  select(-sentiment_year_count, -year_total) %>%
  spread(year, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "NRC Years Radar")  
