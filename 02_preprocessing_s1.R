# SCRIPTS FOR SENTIMENT ANALYSIS OF THE POSTS ON THE SHOW SEVERANCE

## Part II - Pre-processing

### Install & Load Packages
install.packages("tidytext")
install.packages("stopwords")
install.packages("textstem")

library(tidytext)
library(stopwords)
library(dplyr)
library(textstem)
library(readr)
library(stringr) #for extra steps cleanup

### Read the dataset
data <- read_csv("normalized_s1.csv")

### Tokenization 
tokens_s1 <- data %>%
  unnest_tokens(word, text_clean) %>%
  mutate(word = str_squish(word)) %>%
  mutate(word = str_squish(word)) %>%
  filter(str_length(word) > 1)

### Remove stop words (common words like 'the', 'and', etc.)
nostopwords_s1 <- tokens_s1 %>%
  filter(!word %in% stopwords("en"))

### Lemmatize words (reduce words to their base form)
nostopwords_s1 <- nostopwords_s1 %>%
  mutate(word_lemmatized = lemmatize_words(word))

### Additional step: remove single-character tokens
nostopwords_s1 <- nostopwords_s1 %>%
  filter(str_length(word) > 1)

### TF-IDF
### Calculate TF-IDF using the existing dataframe
tfidf <- nostopwords_s1 %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n)

head(tfidf) # view TF-IDF scores

### Reconstruct sentences with after tokenization and lemmatization
sentences_s1 <- nostopwords_s1 %>%
  group_by(id) %>%
  summarise(text_analysis = paste(word_lemmatized, collapse = " ")) %>%
  ungroup()

### Save as a new object and save new file
preprocessed_s1 <- sentences_s1
write.csv(preprocessed_s1, "preprocessed_s1.csv")