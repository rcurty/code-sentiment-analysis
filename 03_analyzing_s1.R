# SCRIPTS FOR SENTIMENT ANALYSIS OF THE POSTS ON THE SHOW SEVERANCE

## Part III - Sentiment Analysis
install.packages("sentimentr")
install.packages("readr")
install.package("devtools")
install.packages("syuzhet")

library(sentimentr)
library(readr)
library(devtools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(syuzhet)

data <- read_csv("preprocessed_s1.csv")

### Sentiment Analysis

#### Polarity with Sentiment R 

##### Avoid repated sentence parsing and warning
sentences_list <- get_sentences(data$text_analysis)

##### Calculate sentiment per row and add labels and columns to dataset
sentiment_scores <- sentiment_by(sentences_list)$ave_sentiment
data <- data %>%
  mutate(score = sentiment_scores,
         sentiment_label = case_when(
           score > 0.1  ~ "positive",
           score < -0.1 ~ "negative",
           TRUE         ~ "neutral"
         ))

head(data)

# Visualize

ggplot(data, aes(x = score)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "white") +
  theme_minimal() +
  labs(title = "Sentiment Score Distribution", x = "Average Sentiment", y = "Count")

table(data$sentiment_label)

# See extreme cases
head(data[order(data$score), ], 10)   # Most negative
head(data[order(-data$score), ], 10)  # Most positive

# Plotting Scores
ggplot(data, aes(x = seq_along(score), y = score)) +
  geom_line(color = "steelblue") +
  geom_smooth(se = FALSE, color = "orange") +
  theme_minimal() +
  labs(title = "Sentiment Trajectory by Entry", x = "Entry Order", y = "Average Sentiment")


### Fine-grained analysis with Syuzet
#Get emotion scores based on NCR system (8 emotions)
emotion_scores_s1 <- get_nrc_sentiment(data$text_analysis)

#Combine with data
data_emotions_s1 <- cbind(data, emotion_scores_s1)

#View top entries by emotion
head(data_emotions_s1[order(-data_emotions_s1$joy), ])
head(data_emotions_s1[order(-data_emotions_s1$disgust), ])

#Visualizing Top Emotions
emotion_summary <- data_emotions_s1 %>%
  select(anger:trust) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(cols = everything(), names_to = "emotion", values_to = "count")

ggplot(emotion_summary, aes(x = reorder(emotion, -count), y = count)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Emotion Counts in Text", x = "Emotion", y = "Count")

###Getting some examples
joyful_examples <- data_emotions_s1[order(-data_emotions_s1$joy), ]
head(joyful_examples$text_clean, 5)

sadness_examples <- data_emotions_s1[order(-data_emotions_s1$sadness), ]
head(sadness_examples$text_clean, 5)