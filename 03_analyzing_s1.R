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

#CONSIDER REMOVING - NOT TRAINED AND NOT A GOOD PERFORMANCE

###More advanced fine-grained analysis (Python boosted)
install.packages("reticulate")
library(reticulate)
library(ggplot2)

virtualenv_create("myenv")
use_virtualenv("myenv", required = TRUE)
virtualenv_install("myenv", packages = c("transformers", "torch"))

transformers <- import("transformers")
torch <- import("torch")

model <- transformers$AutoModelForSequenceClassification$from_pretrained("bert-base-uncased")
tokenizer <- transformers$AutoTokenizer$from_pretrained("bert-base-uncased")

predict_emotion <- function(text) {
  labels <- unlist(model$config$id2label) #28+ emotions
    
  sapply(texts, function(text) {
    inputs <- tokenizer(text, return_tensors = "pt", padding = TRUE, truncation = TRUE)
    logits <- model(inputs$input_ids)$logits
    idx <- torch$argmax(logits, dim = as.integer(1))$item()
    labels[idx + 1]  # Python indexing starts at 0
    })
  }

# Add a new column with predicted emotions
data$emotion <- sapply(data$text_analysis, predict_emotion)

# See the results
head(data[, c("text_analysis", "emotion")])


unique_emotions <- unique(data$emotion)
print(unique_emotions)

ggplot(data, aes(x = emotion)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribution of Emotions", x = "Emotion", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))