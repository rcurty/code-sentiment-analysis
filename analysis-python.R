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