# SCRIPTS FOR SENTIMENT ANALYSIS OF THE POSTS ON THE SHOW SEVERANCE

## PART I - Normalization

### Install and Load Packages (Part I - Normalization & Cleaning)
install.packages("tidyverse")
install.packages("textclean") #expand contractions
install.packages("emoji") #convert emoji to text

library(tidyverse)
library(dplyr)
library(stringr)
library(textclean)
library(emoji)
library(stringi)


### Inspecting
data <- read_csv("comments_s1.csv")
comments_s1 <- read_csv("comments_s1.csv")
View(comments_s1)
head(data$text)  # inspect first rows of the text column (comments)

### Ensure all contractions are apostrophes
comments_s1$text <- gsub("[’‘ʼ`]", "'", comments_s1$text) #normalize apostrophes

### Expand contractions
comments_s1 <- comments_s1 %>%
  mutate(text_expand = replace_contraction(text))

### Lower casing text
comments_s1 <- comments_s1 %>%
  mutate(text_lower = tolower(text_expand))

### Remove URLs
comments_s1 <- comments_s1 %>%
  mutate(text_nourl = str_replace_all(text_lower, "http[s]?://[^\\s,]+|www\\.[^\\s,]+", ""))

#### Check if URLS are gone
sum(grepl("http|www", comments_s1$text_nourl))

### Remove mentions/usernames preceded by @
comments_s1 <- comments_s1 %>%
  mutate(
    text_nomention = str_replace_all(text_nourl, "@[A-Za-z0-9_]+", ""), # remove @mentions
    text_nomention = str_squish(text_nomention) # remove extra spaces
  )

### Remove punctuation & typographic marks
comments_s1 <- comments_s1 %>%
  mutate(text_nopunct = 
           str_replace_all(text_nomention, "[[:punct:]“”‘’–—…|]", ""))

### Remove numbers
comments_s1 <- comments_s1 %>%
  mutate(text_nonumbers = 
           str_replace_all(text_nopunct, "[[:digit:]]", ""))

### Collapsing 3 or more of the same letter in a row into one
comments_s1 <- comments_s1 %>%
  mutate(text_norep = 
           str_replace_all(text_nonumbers, "(.)\\1{2,}", "\\1"))

### Remove trailing and leading spaces
comments_s1 <- comments_s1 %>%
  mutate(text_nospace = 
           str_squish(text_norep))

### Converting emojis
#### Creating the emoji dictionary
emoji_dict <- emo::jis[, c("emoji", "name")]
emoji_dict$name <- paste0("[", emoji_dict$name, "]")  # add brackets around emoji names


#### Replacing emoji with text
replace_emojis <- function(text, emoji_dict) {
  stri_replace_all_fixed(
    str = text, 
    pattern = emoji_dict$emoji, 
    replacement = emoji_dict$name, 
    vectorize_all = FALSE
  )
}

#### Create new column in the data frame
comments_s1 <- comments_s1 %>%
  mutate(text_noemoji = replace_emojis(text_nospace, emoji_dict))

# We still have some non-characters leftover (e.g row 1755 - waffle) also, the brackets where added. 
# So let's do a final cleanup (but it is important to perform this after emoji conversion)
comments_s1 <- comments_s1 %>%
  mutate(text_clean = str_replace_all(text_noemoji, 
                                      "[^[:alpha:][:space:]]", ""),  # remove everything except letters & spaces
    text_clean = str_squish(text_clean)  # remove extra spaces
  )

### Let's save it as a new dataset
selected_columns <- comments_s1[c("id", "text_clean")]
write.csv(selected_columns, "normalized_s1.csv", row.names = FALSE)