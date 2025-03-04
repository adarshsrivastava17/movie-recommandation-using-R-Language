library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(e1071)
library(ggplot2)
library(corrplot)
library(GGally)
library(gridExtra)

data <- read.csv("C:/Users/adars/OneDrive/Desktop/AI-Project/movies.csv")
print(head(data))

str(data)
dim(data)
summarize_all(data, funs(sum(is.na(.))))

# Exploratory Data Analysis (EDA)

num_cols <- names(data)[sapply(data, is.numeric)]
plot_list <- lapply(num_cols, function(col) {
  ggplot(data, aes_string(x = col)) + 
    geom_histogram(fill = "blue", color = "black", bins = 30) +
    ggtitle(paste("Distribution of", col))
})
grid.arrange(grobs = plot_list, ncol = 3)

missing_data <- colSums(is.na(data))
ggplot(data.frame(Feature = names(missing_data), Missing = missing_data), aes(x = Feature, y = Missing)) + 
  geom_bar(stat = "identity", fill = "red") + 
  ggtitle("Missing Values in Dataset") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

selected_features <- c('genres', 'keywords', 'tagline', 'cast', 'director')
data[selected_features] <- lapply(data[selected_features], function(x) ifelse(is.na(x), '', x))

data$combined_features <- apply(data[selected_features], 1, paste, collapse = ' ')

vectorizer <- text2vec::TfIdf$new()
feature_vectors <- vectorizer$fit_transform(data$combined_features)

similarity <- proxy::dist(feature_vectors, method = "cosine")

# Movie Recommendation System
recommend_movies <- function(movie_name) {
  list_of_all_titles <- data$title
  close_match <- agrep(movie_name, list_of_all_titles, value = TRUE, ignore.case = TRUE)[1]
  index_of_movie <- which(data$title == close_match)
  similarity_score <- as.numeric(similarity[index_of_movie, ])
  sorted_similar_movies <- order(similarity_score, decreasing = TRUE)
  
  print("Movies suggested for you:")
  for (i in 2:11) {
    print(data$title[sorted_similar_movies[i]])
  }
}

movie_name <- readline(prompt = "Enter your favorite movie name: ")
recommend_movies(movie_name)
