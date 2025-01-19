library(dplyr)
library(tidyr)
library(tidyverse)

#################################################################################
## PCA - Comparing Valence & Sentiment in Popular Genres ########################
#################################################################################
library(tidytext)
library(tm)
install.packages("stopwords")
library(stopwords)

#build custom stop words which includes spanish, portuguese, japanese and mandarin 
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = stopwords("portuguese"),
                                          lexicon = "custom"),
                               data_frame(word = stopwords("spanish"),
                                          lexicon = "custom"),
                               data_frame(word = stopwords(language = "zh", source= "misc"),
                                          lexicon = "custom"),
                               data_frame(word = stopwords("ja", source = "marimo"),
                                          lexicon = "custom"))

#tokenise the words from track names 
track_words <- spotify %>%
  unnest_tokens(word, track_name) %>% 
  anti_join(custom_stop_words)
count(track_words)

#count occurances of each word
track_words %>%
  count(word, sort = TRUE)

#occurances of words by genre
words_by_genre <- track_words %>%
  count(track_genre, word, sort = TRUE) %>%
  ungroup()
words_by_genre

################################################################################
## Get sentiment Score for each track title ####################################
################################################################################
# Tokenize titles and join with AFINN
track_sentiments <- spotify %>% 
  unnest_tokens(word, track_name) %>%  # Tokenize track titles into words
  anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%    # Join with AFINN lexicon
  group_by(track_id) %>%                # Group by track
  summarise(
    sentiment_score = mean(value, na.rm = TRUE),  # Calculate mean sentiment per track
  ) 

# Merge sentiment scores with original dataset
tracks <- spotify %>%
  left_join(track_sentiments, by = "track_id") %>%
  mutate(across(sentiment_score, ~ replace(., is.na(.), 0))) %>%
  distinct()
View(tracks)

#aggregate data - mean values for visualisation
aggregated_data <- tracks %>%
  group_by(track_genre) %>%
  summarise(
    mean_valence = mean(valence),
    mean_popularity = mean(popularity),
    mean_sentiment = mean(sentiment_score)
  )
View(aggregated_data)

#################################################################################
#select acoustic variables for PCA
pca_data<- tracks %>%
  select(track_genre, popularity, valence, energy, danceability, acousticness, sentiment_score)

#aggregate the variables so mean score for each genre is established
pca_aggregated_data <- pca_data %>%
  group_by(track_genre) %>%
  summarise(
    mean_popularity = mean(popularity),
    mean_valence = mean(valence),
    mean_energy = mean(energy),
    mean_danceability = mean(danceability),
    mean_acousticness = mean(acousticness),
    mean_sentiment = mean(sentiment_score)
  )

pca<-prcomp(pca_aggregated_data[,3:7], scale=TRUE) #scale all variables
pca_aggregated_data.pca.scaled<-data.frame(
  genre=pca_aggregated_data$track_genre,
  popularity=pca_aggregated_data$mean_popularity,
  PC1=pca$x[,1],
  PC2=pca$x[,2]
)

#find top 50 popular genres
top_genres <- tracks %>%
  group_by(track_genre) %>%
  summarise(mean_popularity = mean(popularity)) %>%
  arrange(desc(mean_popularity)) %>%
  head(50)

#filter pca to only include top 50 most popular genres to not clutter visualisation
top_pca_scores <- pca_aggregated_data.pca.scaled %>%
  filter(genre %in% top_genres$track_genre)

#plot PCA 
p4<-ggplot(top_pca_scores, aes(x = PC1, y = PC2, label = genre)) +
  geom_text(size = 4, color = "black") +          # Labels outside the points
  labs(
    title = "PCA of Spotify Tracks",
    subtitle = "PCA of Spotify tracks using valence, track title sentiment, energy, danceability and acousticness\ncategorised by mean popularity score",
    x = "PCA 1",
    y = "PCA 2"
  ) +
  #scale_color_gradient(low = "blue", high = "orange") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),          # Remove major grid lines
    panel.grid.minor = element_blank(),          # Remove minor grid lines
    #legend.title = element_text(size = 12),     # Adjust legend title font size
    #legend.text = element_text(size = 10),
    #legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),  # Title customization
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 10)         # Axis title size
  )

#########################################################################################
## Scatter plot - Valence vs Sentiment of Genres with Popularity ########################
#########################################################################################

#normalisation function
normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#normalise the variables
aggregated_data$mean_valence_normalised <- normalise(aggregated_data$mean_valence)
aggregated_data$mean_sentiment_normalised <- normalise(aggregated_data$mean_sentiment)

#plot scatter
p1<-ggplot(aggregated_data, aes(x = mean_valence_normalised, y = mean_sentiment_normalised, label = track_genre)) +
  geom_point(aes(color = mean_popularity), size = 3, alpha = 0.7) +    # Set popularity as colour of points 
  geom_text(vjust = -1, size = 3, color = "black") +          # Labels outside the points
  labs(
    title = "Mean Valence vs Mean Sentiment by Genre",
    subtitle = "Top 30 genres",
    x = "Valence",
    y = "Sentiment Score"
  ) +
  scale_color_gradient(low = "blue", high = "orange") +  #set gradient colour scale
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),          # Remove major grid lines
    panel.grid.minor = element_blank(),          # Remove minor grid lines
    legend.title = element_text(size = 12),     # Adjust legend title font size
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),  # Title customisation
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 10)         # Axis title size
  )
    
################################################################################
## Wordcloud - Frequent words from track titles ################################
################################################################################
install.packages("wordcloud2")
library(wordcloud2)
library(ggwordcloud)

#sort words by occurance
word_count <- count(track_words, word) %>% 
  arrange(desc(count=n))
head(word_count, 20)

#wordcloud for all words
wordcloud2(word_count, size=1.6, color='random-dark')

################################################################################

# Join with Bing lexicon to get sentiment labels (positive/negative)
sentiment_words <- track_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%+
  head(200) #get top 200 words to limit chart size 

sentiment_words <- sentiment_words[-1, ] #remove top entry 'feat' as skews graph

#plot wordcloud
p2<-ggplot(sentiment_words, aes(label = word, size = n, colour = sentiment)) +
  geom_text_wordcloud_area() +      # Use area-based sizing for better aesthetics
  scale_size_area(max_size = 60) + # Adjust word size
  scale_color_manual(values = c("positive" = "red3", "negative" = "blue3")) + # set colours
  theme_minimal() + 
  labs(title = "Comparison Cloud: Positive vs. Negative Sentiment",
       colour = "Sentiment",    # Legend title for color
       size = "Frequency"      # Legend title for size
  ) +
  theme(legend.position = "right")

#################################################################################
## Heatmap: Audio Features Accross Genres #######################################
#################################################################################

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Example data: Audio features and genre labels (replace with your actual data)
audio_features_data <- top_genre_tracks %>%
  select(track_genre, tempo, loudness, danceability, energy, acousticness, valence, sentiment_score) %>%
  group_by(track_genre) %>%
  summarise(across(tempo:sentiment_score, mean, na.rm = TRUE))  # Calculate mean audio features per genre

audio_features_normalised <- audio_features_data %>%
  mutate(across(tempo:sentiment_score, normalise)) 

# Reshape the data to long format
audio_features_long <- melt(audio_features_normalised, id.vars = "track_genre")

# Create the heatmap
p3 <- ggplot(audio_features_long, aes(x = variable, y = track_genre, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Audio Features Across 15 Most Popular Genres",
       x = "Audio Features", y = "Genres") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################################################################
# combine plots into composite
install.packages("cowplot")
library(cowplot)

combined_plot <- plot_grid(p1, p2, p3, p4, ncol = 2)

# Print the combined plot
print(combined_plot)
