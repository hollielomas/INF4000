library(dplyr)
library(tidyr)
library(tidyverse)

#read in Spotify dataset
spotify<-read.csv("dataset.csv", header=TRUE)
View(spotify)

#new dataframe spliting tracks with multiple artists
spotify_new <- spotify %>% 
  mutate(artists = str_split(artists, ";")) %>% 
  unnest(artists)

#convert explicit column from char to numeric 0=false, 1=true
spotify$explicit <- as.integer(as.logical(spotify$explicit))
spotify_new$explicit <- as.integer(as.logical(spotify_new$explicit))

#################################################################################
## Exploratory Analysis #########################################################
#################################################################################
#count number of genres
select(spotify, track_genre) %>% n_distinct()

#count number of track's per genre
count(spotify, track_genre)

#get longest (duration) songs
longest<- spotify %>%
  arrange(desc(duration_ms)) %>%
  select(artists, track_name, duration_ms, popularity, track_genre) %>% distinct()

#longest and shortest track lengths
head(longest, 20)
tail(longest, 20)

#plot popularity vs song length
ggplot(longest, aes(x=duration_ms, y=popularity)) +
  geom_line()

#plot energy vs popularity
ggplot(spotify, aes(x=energy, y=popularity)) +
  geom_point(size=0.5)

#investigate high and low tempo tracks
tempo <- spotify %>%
  arrange(desc(energy)) %>%
  select(artists, track_genre, energy, tempo) %>%
  distinct()

head(tempo, 20)  
tail(tempo, 20)

#plot popularity vs energy by genre for top 100 most popular tracks
ggplot(head(new_df, 100), aes(x=energy, y=popularity, colour=track_genre)) +
  geom_point()

#plot popularity vs valence by genre for top 500 most popular tracks
ggplot(head(new_df, 500), aes(x=valence, y=popularity, colour=track_genre)) +
  geom_point()

#count number of tracks per artist, arrange from most to least
head(count(spotify_new, artists) %>% arrange(desc(count=n)), 20)

#get tracks with highest valence (happy/sad) scores
vibe<- spotify %>%
  arrange(desc(valence)) %>%
  select(artists, popularity, track_genre) %>% distinct()

head(vibe, 20)
tail(vibe, 20)

#number of different artists in the data
count(spotify_new, artists) %>% n_distinct()

#top 20 most frequent artists
head(count(spotify_new, artists) %>% arrange(desc(count=n)), 20)

#top 20 most popular artists
artist_pop <- spotify_new %>% 
  # desc orders from largest to smallest
  arrange(desc(popularity)) %>%
  # select subsets the columns you want
  select(artists, popularity, valence, track_genre) %>% 
  distinct() %>% head(250)

View(artist_pop)

#get subset of top 250 popular artists
top_artists <- spotify_new %>% 
  filter(artists %in% artist_pop$artists)

View(top_artists)

#avg popularity score per artist - arrange top 20 most popular
popular_artists<- spotify_new %>%
  select(artists, popularity) %>%
  group_by(artists) %>%
  summarise_at(vars(popularity), list(avg_popularity = mean)) %>%
  arrange(desc(avg_popularity)) %>%
  head(20)

popular_artists

#################################################################################
## Network Map - Valence correlation in popular genres ######################################
#################################################################################

# Install necessary libraries
install.packages(c("igraph", "ggraph", "tidygraph", "RColorBrewer"))
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)

#avg popularity by genre
popular_genres <- tracks %>%
  #select(track_name, artists, track_genre, popularity) %>%
  group_by(track_genre) %>%
  summarise_at(vars(popularity), list(avg_popularity = mean)) %>%
  arrange(desc(avg_popularity)) %>%
  head(15)

top_genre_tracks <- tracks %>%
  #select(track_name, track_genre) %>%
  filter(track_genre %in% popular_genres$track_genre) %>%
  #select(track_name, popularity, valence, track_genre) %>%
  distinct()

top_track_words <- top_genre_tracks %>%
  unnest_tokens(word, track_name) %>% 
  anti_join(custom_stop_words)
count(track_words)

#occurances of words by genre
words_by_top_genre <- top_track_words %>%
  count(track_genre, word, sort = TRUE) %>%
  ungroup()
words_by_top_genre

#Calculate correlations between artists
track_genre_cors <- words_by_top_genre %>%
  pairwise_cor(track_genre, word, n, sort = TRUE)
track_genre_cors

track_genre_cors %>%
  filter(correlation > 0.2) %>% #display correlations greater than .65
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, 
                     width = correlation), 
                 color = "aquamarine3") +
  scale_edge_width(range = c(0.5, 1.2)) +
  geom_node_point(size = 3, 
                  color = "aquamarine3") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  scale_fill_manual() +
  theme_graph(base_family = "Helvetica", base_size = 12) +
  theme(legend.position = "bottom") +
  labs(title = "Correlating Music Genres",
       subtitle ="By Common Words in Track Names")
