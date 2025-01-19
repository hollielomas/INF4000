library(dplyr)
library(tidyr)
library(tidyverse)

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

longest<- spotify %>%
  arrange(desc(duration_ms)) %>%
  select(artists, track_name, duration_ms, popularity, track_genre) %>% distinct()

#longest and shortest track lengths
head(longest, 20)
tail(longest, 20)

#popularity vs length
ggplot(longest, aes(x=duration_ms, y=popularity)) +
  geom_line()

#energy vs popularity
ggplot(spotify, aes(x=energy, y=popularity)) +
  geom_point(size=0.5)

#tempo
tempo <- spotify %>%
  arrange(desc(energy)) %>%
  select(artists, track_genre, energy, tempo) %>%
  distinct()

head(tempo, 20)  
tail(tempo, 20)

#popularity vs energy by genre for top 100 most popular tracks
ggplot(head(new_df, 100), aes(x=energy, y=popularity, colour=track_genre)) +
  geom_point()

#popularity vs valence by genre for top 500 most popular tracks
ggplot(head(new_df, 500), aes(x=valence, y=popularity, colour=track_genre)) +
  geom_point()

#count number of tracks per artist, arrange from most to least
head(count(spotify_new, artists) %>% arrange(desc(count=n)), 20)

#get tracks with highest valence scores
vibe<- spotify %>%
  arrange(desc(valence)) %>%
  select(artists, popularity, track_genre) %>% distinct()

head(vibe, 20)
tail(vibe, 20)

#number of different artists
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
