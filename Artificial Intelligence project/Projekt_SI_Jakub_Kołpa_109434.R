# ?adowanie wymaganych bibliotek
rm(list=ls())
setwd("C:/Users/Kuba/OneDrive - SGH/Pulpit/stare/Licencjat/Spotify")
library(httr)
library(jsonlite)
require(purrr)
library(writexl)
library(spotifyr)
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
require(ggplot2)
require(pROC); citation("pROC")
library(pscl)
library(ggcorrplot)
library(regclass)
library(reshape2)
library(tidyr)
library(MASS)
library(randomForest)
library(tidyverse)
library(e1071)
library(nnet)
library(NeuralNetTools)
library(ggpubr)

#-----------------------------------------------------------------------------------------------------------------------------
# ??danie API 
base_url <- "https://api.spotify.com/v1/search?"
query <- "year:2000-2022" # Ograniczenie wyszukiwania do utwor?w od 2000 do 2022
limit <- 50 # Liczba utwor?w na jedno ??danie API 
n <- 10000 # Ca?kowita liczba identyfikator?w utwor?w do pobrania
offsets <- seq(0, n, by = limit)
client_id <- "39417f8584d64122a8260391119d4808"
client_secret <- "cc71a6107caa4548b29f7c44c2d69176"
auth <- POST("https://accounts.spotify.com/api/token",
             body = list(grant_type = "client_credentials"),
             authenticate(client_id, client_secret),
             encode = "form")
access_token <- content(auth)$access_token

# Tworzenie pustego wektora identyfikator?w utwor?w muzycznych
song_ids <- character()

# Tworzenie wektora zawieraj?cego kody kraj?w Unii Europejskiej
countries <- c("AT","BE", "BG","HR","CY","CZ","DK","EE","FI","FR","EL","ES","IE",
               "LT","LV","LU","MT","NL","DE","PL","PT","RO","SK","SI","SE","HU","IT")

# P?tla przechodz?ca przez wszystkie ??dania
for (country in countries){
  offsets <- seq(0, n, by = limit)
  for (offset in offsets) {
    # Tworzenie ??dania API
    response <- GET(
      url = base_url,
      query = list(
        q = query,
        type = "track",
        limit = limit,
        offset = offset,
        market = country
      ),
      add_headers(Authorization = paste0("Bearer ", access_token))
    )
    
    # Przetwarzanie odpowiedzi JSON
    data <- fromJSON(rawToChar(response$content))
    
    # Wyodr?bnienie identyfikator?w utwor?w z odpowiedzi i do??czenie ich do wektora.
    song_ids <- c(song_ids, data$tracks$items$id)
    
    # Zatrzymanie p?tli, je?li uzyskano wystarczaj?co du?o identyfikator?w piosenek
    if (length(song_ids) >= n) break
    
    # Wstrzymanie na 1 sekund?, aby unikn?? przekroczenia limitu szybko?ci API
    Sys.sleep(1)
  }
}

# Usuni?cie zduplikowanych identyfikator?w
song_ids <- unique(song_ids)

# Stworzenie pliku o formacie "xlsx" z identyfikatorami
write.csv(song_ids,"SONGIDS.csv")
#------------------------------------------------------------------------------------------------------------------------

# Uzyskiwanie informacji o utworach z identyfikator?w
base_url <- "https://api.spotify.com/v1/tracks?"

# Utworzenie wektora identyfikator?w piosenek na wypadek, gdyby tutaj zaczyna?o si? wykonywanie kodu
song_ids <- read_xlsx("SONGIDS.xlsx")
song_ids <- data.frame(song_ids[,2])
songs_ids <- c()
for (i in 1:nrow(song_ids)){
  songs_ids<- c(songs_ids,song_ids[i,])
}
songs_ids <- songs_ids[1:2000]
song_ids <- c(song_ids)

# Tworzenie data frame'u z informacjami o utworach
songs_df <- data.frame(track_id = character(),
                       track_name = character(),
                       artist_name = character(),
                       album_name = character(),
                       release_date = character(),
                       song_popularity = integer(),
                       artist_id = character(),
                       album_id = character(),
                       stringsAsFactors = FALSE)


# P?tla przez ??dania API a? do osi?gni?cia po??danej liczby utwor?w
for (i in 1:length(songs_ids)) {
  ids <- songs_ids[i]
  params <- list(ids=ids)
  
  # Tworzenie ??dania API
  res <- GET(url = base_url,
             query = params,
             add_headers(Authorization = paste("Bearer", access_token)))
  
  songs <- content(res)$tracks
  songs_info <- data.frame(track_id = sapply(songs, function(x) x$id),
                           track_name = sapply(songs, function(x) x$name),
                           artist_name = sapply(songs, function(x) x$artists[[1]]$name),
                           album_name = sapply(songs, function(x) x$album$name),
                           release_date = sapply(songs, function(x) x$album$release_date),
                           song_popularity = sapply(songs, function(x) x$popularity),
                           artist_id = sapply(songs, function(x) x$artists[[1]]$id),
                           album_id =  sapply(songs, function(x) x$album$id),                
                           stringsAsFactors = FALSE)
  
  # Dodawanie informacji do data frame'a
  songs_df <- rbind(songs_df, songs_info)
}

# Tworzenie pliku xlsx zawieraj?cego uzyskane informacje
write_xlsx(songs_df, "Songs.xlsx", col_names = TRUE)

# Ponowne odczytanie data frame'a
songs_df <- read_xlsx("Songs.xlsx")
songs_df <- data.frame(songs_df)


# Tworzenie pustego data frame'a do przechowywania informacji o artystach
artist_url <- "https://api.spotify.com/v1/artists?"
artists_df <- data.frame(artist_id = character(),
                         artist_name = character(),
                         artist_popularity = integer(),
                         followers = integer(),
                         stringsAsFactors = FALSE)

# P?tla przez ??dania API a? do osi?gni?cia po??danej liczby artyst?w
for (i in 1:length(songs_ids)) {
  id <- songs_df[i,"artist_id"]
  params2 <- list(ids=id)
  
  # Tworzenie ??dania API
  res2 <- GET(url = artist_url,
              query = params2,
              add_headers(Authorization = paste("Bearer", access_token)))
  # Wyci?gni?cie informacji o arty?cie z odpowiedzi API
  artists <- content(res2)$artists
  artists_info <- data.frame(artist_id = sapply(artists, function(x) x$id),
                             artist_name = sapply(artists, function(x) x$name),
                             artist_popularity = sapply(artists, function(x) x$popularity),
                             followers = sapply(artists, function(x) x$followers$total),
                             stringsAsFactors = FALSE)
  
  # Dodawanie informacji do data frame'a
  artists_df <- rbind(artists_df, artists_info)
  if (i%%10==0){
    Sys.sleep(1)
  }
}

# Tworzenie pliku xlsx zawieraj?cego uzyskane informacje
write_xlsx(artists_df, "Artists.xlsx", col_names = TRUE)

# Ponowne odczytanie data frame'a
artists_df <- read_xlsx("Artists.xlsx")
artists_df <- data.frame(artists_df)

# Tworzenie pustego data frame'a do przechowywania informacji o albumach
album_url <- "https://api.spotify.com/v1/albums?"
albums_df <- data.frame(album_id = character(),
                        artist_id = character(),
                        album_popularity = integer(),
                        stringsAsFactors = FALSE)

# P?tla przez ??dania API a? do osi?gni?cia po??danej liczby album?w
for (i in 1:length(songs_ids)) {
  idd <- songs_df[i,"album_id"]
  params3 <- list(ids=idd)
  
  # Tworzenie ??dania API
  res3 <- GET(url = album_url,
              query = params3,
              add_headers(Authorization = paste("Bearer", access_token)))
  # Wyci?gnij informacje o albumie z odpowiedzi API
  albums <- content(res3)$albums
  albums_info <- data.frame(album_id = sapply(albums, function(x) x$id),
                            artist_id = sapply(albums, function(x) x$artists[[1]]$id),
                            album_popularity = sapply(albums, function(x) x$popularity),
                            stringsAsFactors = FALSE)
  
  # Dodawanie informacji do data frame'a
  albums_df <- rbind(albums_df, albums_info)
  if (i%%5==0){
    Sys.sleep(1)
  }
}

# Tworzenie pliku xlsx zawieraj?cego uzyskane informacje
write_xlsx(albums_df, "Albums.xlsx", col_names = TRUE)

# Ponowne odczytanie data frame'a
albums_df <- read_xlsx("Albums.xlsx")
albums_df <- data.frame(albums_df)

# Tworzenie pustego data frame'a do przechowywania informacji o cechach utwor?w
audio_url <- "https://api.spotify.com/v1/audio-features?"
audio_df <- data.frame(track_id = character(),
                       danceability = numeric(),
                       energy = numeric(),
                       key = integer(),
                       loudness = numeric(),
                       mode = logical(),
                       speechiness = numeric(),
                       acousticness = numeric(),
                       instrumentalness = numeric(),
                       liveness = numeric(),
                       valence = numeric(),
                       tempo = numeric(),
                       duration_ms = numeric(),
                       time_signature = integer(),
                       stringsAsFactors = FALSE)

# P?tla przez ??dania API a? do osi?gni?cia po??danej liczby artyst?w
for (i in 1:length(songs_ids)) {
  ID <- songs_ids[i]
  params4 <- list(ids=ID)
  
  # Tworzenie ??dania API
  res4 <- GET(url = audio_url,
              query = params4,
              add_headers(Authorization = paste("Bearer", access_token)))
  # Wyci?gnij informacje o utworze z odpowiedzi API
  audio_features <- content(res4)$audio_features
  audio_info <- data.frame(track_id = sapply(audio_features, function(x) x$id),
                           danceability = sapply(audio_features, function(x) x$danceability),
                           energy = sapply(audio_features, function(x) x$energy),
                           key = sapply(audio_features, function(x) x$key),
                           loudness = sapply(audio_features, function(x) x$loudness),
                           mode = sapply(audio_features, function(x) x$mode),
                           speechiness = sapply(audio_features, function(x) x$speechiness),
                           acousticness = sapply(audio_features, function(x) x$acousticness),
                           instrumentalness = sapply(audio_features, function(x) x$instrumentalness),
                           liveness = sapply(audio_features, function(x) x$liveness),
                           valence = sapply(audio_features, function(x) x$valence),
                           tempo = sapply(audio_features, function(x) x$tempo),
                           duration_ms = sapply(audio_features, function(x) x$duration_ms),
                           time_signature = sapply(audio_features, function(x) x$time_signature),
                           stringsAsFactors = FALSE)
  
  # Dodawanie informacji do data frame'a
  audio_df <- rbind(audio_df, audio_info)
  if (i%%5==0){
    Sys.sleep(1)
  }
}

# Tworzenie pliku xlsx zawieraj?cego uzyskane informacje
write_xlsx(audio_df, "Audio_features.xlsx", col_names = TRUE)

# Ponowne odczytanie data frame'a
audio_df <- read_xlsx("Audio_features.xlsx")
audio_df <- data.frame(audio_df)

# Dwa utwory nie mia?y informacji o cechach audio, wi?c zosta?y usuni?te z innych data frame'?w
missing_songs <- c("5N8KsYJ8IEMQVw33JvytW8","53PHCS3cgaXm3JCIlAw6z8")
missing_artists <- c("2ZqIdeQMZpsr1jAruu22fI", "6rUkUd35nP2xoKO46sKPqs")

songs_df <- songs_df[!(songs_df$track_id=="5N8KsYJ8IEMQVw33JvytW8" | songs_df$track_id=="53PHCS3cgaXm3JCIlAw6z8"),]
artists_df <- artists_df[!(artists_df$artist_id=="2ZqIdeQMZpsr1jAruu22fI" | artists_df$artist_id=="6rUkUd35nP2xoKO46sKPqs"),]
albums_df <- albums_df[!(albums_df$artist_id=="2ZqIdeQMZpsr1jAruu22fI" | albums_df$artist_id=="6rUkUd35nP2xoKO46sKPqs"),]

#--------------------------------------------------------------------------------------------------------------------------

songs_df <- read_xlsx("Songs.xlsx")
songs_df <- data.frame(songs_df)
artists_df <- read_xlsx("Artists.xlsx")
artists_df <- data.frame(artists_df)
albums_df <- read_xlsx("Albums.xlsx")
albums_df <- data.frame(albums_df)
audio_df <- read_xlsx("Audio_features.xlsx")
audio_df <- data.frame(audio_df)

# ??czenie wszystkich data frame'?w w jeden
final <- cbind(songs_df, artists_df, albums_df, audio_df)
colnames(final)
final <- final[,-c(9,10,13,14,16)]
write_xlsx(final, "Song_popularity_dataset.xlsx", col_names = TRUE)


#------------------------------------------------------------------------------------------------------------------
# Wczytanie pliku z danymi
rm(list=ls())
final <- read_xlsx("Song_popularity_dataset.xlsx")
final <- data.frame(final)
colnames(final)

#Usuni?cie zduplikowanych utwor?w
final <- final[!duplicated(final$track_name), ]
#final$release_date <- as.Date(final$release_date, "%Y-%m-%d")
#final <- final[is.na(final$release_date)==FALSE,]
#Histogram zmiennej zale?nej song_popularity
bw_pop <- bw.nrd(x=final$song_popularity)
ggplot(final, aes(x=song_popularity))+
  geom_histogram(binwidth = bw_pop, color = "black", fill="lightblue")+
  theme_get()+
  scale_color_brewer(palette = "Set1")+
  labs(x = "song_popularity",
       y = "Liczba obserwacji")+
  theme(text = element_text(size = 30))
summary(final$song_popularity)

# Przekszta?cenie zmiennej obja?nianej w zmienn? binarn?
final$song_popularity <- ifelse(final$song_popularity <= 70,0,1)
table(final$song_popularity)

#Dodanie zmiennej year
final$year <- as.numeric(substr(final$release_date,1,4))
colnames(final)

final$song_pop_f <- factor(final$song_popularity)
# Badanie zmiennych

# Badanie zmiennej artist_popularity
histogram(final$artist_popularity, breaks = 50)
summary(final$artist_popularity)
ggplot(data = final, aes(x = artist_popularity, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej album_popularity
histogram(final$album_popularity, breaks = 50)
summary(final$album_popularity)
ggplot(data = final, aes(x = album_popularity, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej followers
histogram(final$followers, breaks = 50)
summary(final$followers)
final <- final[final$followers!=0,]
final$followers <- final$followers/1000000
ggplot(data = final, aes(x = followers, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej tempo
histogram(final$tempo, breaks = 50)
summary(final$tempo)
final <- final[final$tempo!=0,]
ggplot(data = final, aes(x = tempo, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej instrumentalness
histogram(final$instrumentalness, breaks = 50)
summary(final$instrumentalness)
zeros_instr <- final[final$instrumentalness == 0,]
nrow(zeros_instr)
final$instrumentalness <- ifelse(final$instrumentalness <= 0.5,0,1)
table(final$instrumentalness)
instr_f <- factor(final$instrumentalness)
ggplot(final, aes(x=instr_f, y=..count../sum(..count..), fill = instr_f))+
  geom_bar(color = "black")+
  theme_get()+
  scale_color_brewer(palette = "Set1")+
  labs(x = "instrumentalness",
       y = "Procent obserwacji")+
  guides(fill=guide_legend(title="instrumentalness"))+
  theme(text = element_text(size = 15))

# Badanie zmiennej acousticness
histogram(final$acousticness, breaks = 50)
summary(final$acousticness)
ggplot(data = final, aes(x = acousticness, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej danceability
histogram(final$danceability, breaks = 50)
summary(final$danceability)
ggplot(data = final, aes(x = danceability, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej energy
histogram(final$energy, breaks = 50)
summary(final$energy)
ggplot(data = final, aes(x = energy, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej key
histogram(final$key, breaks = 50)
summary(final$key)
ggplot(data = final, aes(x = key, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej loudness
histogram(final$loudness, breaks = 50)
summary(final$loudness)
ggplot(data = final, aes(x = loudness, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))
# Usuni?cie obserwacji odstaj?cej
final <- final[final$loudness>-30,]

# Badanie zmiennej mode
histogram(final$mode)
summary(final$mode)
ggplot(data = final, aes(x = mode, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej speechiness
histogram(final$speechiness, breaks = 50)
summary(final$speechiness)
ggplot(data = final, aes(x = speechiness, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))
histogram(final$speechiness, breaks = 50, xlim = c(0.5,1), ylim = c(0,1))
# Usuni?cie obserwacji odstaj?cych
final <- final[final$speechiness<=0.6,]

# Badanie zmiennej liveness
histogram(final$liveness, breaks = 50)
summary(final$liveness)
ggplot(data = final, aes(x = liveness, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))
histogram(final$liveness, breaks = 50, xlim = c(0.75,1), ylim = c(0,1))
# Usuni?cie obserwacji odstaj?cych
final <- final[final$liveness<=0.8,]

# Badanie zmiennej valence
histogram(final$valence, breaks = 50)
summary(final$valence)
ggplot(data = final, aes(x = valence, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej duration_ms
final$duration_ms <- final$duration_ms/1000
names(final)[names(final) == 'duration_ms'] <- 'duration_s'
#final <- final[final$duration_s<400,] 
histogram(final$duration_s, breaks = 50)
summary(final$duration_s)
ggplot(data = final, aes(x = duration_s, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Badanie zmiennej time_signature
histogram(final$time_signature, breaks = 50)
summary(final$time_signature)
ggplot(data = final, aes(x = time_signature, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))
nrow(final[final$time_signature==4,])

# Badanie zmiennej year
histogram(final$year, breaks = 50)
summary(final$year)
ggplot(data = final, aes(x = year, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  guides(fill=guide_legend(title="song_popularity"))+
  labs(y = "Procent obserwacji")+
  theme(text = element_text(size = 15))

# Histogram zmiennej zale?nej po transformacji
ggplot(final, aes(x=song_pop_f, y=..count../sum(..count..), fill = song_pop_f))+
  geom_bar(color = "black")+
  theme_get()+
  scale_color_brewer(palette = "Set1")+
  labs(x = "song_popularity",
       y = "Procent obserwacji")+
  guides(fill=guide_legend(title="song_popularity"))+
  theme(text = element_text(size = 15))

# Tworzenie data frame'?w potrzebnych do stworzenia wykres?w
df_plot = melt(data=final[,c(10,12,13,15,17,18,20,21,22,23,25,26)], id=c('song_pop_f'), na.rm = TRUE)
df_plot_h = df_plot[which(df_plot$variable!='song_pop_f'),]
df_plot_l = gather(final[,c(10,12,13,15,17,18,20,21,22,23,25)], na.rm = TRUE)

# Histogramy zmiennych
ggplot(data=df_plot_l, mapping=aes(x=value, fill = key))+
  geom_histogram()+
  facet_wrap(~ key, scales = "free")+
  theme(legend.position="none")+
  theme(text = element_text(size = 20))+
  xlab("Warto??")+
  ylab("Liczba")+
  ggtitle("Histogramy zmiennych obja?niaj?cych")

# Wykresy pokazuj?ce udzia? klas zmiennej zale?nej
ggplot(data = df_plot_h, aes(x = value, fill = song_pop_f)) +
  geom_bar(stat = "bin", bins = 20, position="fill", width = 0.05)+
  facet_wrap(~ variable, scales = "free")+
  #theme(legend.position="none")+
  theme(text = element_text(size = 20))+
  xlab("Warto?? zmiennej")+
  ylab("Procent obserwacji")+
  guides(fill=guide_legend(title="song_popularity"))
  

#Korelogram
corr = cor(final[,c(10,12,13,15,17,18,20,21,22,23,25)])

# Tworzenie korelogramu
ggcorrplot(corr,
           type = "lower", 
           lab = TRUE,
           lab_size = 6,
           method="square", 
           ggtheme = theme_bw(),
           legend.title = "Korelacja",
           tl.cex=20)+
  theme(plot.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

colnames(final)
model_data <- final[,-c(1,2,3,4,5,7,8,9,11,14,19,24,26)]
model_data2 <- final[,-c(1,2,3,4,5,7,8,9,10,11,14,19,24,26)]
colnames(model_data)
colnames(model_data2)

# Podzia? zbioru na ucz?cy i testowy
set.seed(2130)
train_indexes = createDataPartition(model_data2$song_popularity, p = .75, list = FALSE)
data.train = model_data2[train_indexes,]
data.test = model_data2[-train_indexes,]

model<- glm(song_popularity~., data=data.train, family=binomial)
summary(model)
pR2(model)['McFadden']
(exp(coef(model))-1)*100
VIF(model)

predicted <- predict(model, data.test, type="response")
predicted_class <- ifelse(predicted > 0.5, 1,0) #pr?g odci?cia dla klasy 1
conf_matrix <- table(predicted_class, data.test$song_popularity)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


model_step <- stepAIC(model, direction = "both", trace = FALSE, family = binomial)
summary(model_step)
pR2(model_step)['McFadden']
(exp(coef(model))-1)*100
VIF(model_step)

predicted <- predict(model_step, data.test, type="response")
predicted_class <- ifelse(predicted > 0.5, 1,0) #pr?g odci?cia dla klasy 1
conf_matrix <- table(predicted_class, data.test$song_popularity)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


#--------------------------------------------------------------------------------------------------------------

tree = rpart(song_popularity ~ ., data=data.train, method="class")#, control = list(cp=0.008, maxdepth=50))
rpart.plot(tree, tweak = 1.7, cex = 0.7)
tree$variable.importance

predicted <- predict(tree, data.test, type="class")
predicted_class <- ifelse(predicted==1, 1,0) #pr?g odci?cia dla klasy 1
conf_matrix <- table(predicted_class, data.test$song_popularity)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


# LASY LOSOWE
## Budowa (hodowla?) lasu losowego
rf <- randomForest(as.factor(song_popularity) ~ ., data = data.train)
# U?yteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego pr?bkowania jako kandydaci przy ka?dym podziale

rf

## Wp?yw zmiennych

impToPlot <- importance(rf,scale=FALSE)
imp <- c(impToPlot[-c(5),1])
imp <- c(impToPlot[-c(4),1])
dotchart(sort(imp), xlab="Istotno??", pch=16, cex = 2, xlim = c(0,200))
dotchart(sort(imp), xlab="Istotno??", pch=16, cex = 2, xlim = c(50,100))

predicted <- predict(rf, data.test, type="class")
predicted_class <- ifelse(predicted > 0.5, 1,0) #pr?g odci?cia dla klasy 1
conf_matrix <- table(predicted, data.test$song_popularity)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


library(caret)
library(pROC)

# Train SVM model
data_mod <- svm(as.factor(song_popularity) ~ ., data = data.train, probability = TRUE)

# Make predictions on the test set
data_pred_probs <- predict(data_mod, newdata = data.test, probability = TRUE)

# Extract probabilities for the positive class
data_pred_probs <- attr(data_pred_probs, "probabilities")[, 1]

# Convert response variable to factor if it's not already
if (!is.factor(data.test$song_popularity)) {
  data.test$song_popularity <- as.factor(data.test$song_popularity)
}

# Calculate ROC curve
roc_curve <- roc(response = data.test$song_popularity, predictor = data_pred_probs)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "red", lwd = 3, xlab = "False Positive Rate", ylab = "True Positive Rate")

# Calculate AUC
data_auc <- auc(roc_curve)
cat("AUC for SVM model: ", data_auc, "\n")

data_mod <- svm(as.factor(song_popularity) ~ ., data = data.train, probability = TRUE)

# Extract support vectors
support_vectors <- data.train[data_mod$index, ]

# Visualize support vectors
plot(support_vectors[, -1], col = ifelse(support_vectors$song_popularity == "X0", "blue", "red"), pch = 19,
     main = "Support Vectors")

# Extract coefficients
coefficients <- t(data_mod$coefs) %*% data_mod$SV

# Show coefficients
coefficients

nn1 <- nnet(as.factor(song_popularity) ~., data = data.train, size = 50,decay = 5e-4, maxit = 200)
plotnet(nn1, struct = struct)

# Wydrukowanie wag sieci funkcja neuralweights z pakietu NeuralNetTools
neuralweights(nn1)


# funkcja predict pozwala na podstawienie wartosci wejsciowych do sieci i obliczenie wyjsc

result<-predict(nn1,newdata=data.train, type='class')

result<-factor(as.integer(result))

confusionMatrix(result,as.factor(data.train$song_popularity))


result_val<-predict(nn1,newdata=data.test, type='class')

result_val<-factor(as.integer(result_val))

# Utworzenie macierzy pomylek funkcja confusionMatrix z pakietu caret

confusionMatrix(result_val,as.factor(data.test$song_popularity))
# Calculate ROC curve
roc_curve <- roc(response = data.test$song_popularity, predictor = as.numeric(result_val))

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "red", lwd = 3, xlab = "False Positive Rate", ylab = "True Positive Rate")

# Calculate AUC
data_auc <- auc(roc_curve)
cat("AUC for NN model: ", data_auc, "\n")


importance <- varImp(nn1)
# Wyodrębnienie istotności jako wektor oraz nazw zmiennych
importance_values <- importance$Overall
variable_names <- rownames(importance)

# Sortowanie wartości istotności oraz nazw zmiennych
sorted_indices <- order(importance_values, decreasing = TRUE)
sorted_importance_values <- importance_values[sorted_indices]
sorted_variable_names <- variable_names[sorted_indices]

# Wykres słupkowy istotności zmiennych z etykietami
barplot(sorted_importance_values, main = "Variable Importance for Neural Network", 
        names.arg = sorted_variable_names, las = 2, cex.names = 0.7)



CM <- list()
CM[["regresja logistyczna"]] <- table(ifelse(predict(model, new = data.test, type = "response") > 0.5, 1, 0), data.test$song_popularity)
### Drzewa
CM[["drzewo klasyfikacyjne"]] <- table(predict(tree, new = data.test, type = "class"), data.test$song_popularity)
### Las
CM[["las losowy"]] <- table(predict(rf, new = data.test, type = "class"), data.test$song_popularity)
CM[["SVM"]] <- table(predict(data_mod, newdata = data.test, probability = TRUE), data.test$song_popularity)
CM[["NN"]] <- table(predict(nn1,newdata=data.test, type='class'), data.test$song_popularity)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

sapply(CM, EvaluateModel)

# Obliczanie krzywych ROC dla różnych modeli
roc.0.1 <- roc(response = data.test$song_popularity, predictor = as.vector(predict(model, newdata = data.test, type = "response")))
roc.0.2 <- roc(response = data.test$song_popularity, predictor = as.vector(predict(tree, newdata = data.test, type = "prob")[, 2]))
roc.0.3 <- roc(response = data.test$song_popularity, predictor = as.vector(predict(rf, newdata = data.test, type = "prob")[, 2]))
roc.0.4 <- roc(response = data.test$song_popularity, predictor = as.vector(attr(predict(data_mod, newdata = data.test, probability = TRUE), "probabilities")[, 2]))
roc.0.5 <- roc(response = data.test$song_popularity, predictor = as.vector(as.numeric(predict(nn1, newdata = data.test, type = "raw"))))

# Wykres ROC z krzywymi dla różnych modeli
ggroc(list(
  regresja = roc.0.1, 
  drzewo = roc.0.2, 
  las = roc.0.3, 
  svm = roc.0.4, 
  nn = roc.0.5), 
  size = 1, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, size = 0.8) +
  xlab("1 - Specyficzność") +
  ylab("Czułość") +
  scale_color_discrete(name = "") +
  theme(text = element_text(size = 25))

# Obliczanie wartości AUC dla różnych modeli
preds <- list()
preds[["regresja_logistyczna"]] <- as.vector(predict(model, newdata = data.test, type = "response"))
preds[["drzewo_klasyfikacyjne"]] <- as.vector(predict(tree, newdata = data.test, type = "prob")[, 2])
preds[["las_losowy"]] <- as.vector(predict(rf, newdata = data.test, type = "prob")[, 2])
preds[["SVM"]] <- as.vector(attr(predict(data_mod, newdata = data.test, probability = TRUE), "probabilities")[, 2])
preds[["NN"]] <- as.vector(as.numeric(predict(nn1, newdata = data.test, type = "raw")))

# AUC (Area Under Curve) - pole pod krzywą ROC
for (i in seq_along(preds)) {
  pred <- preds[[i]]
  
  # Obliczanie wartości AUC za pomocą pakietu pROC
  auc_value <- auc(roc(response = data.test$song_popularity, predictor = pred))
  
  # Wydrukowanie wartości AUC
  cat(names(preds)[i], ": ", auc_value, "\n")
}



#-----------------------------------------------------------------------------------
# Podzia? zbioru na ucz?cy i testowy
set.seed(2130)
train_indexes = createDataPartition(model_data$song_popularity, p = .75, list = FALSE)
data.train = model_data[train_indexes,]
data.test = model_data[-train_indexes,]

model<- glm(song_popularity~., data=data.train, family=binomial)
summary(model)
pR2(model)['McFadden']
(exp(coef(model))-1)*100
VIF(model)

predicted <- predict(model, data.test, type="response")
predicted_class <- ifelse(predicted > 0.5, 1,0) #pr?g odci?cia dla klasy 1
conf_matrix <- table(predicted_class, data.test$song_popularity)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


#--------------------------------------------------------------------------------------------------------------

tree = rpart(song_popularity ~ ., data=data.train, method="class")#, control = list(cp=0.008, maxdepth=50))
rpart.plot(tree, tweak = 1.7, cex = 0.7)
tree$variable.importance

predicted <- predict(tree, data.test, type="class")
predicted_class <- ifelse(predicted==1, 1,0) #pr?g odci?cia dla klasy 1
conf_matrix <- table(predicted_class, data.test$song_popularity)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


# LASY LOSOWE
## Budowa (hodowla?) lasu losowego
rf <- randomForest(as.factor(song_popularity) ~ ., data = data.train)
# U?yteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego pr?bkowania jako kandydaci przy ka?dym podziale

rf

## Wp?yw zmiennych

impToPlot <- importance(rf,scale=FALSE)
imp <- c(impToPlot[-c(5),1])
imp <- c(impToPlot[-c(4),1])
dotchart(sort(imp), xlab="Istotno??", pch=16, cex = 2, xlim = c(0,200))
dotchart(sort(imp), xlab="Istotno??", pch=16, cex = 2, xlim = c(50,100))

predicted <- predict(rf, data.test, type="class")
predicted_class <- ifelse(predicted > 0.5, 1,0) #pr?g odci?cia dla klasy 1
conf_matrix <- table(predicted, data.test$song_popularity)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


library(caret)
library(pROC)

# Train SVM model
data_mod <- svm(as.factor(song_popularity) ~ ., data = data.train, probability = TRUE)

# Make predictions on the test set
data_pred_probs <- predict(data_mod, newdata = data.test, probability = TRUE)

# Extract probabilities for the positive class
data_pred_probs <- attr(data_pred_probs, "probabilities")[, 1]

# Convert response variable to factor if it's not already
if (!is.factor(data.test$song_popularity)) {
  data.test$song_popularity <- as.factor(data.test$song_popularity)
}

# Calculate ROC curve
roc_curve <- roc(response = data.test$song_popularity, predictor = data_pred_probs)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "red", lwd = 3, xlab = "False Positive Rate", ylab = "True Positive Rate")

# Calculate AUC
data_auc <- auc(roc_curve)
cat("AUC for SVM model: ", data_auc, "\n")

# Extract coefficients
coefficients <- t(data_mod$coefs) %*% data_mod$SV

# Show coefficients
coefficients

nn1 <- nnet(as.factor(song_popularity) ~., data = data.train, size = 50,decay = 5e-4, maxit = 200)
plotnet(nn1, struct = struct)

# Wydrukowanie wag sieci funkcja neuralweights z pakietu NeuralNetTools
neuralweights(nn1)


# funkcja predict pozwala na podstawienie wartosci wejsciowych do sieci i obliczenie wyjsc

result<-predict(nn1,newdata=data.train, type='class')

result<-factor(as.integer(result))

confusionMatrix(result,as.factor(data.train$song_popularity))


result_val<-predict(nn1,newdata=data.test, type='class')

result_val<-factor(as.integer(result_val))

# Utworzenie macierzy pomylek funkcja confusionMatrix z pakietu caret

confusionMatrix(result_val,as.factor(data.test$song_popularity))
# Calculate ROC curve
roc_curve <- roc(response = data.test$song_popularity, predictor = as.numeric(result_val))

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "red", lwd = 3, xlab = "False Positive Rate", ylab = "True Positive Rate")

# Calculate AUC
data_auc <- auc(roc_curve)
cat("AUC for NN model: ", data_auc, "\n")


importance <- varImp(nn1)
# Wyodrębnienie istotności jako wektor oraz nazw zmiennych
importance_values <- importance$Overall
variable_names <- rownames(importance)

# Sortowanie wartości istotności oraz nazw zmiennych
sorted_indices <- order(importance_values, decreasing = TRUE)
sorted_importance_values <- importance_values[sorted_indices]
sorted_variable_names <- variable_names[sorted_indices]

# Wykres słupkowy istotności zmiennych z etykietami
barplot(sorted_importance_values, main = "Variable Importance for Neural Network", 
        names.arg = sorted_variable_names, las = 2, cex.names = 0.7)



CM <- list()
CM[["regresja logistyczna"]] <- table(ifelse(predict(model, new = data.test, type = "response") > 0.5, 1, 0), data.test$song_popularity)
### Drzewa
CM[["drzewo klasyfikacyjne"]] <- table(predict(tree, new = data.test, type = "class"), data.test$song_popularity)
### Las
CM[["las losowy"]] <- table(predict(rf, new = data.test, type = "class"), data.test$song_popularity)
CM[["SVM"]] <- table(predict(data_mod, newdata = data.test, probability = TRUE), data.test$song_popularity)
CM[["NN"]] <- table(predict(nn1,newdata=data.test, type='class'), data.test$song_popularity)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

sapply(CM, EvaluateModel)

# Obliczanie krzywych ROC dla różnych modeli
roc.0.1 <- roc(response = data.test$song_popularity, predictor = as.vector(predict(model, newdata = data.test, type = "response")))
roc.0.2 <- roc(response = data.test$song_popularity, predictor = as.vector(predict(tree, newdata = data.test, type = "prob")[, 2]))
roc.0.3 <- roc(response = data.test$song_popularity, predictor = as.vector(predict(rf, newdata = data.test, type = "prob")[, 2]))
roc.0.4 <- roc(response = data.test$song_popularity, predictor = as.vector(attr(predict(data_mod, newdata = data.test, probability = TRUE), "probabilities")[, 2]))
roc.0.5 <- roc(response = data.test$song_popularity, predictor = as.vector(as.numeric(predict(nn1, newdata = data.test, type = "raw"))))

# Wykres ROC z krzywymi dla różnych modeli
ggroc(list(
  regresja = roc.0.1, 
  drzewo = roc.0.2, 
  las = roc.0.3, 
  svm = roc.0.4, 
  nn = roc.0.5), 
  size = 1, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, size = 0.8) +
  xlab("1 - Specyficzność") +
  ylab("Czułość") +
  scale_color_discrete(name = "") +
  theme(text = element_text(size = 25))

# Obliczanie wartości AUC dla różnych modeli
preds <- list()
preds[["regresja_logistyczna"]] <- as.vector(predict(model, newdata = data.test, type = "response"))
preds[["drzewo_klasyfikacyjne"]] <- as.vector(predict(tree, newdata = data.test, type = "prob")[, 2])
preds[["las_losowy"]] <- as.vector(predict(rf, newdata = data.test, type = "prob")[, 2])
preds[["SVM"]] <- as.vector(attr(predict(data_mod, newdata = data.test, probability = TRUE), "probabilities")[, 2])
preds[["NN"]] <- as.vector(as.numeric(predict(nn1, newdata = data.test, type = "raw")))

# AUC (Area Under Curve) - pole pod krzywą ROC
for (i in seq_along(preds)) {
  pred <- preds[[i]]
  
  # Obliczanie wartości AUC za pomocą pakietu pROC
  auc_value <- auc(roc(response = data.test$song_popularity, predictor = pred))
  
  # Wydrukowanie wartości AUC
  cat(names(preds)[i], ": ", auc_value, "\n")
}
  
