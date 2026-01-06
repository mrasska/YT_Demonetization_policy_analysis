library(readr)
library(tidyr)
library(dplyr)

weekly_uploads <- read_delim("D:/Dropbox/Doctorat/Chapitre 2/YouNiverse/August_version/weekly_uploads_cat.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
#weekly_uploads <- read_delim("C:/Users/maria.rasskazova/Dropbox/Doctorat/Chapitre 2/YouNiverse/August_version/weekly_uploads_cat.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

View(weekly_uploads)

#complete missing week
weekly_uploads <- complete(data=weekly_uploads, week, nesting(channel))

#Duplicating id to complete it
weekly_uploads <- weekly_uploads %>% 
  mutate(id_0 = ifelse(is.na(id)==TRUE, 0, id))

#creating video category category
weekly_uploads <- weekly_uploads %>% 
  mutate(cat_people = ifelse(category == 'People & Blogs',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_ent = ifelse(category == 'Entertainment',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_educ = ifelse(category == 'Education',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_comedy = ifelse(category == 'Comedy',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_how = ifelse(category == 'Howto & Style',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_auto = ifelse(category == 'Autos & Vehicles',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_travel = ifelse(category == 'Travel & Events',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_game = ifelse(category == 'Gaming',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_science = ifelse(category == 'Science & Technology',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_activ = ifelse(category == 'Nonprofits & Activism',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_pet = ifelse(category == 'Pets & Animals',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_film = ifelse(category == 'Film & Animation',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_music = ifelse(category == 'Music',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_news = ifelse(category == 'News & Politics',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_sport = ifelse(category == 'Sports',id, 0))

weekly_uploads <- weekly_uploads %>% 
  mutate(cat_unknown = ifelse(category == 'Unknown',id, 0))

#saving dataframe into a new csv
write.csv2(weekly_uploads, file="weekly_uploads_cat_completed.csv", row.names=FALSE)