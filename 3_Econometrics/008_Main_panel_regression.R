library(readr)
library(psych)
library(tidyr)
library(dplyr)
library(stargazer)
library(fixest)

uploads <- read_delim("file_path", delim = ";", escape_double = FALSE, trim_ws = TRUE)

#### Cleaning ----
# Dropping rows with channels inactive prior to moderation update
uploads <-uploads[uploads$type!= "missing", ] 

# Dropping channels with not-greenlighted categories
uploads <-uploads[uploads$main_cat!= "Film & Animation", ] 
uploads <-uploads[uploads$main_cat!= "Music", ] 
uploads <-uploads[uploads$main_cat!= "News & Politics", ] 
uploads <-uploads[uploads$main_cat!= "Sports", ] 

# Dropping channels with missing sub count
uploads <- uploads %>% drop_na(sub_bin_prior)

# Merge with week_period
week <- read_delim("file_path", delim = ";", escape_double = FALSE, trim_ws = TRUE)

uploads <- merge(uploads, week, all.x = TRUE,
                  by.x = 'week', by.y = 'week')

uploads$ln_id <- log(uploads$id_0 + 1)

### Stats ----
# Counting channels & videos
n_distinct(uploads$channel)
sum(uploads$id_0)

#Counting channels per type
types <- uploads %>%
  group_by(type) %>%
  summarize(count = n_distinct(channel))

#Counting channeks per main category
cat <- uploads %>%
  group_by(main_cat) %>%
  summarize(count = n_distinct(channel))

#Counting channeks per nb subscribers
size <- uploads %>%
  group_by(sub_bin_prior) %>%
  summarize(count = n_distinct(channel))

#Counting channeks per nb subscribers
type_size <- uploads %>%
  group_by(type, sub_bin_prior) %>%
  summarize(count = n_distinct(channel))

#Average weekly uploads by period and type
type_average<- uploads %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

### Adding variables ----
uploads <- uploads %>% 
  mutate(cat_gaming = ifelse(main_cat == 'Gaming', 1, 0))

uploads <- uploads %>% 
  mutate(cat_enter = ifelse(main_cat == 'Entertainment', 1, 0))

uploads <- uploads %>% 
  mutate(cat_people = ifelse(main_cat == 'People', 1, 0))

uploads <- uploads %>% 
  mutate(cat_activ = ifelse(main_cat == 'Nonprofits & Activism', 1, 0))

uploads <- uploads %>% 
  mutate(audience1 = ifelse(sub_bin_prior == 'Between 10k and 100k subs', 1, 0))

uploads <- uploads %>% 
  mutate(audience2 = ifelse(sub_bin_prior == 'Between 100k and 1M subs', 1, 0))

uploads <- uploads %>% 
  mutate(audience3 = ifelse(sub_bin_prior == 'More than 1M subs', 1, 0))

## Pre-trends -----
# The variable treated is interacted with each value of period (week) and Period 0 is set as a reference
# Important : treated should not be set as a factor variable in order to plot the coefficients
pre_min = feols(id_0~ pre_mod*treated + post_mod*treated | channel+week, uploads)
summary(pre_min)

pre_group1 = feols(id_0 ~ i(as.factor(p3), treated, 0) | channel+week, uploads)
summary(pre_group1)

pre_group = feols(id_0 ~ i(as.factor(p2), treated, 0) | channel+week, uploads)
summary(pre_group)

pre_group2 = feols(id_0 ~ i(period, treated, 0) | channel+week, uploads)
summary(pre_group2)
iplot(pre_group2, ref.line = 0, main="Coefficient plot for exposed channels")

# Statistical summary ----
uploads$first_moderation_update <- as.factor(uploads$first_moderation_update)
describe(uploads$id_0)
describe(uploads$ln_id)
describe(uploads$treated)
describe(uploads$first_moderation_update) 

## Econometic regressions ----
uploads$treated <- as.factor(uploads$treated)

## Two-way fixed effect = channel & week
twfe <- feols(id_0 ~ treated*first_moderation_update |channel + week, 
              data= uploads)
summary(twfe)

twfe_ln <- feols(ln_id ~ treated*first_moderation_update |channel + week, 
                data= uploads)
summary(twfe_ln)


#Average weekly uploads by period and type
type_average<- uploads %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

# Per channels' main video category ----
twfe_cat <- feols(id_0 ~ treated*first_moderation_update*(cat_gaming+cat_enter+cat_people) |channel + week, 
                  data= uploads)
summary(twfe_cat)

## Genre : Gaming ---
df_genre1=uploads[uploads$main_cat=='Gaming',]

twfe_genre1=feols(id_0 ~ treated + first_moderation_update + treated*first_moderation_update | channel+ week, 
                data=df_genre1)
summary(twfe_genre1)

#Average weekly uploads by period and type
average1<- df_genre1 %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

## Genre : Entertainment ---
df_genre2=uploads[uploads$main_cat=='Entertainment',]

twfe_genre2=feols(id_0 ~ treated + first_moderation_update + treated*first_moderation_update | channel+ week, 
                  data=df_genre2)
summary(twfe_genre2)

#Average weekly uploads by period and type
average2<- df_genre2 %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

## Genre : People and blogs ---
df_genre3=uploads[uploads$main_cat=='People',]

twfe_genre3=feols(id_0 ~ treated + first_moderation_update + treated*first_moderation_update | channel+ week, 
                  data=df_genre3)
summary(twfe_genre3)

#Average weekly uploads by period and type
average3<- df_genre3 %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))


# Channels' audience size ----
twfe_audience <- feols(id_0 ~ treated*first_moderation_update*(audience1+audience2+audience3) |channel + week, 
                       data= uploads)
summary(twfe_audience)

## Dataset - Less than 10k subs ---
df_cat1=uploads[uploads$sub_bin_prior=='Less than 10k subs',]

describe(df_cat1$id_0)

twfe_cat1=feols(id_0 ~ treated + first_moderation_update + treated*first_moderation_update | channel+ week, 
                data=df_cat1)
summary(twfe_cat1)

twfe_cat1=feols(ln_id ~ treated + first_moderation_update + treated*first_moderation_update | channel+ week, 
                data=df_cat1)
summary(twfe_cat1)

#Average weekly uploads by period and type
average1<- df_cat1 %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

## Dataset - Between 10k and 100k subs ---
df_cat2=uploads[uploads$sub_bin_prior=='Between 10k and 100k subs',]

describe(df_cat2$id_0)

twfe_cat2=feols(id_0 ~ treated + first_moderation_update + treated*first_moderation_update | channel+week, 
                data=df_cat2)
summary(twfe_cat2)

twfe_cat2=feols(ln_id ~ treated + first_moderation_update + treated*first_moderation_update | channel+week, 
                data=df_cat2)
summary(twfe_cat2)

#Average weekly uploads by period and type
average2<- df_cat2 %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

## Dataset - Between 100k and 1M subs ---
df_cat3=uploads[uploads$sub_bin_prior=='Between 100k and 1M subs',]

describe(df_cat3$id_0)

twfe_cat3=feols(id_0 ~ treated + first_moderation_update + treated*first_moderation_update |week+channel, 
                data=df_cat3)
summary(twfe_cat3)

twfe_cat3=feols(ln_id ~ treated + first_moderation_update + treated*first_moderation_update |week+channel, 
                data=df_cat3)
summary(twfe_cat3)

#Average weekly uploads by period and type
average3<- df_cat3 %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

## Dataset - Between More than 1M subs ---
df_cat4=uploads[uploads$sub_bin_prior=='More than 1M subs',]

describe(df_cat4$id_0)

twfe_cat4=feols(id_0 ~ treated + first_moderation_update + treated*first_moderation_update |week+ channel, 
                data=df_cat4)
summary(twfe_cat4)

twfe_cat4=feols(ln_id ~ treated + first_moderation_update + treated*first_moderation_update |week+ channel, 
                data=df_cat4)
summary(twfe_cat4)

df_bef4=df_cat4[(df_cat4$first_moderation_update==0) & (df_cat4$treated==1),]

#Average weekly uploads by period and type
average4<- df_cat4 %>%
  group_by(type, first_moderation_update) %>%
  summarize(count = mean(id_0))

## Couting distinc channel per each group ----
n_distinct(df_cat1$channel)
n_distinct(df_cat2$channel)
n_distinct(df_cat3$channel)
n_distinct(df_cat4$channel)

n_distinct(uploads$channel)

n_distinct(uploads$sub_bin_prior)

missing_chan <- uploads[is.na(uploads$sub_bin_prior),]
missing_chan
