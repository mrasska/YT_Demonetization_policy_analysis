library(readr)
library(tidyr)
library(dplyr)
library(psych)
library(stargazer)
library(fixest)

week_keywords <- read_delim("file_path", delim = ";", escape_double = FALSE, trim_ws = TRUE)

### Data cleaning and variable creation ----
week_keywords <- complete(data=week_keywords, week, nesting(channel))
week_keywords <- week_keywords[, -3]

#Verifying if right count of channels and weeks
n_distinct(week_keywords$channel)
n_distinct(week_keywords$week)

sum(week_keywords$id, na.rm = TRUE)
sum(week_keywords$naf_content, na.rm = TRUE)

week_keywords <- week_keywords %>% 
  mutate(id = ifelse(is.na(id)==TRUE, 0, id))

week_keywords$percentage_naf <- (week_keywords$naf_content/week_keywords$id)*100

week_keywords$moderation_update <- ifelse(week_keywords$week>='2017-12', 1, 0)

#### Statistics ----
average <- week_keywords %>%
  group_by(moderation_update) %>%
  summarize(av = mean(percentage_naf, na.rm=TRUE))

#### Rate variable statistical summary
describe(week_keywords$percentage_naf)
describe(week_keywords$moderation_update)

### Econometric regressions ----
#### Rate variable econometric regression
lm_rate_naf <- lm(percentage_naf ~ moderation_update, 
                  data=week_keywords)
stargazer(lm_rate_naf, type='text')

### Per number of subscribers ----
chan_subs <- read_delim("file_path", delim = ";", escape_double = FALSE, trim_ws = TRUE)

keywords <- merge(week_keywords, chan_subs, all.x = TRUE,
      by.x = 'channel', by.y = 'channel')

keywords <- keywords[, -7]

write.csv2(keywords, file="week_keywords_complete.csv", row.names=FALSE)

av_type <- keywords %>%
  group_by(sub_bin_prior, moderation_update) %>%
  summarize(av = mean(percentage_naf, na.rm=TRUE))


## Less than 10k subs
df1 <-keywords[keywords$sub_bin_prior== "Less than 10k subs", ] 

describe(df1$percentage_naf)

lm_rate_naf_1 <- lm(percentage_naf ~ moderation_update, 
                  data=df1)
stargazer(lm_rate_naf_1, type='text')

## Between 10k and 100k subs
df2 <-keywords[keywords$sub_bin_prior== "Between 10k and 100k subs", ] 

describe(df2$percentage_naf)

lm_rate_naf_2 <- lm(percentage_naf ~ moderation_update, 
                    data=df2)
stargazer(lm_rate_naf_2, type='text')

## Between 100k and 1M subs
df3 <-keywords[keywords$sub_bin_prior== "Between 100k and 1M subs", ] 

describe(df3$percentage_naf)

lm_rate_naf_3 <- lm(percentage_naf ~ moderation_update, 
                    data=df3)
stargazer(lm_rate_naf_3, type='text')

## More than 1M subs
df4 <-keywords[keywords$sub_bin_prior== "More than 1M subs", ] 

describe(df4$percentage_naf)

lm_rate_naf_4 <- lm(percentage_naf ~ moderation_update, 
                    data=df4)
stargazer(lm_rate_naf_4, type='text')

