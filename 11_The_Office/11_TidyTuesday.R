##Load packages
library(tidyverse)
library(tidytext)
library(schrute) #https://bradlindblad.github.io/schrute/articles/theoffice.html
library(lubridate)

#pairwise sentiment comparisons?
#https://www.tidytextmining.com/sentiment.html
#https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html

##Get data
office_data <- schrute::theoffice

##Data wrangling
office_dialogue<- office_data %>%
  unnest_tokens(word, text)

reduced_office_dialogue<- office_dialogue %>%
  anti_join(get_stopwords())

positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")
sentiments<- rbind(positive, negative)

reduced_office_dialogue <- reduced_office_dialogue %>%
  left_join(sentiments)

#sentiment and popularity
xx<- reduced_office_dialogue %>%
  select(season, episode, director, writer, character, imdb_rating, sentiment, air_date) %>%
  filter(!is.na(sentiment)) %>%
  filter(!is.na(air_date)) %>%
  group_by(sentiment, air_date) %>%
  summarise(sentiment_count= n()) %>%
  mutate(sentiment_count= ifelse(sentiment == "negative", -1*sentiment_count, sentiment_count))%>%
  mutate(month= month(air_date))

ggplot(xx) +
  theme_minimal() +
  geom_bar(aes(air_date, sentiment_count, fill=month), stat = "identity") +
  geom_hline(yintercept = 0, color= "red") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major = element_blank()) +
  ylab("Sentiment count") +
  xlab("Episode air date") +
  ggtitle("Time series of sentiments in The Office") +
  scale_fill_viridis_c() +
  scale_y_continuous(limits = c(-300,300))

#popularity and director/writer
xxx<- reduced_office_dialogue %>%
  select(season, episode, director, writer, character, imdb_rating, sentiment, air_date) %>%
  filter(!is.na(sentiment)) %>%
  filter(!is.na(air_date)) %>%
  separate_rows(director, sep = ";") %>%
  separate_rows(writer, sep = ";") 

x4<-xxx %>% group_by(director) %>%
  summarise(ave_imdb_rating = mean(imdb_rating)) %>%
  arrange(desc(ave_imdb_rating)) %>%
  mutate(director=as.factor(director))

x4$director <- factor(x4$director , levels = x4$director [order(x4$ave_imdb_rating)])
levels(x4$director)

ggplot(x4) +
  theme_minimal() +
  geom_point(aes(director, ave_imdb_rating)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Director") +
  ylab("Average rating")

x5<-xxx %>% group_by(writer) %>%
  summarise(ave_imdb_rating = mean(imdb_rating)) %>%
  arrange(desc(ave_imdb_rating)) %>%
  mutate(writer=as.factor(writer))
  
x5$writer <- factor(x5$writer , levels = x5$writer [order(x5$ave_imdb_rating)])
levels(x5$writer)

ggplot(x5) +
  theme_minimal() +
  geom_point(aes(writer, ave_imdb_rating)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Writer") +
  ylab("Average rating")

x4 %>% top_n(10)
x5 %>% top_n(10)
