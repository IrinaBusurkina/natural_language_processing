
library("readxl")
tweets <- read_excel("/Users/irinabusurkina/Downloads/trump-clinton.xlsx")

tw <- select(tweets, X__1, X__2, X__3, X__10, X__29, X__30, X__31, X__32)
colnames(tw) <- c("Number","text", "Timestamp", "Avg", "ConAvg", "LibAvg", "Diff", "libsovercons")
tw = tw[-c(1),]

#лематизируем
tw_words <- tw %>%
  unnest_tokens(word, text) %>%
  mutate(word_lemma = textstem::lemmatize_words(word)) %>% 
  count(Number,word_lemma, sort = TRUE)

head(tw_words, 15)

# берем стоп-стола из датасета в библиотеке tidytext
data("stop_words")
names(stop_words)[1] = "word_lemma"

tw_words <- tw_words %>%
  anti_join(stop_words)

typeof(tw_words$word_lemma)

# общее количество каждого слова во всех текстах
words_count = tw_words %>% 
  group_by(word_lemma) %>% 
  summarise(total = sum(n))

words_count %>% 
  ggplot() + 
  geom_histogram(aes(x = total)) + 
  theme_bw()
# есть редкие слова и большое количество очень частых

# сколько слов встречается почти во всех текстах
quantile(words_count$total, 0.95)

#удалим слишком редкие и наоборот, слишком распространенные
words_count_non_stop = words_count %>% 
  filter(total > 1 & total < quantile(words_count$total, 0.95))


words_count_non_stop %>% 
  ggplot() + 
  geom_histogram(aes(x = total)) + 
  theme_bw()
# так лучше


un_words_no_stop = tw_words %>% 
  filter(word_lemma %in% words_count_non_stop$word_lemma)

head(un_words_no_stop, 15)


# общее кол-во слов в каждом тексте ()
total_words <- un_words_no_stop %>%
  group_by(Number) %>%
  summarize(total = sum(n))

un_words <- left_join(un_words_no_stop, total_words)

# tf-idf
un_tfidf <- tw_words %>%
  bind_tf_idf(word_lemma, Number, n)

# приведем данные к широкому формату. создадим term-document matrix
un.tdm = un_tfidf %>%
  select(Number,word_lemma, tf_idf) %>%
  spread(word_lemma, tf_idf, fill = 0)

# делаем матрицу терм-документ
un.tdm_m = un.tdm[-(1:2)] %>% as.matrix()
rownames(un.tdm_m) = un.tdm$Number


#
res_average <- hcut(dist(un.tdm_m, method = "euclidean"), hc_method = "average", k = 5, stand = TRUE)
fviz_dend(res_average)

table(tw$ConAvg)
table(tw$LibAvg)

library(stringr)
# создадим колонку, в которой указано, упоминается ли Россия в статье
tw$trump = str_detect(tw$text, "Trump")
tw$trump = ifelse(tw$trump == "TRUE", 0, 1)

tw$america = str_detect(tw$text, "Great")
tw$america = ifelse(tw$america == "TRUE", 0, 1)

write.csv(tw, "tweets.csv")

mean(tw$LibAvg)
class(tw$ConAvg)
tw$LibAvg <- as.numeric(tw$LibAvg)
hist(tw$ConAvg)
tw$libsovercons <- as.numeric(tw$libsovercons)


cor <- cor.test(tw$LibAvg, tw$ConAvg)
cor <- cor.test(tw$ConAvg, tw$Avg)
cor.test(tw$libsovercons, tw$ConAvg)
plot(tw$ConAvg, tw$LibAvg)
abline(lm(tw$ConAvg ~ tw$LibAvg), col = "blue")

plot(tw$ConAvg, tw$LibAvg)
lines(lowess(tw$ConAvg, tw$LibAvg), col = "blue")

library("car")
scatterplot(tw$LibAvg ~tw$ConAvg)

library(lubridate)
class(tw$Timestamp)
tw$Timestamp = as.Date(tw$Timestamp)
