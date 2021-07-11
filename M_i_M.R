library(tm)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)

slovar <- read.csv(".\\data\\emo_dict.csv", 
                   sep = ";",
                   encoding = "UTF-8",
                   stringsAsFactors = F) %>% 
  mutate(tag = factor(tag))

slovar$steam_words = stemDocument(slovar[[1]], "russian")

mm_full_1 <- readLines(".\\data\\mm1.txt", 
                     encoding = "UTF-8")

mm_full_2 <- readLines(".\\data\\mm2.txt", 
                       encoding = "UTF-8")

toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))

positive_or_negative <- function(x) {
  df_corpus <- VCorpus(VectorSource(x)) %>% 
    tm_map(toSpace, "–") %>%
    tm_map(toSpace, "\\,") %>% 
    tm_map(toSpace, "\\«") %>% 
    tm_map(toSpace, "\\»") %>% 
    tm_map(toSpace, "\\:") %>% 
    tm_map(toSpace, "\\?") %>% 
    tm_map(toSpace, "\\!") %>% 
    tm_map(toSpace, "\\...") %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(removeWords, stopwords("russian"))
  
  tab <- TermDocumentMatrix(df_corpus) %>% 
    as.matrix()
  
  tab <- sort(rowSums(tab), decreasing = T)
  tab <- data.frame(steam_words = names(tab),
                    freq = tab)
  
  tab_join_f <- inner_join(tab, slovar, by = "steam_words") %>% 
    group_by(tag) %>% 
    summarise(number = n())
  return(tab_join_f)
}

files <- paste('C:\\Data\\R\\content\\kontent_analiz\\data\\mm', "\\g", seq(1:33), ".txt", sep = "")

vse_glavi <- lapply(files, function(x) {
  readLines(x, encoding = "UTF-8") %>% 
    stemDocument("russian")
})

fit <- lapply(vse_glavi, positive_or_negative)

fit_df <- as.data.frame(fit) %>% 
  select(tag, number,
         paste("number", seq(1:32), sep = "."))

colnames(fit_df) <- c("tag", paste("глава", seq(1:32), sep = " "), "эпилог")

fit_df <- fit_df %>% 
  pivot_longer(-tag, names_to = "glava", values_to = "sum")

# write.table(fit_df, "fit.csv")

p <- ggplot()+
  geom_col(fit_df %>% filter(tag == "PSTV") %>%
             mutate(glava = factor(glava, levels = glava)), 
                 mapping = aes(x = glava,
                               y = sum),
                 fill = 'yellow',
                 color = 'orange') +
  geom_col(fit_df %>% filter(tag == "NGTV")%>%
             mutate(glava = factor(glava, levels = glava)),
    mapping = aes(x = glava, 
                  y = -1 * sum),
    fill = 'lightgrey',
    color = 'darkgrey')

p + ggtitle("Распределение слов с положительным и отрциательным оттенками по главам\n 
          в романе М.А. Булгакова 'Мастер и Маргарита'") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 14, hjust = 0.5, family = "sans"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

mm_corpus_2 <- VCorpus(VectorSource(mm_full_2)) %>% 
  tm_map(toSpace, "–") %>%
  tm_map(toSpace, "\\,") %>% 
  tm_map(toSpace, "\\«") %>% 
  tm_map(toSpace, "\\»") %>% 
  tm_map(toSpace, "\\:") %>% 
  tm_map(toSpace, "\\?") %>% 
  tm_map(toSpace, "\\!") %>% 
  tm_map(toSpace, "\\...") %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeWords, stopwords("russian")) %>% 
  tm_map(removeWords,
         c("это", "тебе", "чтото"))

tab_mm_2 <- TermDocumentMatrix(mm_corpus_2) %>% 
  as.matrix()

tab_mm_2 <- sort(rowSums(tab_mm_2), decreasing = T)
tab_mm_2 <- data.frame(steam_words = names(tab_mm_2),
                  freq = tab_mm_2)
dev.new(width = 1000,
        height = 1000,
        unit = "px")

wordcloud(words = tab_mm_2$steam_words, 
          freq = tab_mm_2$freq,
          min.freq = 10,
          max.words = 100,
          scale = c(3, .5), 
          random.order = F,
          colors = brewer.pal(8, "Dark2"),
          rot.per = 0)

mm_corpus_1 <- VCorpus(VectorSource(mm_full_1)) %>% 
  tm_map(toSpace, "–") %>%
  tm_map(toSpace, "\\,") %>% 
  tm_map(toSpace, "\\«") %>% 
  tm_map(toSpace, "\\»") %>% 
  tm_map(toSpace, "\\:") %>% 
  tm_map(toSpace, "\\?") %>% 
  tm_map(toSpace, "\\!") %>% 
  tm_map(toSpace, "\\...") %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeWords, stopwords("russian")) %>% 
  tm_map(removeWords,
         c("это", "очень", "чтото", "нету"))

tab_mm_1 <- TermDocumentMatrix(mm_corpus_1) %>% 
  as.matrix()

tab_mm_1 <- sort(rowSums(tab_mm_1), decreasing = T)
tab_mm_1 <- data.frame(steam_words = names(tab_mm_1),
                       freq = tab_mm_1)
dev.new(width = 1000,
        height = 1000,
        unit = "px")

wordcloud(words = tab_mm_1 $steam_words, 
          freq = tab_mm_1 $freq,
          min.freq = 10,
          max.words = 100,
          scale = c(3, .5), 
          random.order = F,
          colors = brewer.pal(8, "Dark2"),
          rot.per = 0)


