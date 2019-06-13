##### HW16 #####
################
# Library
library(readr); library(tidytext); library(wordcloud); library(RColorBrewer); library(Rmisc); library(dplyr); library(NLP); library(tm);

# Problem 1
# (a)
web <- paste0("http://thefourthrevolution.org/wordpress")
tokens = tibble(text=read_lines(web)) %>%
  unnest_tokens(word, text, format="html")
wcorp = tokens %>% anti_join(stop_words) %>%
  count(word, sort=TRUE)
wcorp

rwords = tibble(word=c("world's", "people", "posted", "revolutions"))
wcorp = wcorp %>% anti_join(rwords)

wcorp %>% with(wordcloud(word, n, scale=c(3, 0.5), max.words=100,
                         min.freq=6, colors=brewer.pal(8, "Dark2"), random.order = FALSE))

# (b)
wcorp %>% filter(n > 5,) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat="identity", fill="deepskyblue") +
  theme_bw() + coord_flip()
  
