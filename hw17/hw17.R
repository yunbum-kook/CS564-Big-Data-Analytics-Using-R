##### HW17 #####
################
# Library
library(dplyr); library(readr); library(wordcloud); library(RColorBrewer); library(Rmisc);
library(tidytext); library(gutenbergr); library(igraph); library(ggraph); library(tm); library(tidyr)

# Problem 1
# (a)
oliver <- gutenberg_download(730)
tm_oliver = oliver %>%
  unnest_tokens(word, text, "ngrams", n=2) %>%
  anti_join(stop_words)

bg_oliver = tm_oliver %>%
  separate(word, c("word1", "word2"), sep=" ")

filter_bg = bg_oliver %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# (b)
count_bg = filter_bg %>%
  count(word1, word2, sort=TRUE)

graph_bg = count_bg %>% filter(n > 4) %>% graph_from_data_frame()
ar <- grid::arrow(type="closed", length=unit(.1,"inches"))
ggraph(graph_bg,layout='fr') +
  geom_edge_link(aes(edge_alpha=n), show.legend=FALSE,
                 arrow=ar, end_cap=circle(.07,"inches")) +
  geom_node_point(color="navyblue", size=2) +
  geom_node_text(aes(label=name), vjust=1, hjust=1) +
  theme_void()

# (c)
count_bg %>%
  filter(n > 4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha=n,edge_width=n),
                 edge_colour="cyan4") +
  geom_node_point(color="navyblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# (d)
AFINN = get_sentiments("afinn")
nots <- tm_oliver %>%
  separate(word, c("word1","word2"), sep=" ") %>%
  filter(word1 %in% c("not", "no", "never", "neither")) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE)
nots
