### Text Mining Part2 ###
# Library
library(tidytext); library(gutenbergr); library(igraph); library(ggraph); library(reshape2); library(wordcloud);

### 3. Sentiment Analysis
# (1) The sentiments dataset
nrc_sword = get_sentiments("nrc")
nrc_sword
table(nrc_sword$sentiment)

# (2) Text Data: Jane Eyre by Charlotte Brontë
library(gutenbergr)
jane <- gutenberg_download(1260)
head(jane,2)
str(jane)

#Split a column into tokens and remove stop_words
library(dplyr); library(tidytext)
tm_jane <- jane %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tm_jane %>% top_n(5)

#replace "days,eyes,ladies" to "day,eye,lady"
tm_jane$word <- gsub("days","day",tm_jane$word)
tm_jane$word <- gsub("eyes","eye",tm_jane$word)
tm_jane$word <- gsub("ladies","lady",tm_jane$word)

# (3) Frequency Distribution of Sentiments according to Sentiment Categories
jane_sent = tm_jane %>%
  inner_join(nrc_sword, by="word") %>%
  count(word, sentiment, sort = TRUE)
jane_sent

library(ggplot2)
ggplot(jane_sent, aes(x=sentiment)) +
  geom_bar(aes(y=..count.., fill=sentiment)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Sentiment Analysis of Jane Eyre by Charlotte Brontë") +
  theme(legend.position="right") +
  ylab("Number of Words") + xlab("Sentiment Categories") +
  guides(fill=FALSE)

# (4) Comparison Word Cloud
library(reshape2); library(wordcloud);
jane_sent %>%
  acast(word~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=rainbow(10), title.size=1.5)

# (5)
posneg = get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative"))

jane_posneg = tm_jane %>%
  inner_join(posneg, by="word") %>%
  count(word, sentiment, sort=TRUE)
jane_posneg

jane_posneg %>% group_by(sentiment) %>%
  top_n(25) %>% ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill=FALSE)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Contribution to Sentiment", x=NULL) +
  coord_flip()

jane_posneg %>%
  acast(word~sentiment,value.var="n",fill=0) %>%
  comparison.cloud(colors=c("#FF4040","#00C0C0"))

### 4. Relationships between Words
library(gutenbergr)
alice <- gutenberg_download(11)
alice

# (1) Tokenizing by n-grams
library(dplyr); library(tidytext)
tm_alice <- alice %>%
  unnest_tokens(word,text,token="ngrams",n=2) %>%
  anti_join(stop_words)
tm_alice

# (2) Counting and Filtering n-grams
library(tidyr)
bg_alice <- tm_alice %>%
  separate(word,c("word1","word2"),sep=" ")
head(bg_alice,3)

filter_bg <- bg_alice %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
filter_bg %>% top_n(3)

# new bigram counts:
count_bg = filter_bg %>%
  count(word1, word2, sort=TRUE)
count_bg %>% top_n(3)

# (3) Creating graph object
graph_bg = count_bg %>%
  filter(n > 2) %>%
  graph_from_data_frame()
graph_bg

# (4) Visualizing a Network Bigrams
library(ggraph)
ar <- grid::arrow(type="closed", length=unit(.1,"inches"))
?ggraph
ggraph(graph_bg,layout='fr') +
  geom_edge_link(aes(edge_alpha=n), show.legend=FALSE,
                 arrow=ar, end_cap=circle(.07,"inches")) +
  geom_node_point(color="navyblue", size=2) +
  geom_node_text(aes(label=name), vjust=1, hjust=1) +
  theme_void()

count_bg %>%
  filter(n > 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha=n,edge_width=n),
                 edge_colour="cyan4") +
  geom_node_point(color="navyblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# (5) Sentiment-Associated Words
library(tidytext)
AFINN <- get_sentiments("afinn")
head(AFINN)

nots <- tm_alice %>%
  separate(word, c("word1","word2"), sep=" ") %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE)
nots

notes %>%
  mutate(contribution = n*score) %>%
  arrange(desc(abs(contribution))) %>%
  ggplot(aes(reorder(word2, contribution), n*score, fill=n*score>0)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  xlab("Words preceded by 'not'") +
  ylab("Sentiment score x Number of occurences") +
  coord_flip()