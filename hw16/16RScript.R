### Text Mining : Part 1 ###
# library
library(readr); library(tidytext); library(wordcloud); library(RColorBrewer); library(Rmisc); library(dplyr); library(NLP); library(tm);

### 1. Web Scraping and Tidy Text
#web link
#https://www.tidytextmining.com/index.html

#[step 1] Download text data from the web page
web <- paste0("https://www.voanews.com/a/text-of-trump-",
              "speech-to-south-korean-national-assembly-/4106294.html")
tokens = tibble(text=read_lines(web)) %>%
  unnest_tokens(word, text, format="html")
head(tokens, 5)

#[step 2] Cleaning and soring of text data
wcorp = tokens %>% anti_join(stop_words) %>%
  count(word, sort=TRUE)
wcorp

# removing some words
rwords <- tibble(word=c("day","line","live","voa",
                        "applause","trump","english","newscasts"))
wcorp <- wcorp %>% anti_join(rwords)

#[step 3] Plot word frequency
library(ggplot2)
wcorp[1:25,] %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat="identity",fill="deepskyblue") + 
  theme_bw() +
  coord_flip()

#[step 4] Generate the Word Cloud
library(wordcloud)
library(RColorBrewer)
wcorp %>%
  with(wordcloud(word,n,scale=c(3,0.5),max.words=100,
  colors=brewer.pal(8,"Dark2"),random.order=FALSE))

### 2. Text Mining with Multiple Text Datasets
#[step 1] Loading Text Data into R
df = file.path("./Data")
dir(df)

library(NLP); library(tm);
docs = VCorpus(DirSource(df))
summary(docs)

inspect(docs)
inspect(docs[1])
inspect(docs[[1]])

#[step 2] Cleaning and Counting Text Data
# Split a column into tokens
library(dplyr); library(tidytext)
tidy(docs)
tidy(docs) %>% unnest_tokens(word,text)
dxt <- tidy(docs) %>% unnest_tokens(word,text)
dxt %>% top_n(4)
dxt$word <- gsub("technologies","technology",dxt$word)
dxt$word <- gsub("sites","site",dxt$word)
dxt$word <- gsub('[[:digit:]]+', '', dxt$word)

#data(word, n)
dxta = dxt %>% anti_join(stop_words) %>%
  count(word, sort=TRUE)
dxta

#data(id, word, n)
dxtb = dxt %>% anti_join(stop_words) %>%
  group_by(id) %>% count(word, sort=TRUE)
dxtb %>% head(4)

#[step 3] Visualizing Word Frequencies
dxta %>% filter(n >7) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat="identity", fill="blue") + theme_bw() +
  coord_flip()

dxtb %>% filter(n > 5) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat="identity", fill="blue") + theme_bw() +
  coord_flip()

gbar <- function(dat,nf,ids)
{ dat %>% filter(n > nf, id==ids) %>%
    mutate(word=reorder(word, n)) %>%
    ggplot(aes(word, n)) + ggtitle(ids) +
    geom_bar(stat="identity",fill="green") +
    theme_bw() + coord_flip()
}
g1 <- gbar(dxtb,4,"Boer.txt")
g2 <- gbar(dxtb,2,"Marr.txt")
g3 <- gbar(dxtb,4,"Schwab.txt")
library(Rmisc)
multiplot(g1,g2,g3,cols=3)

#[step 4] Word Clouds
dxta %>% with(wordcloud(word, n, max.words = 60, colors = 1:60))

dxtb %>% acast(word ~ id, value.var = "n", fill=0) %>%
  comparison.cloud(random.order = FALSE,
                   colors=c("purple", "red", "darkgreen"),
                   title.size=1.5, max.words = 100)
set1 = brewer.pal(8, "Set1")
dxtb %>%
  acast(word ~id, value.var = "n", fill=0) %>%
  commonality.cloud(colors=set1, max.words=50,
                    random.order=FALSE)

#[step 5] Hierarchical Clustering of Words
corp = tm_map(docs,removePunctuation)
corp = tm_map(corp,tolower)
corp = tm_map(corp,removeWords, stopwords("english"))
corp = tm_map(corp, removeWords,c("also","can","even","every",
                                  "made","well","will"))
corp = tm_map(corp,removeNumbers)
corp = tm_map(corp, stripWhitespace)
corp = tm_map(corp,PlainTextDocument)

removePUNCT <- content_transformer(
  function(x) gsub('[[:punct:][:blank:]]+'," ",x))
corp <- tm_map(corp, removePUNCT)

dtm = DocumentTermMatrix(corp)
dtm = removeSparseTerms(dtm, 0.15)

library(cluster)
ds = dist(t(dtm),method="euclidean")
fit = hclust(ds,method="complete")
plot(fit,hang=-1); rect.hclust(fit,k=4,border=2)

#[step 6] Silhouette Extract Information From Clustering
pam0 = pam(ds,4); si0 = silhouette(pam0); summary(si0)
plot(si0,col=2:5)

#[step 7] K-Means Clustering
kfit = kmeans(ds,4)
table(kfit$cluster)
clusplot(as.matrix(ds), kfit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0,cex=1)

