### HW3 ###
###########
# Library
library(ggplot2); library(gtrendsR); library(xml2); library(rvest);

# Problem 1
web <- read_html("https://en.wikipedia.org/wiki/World_Happiness_Report")
tbl <- html_nodes(web, "table")
tbl3 <- html_table(tbl[5], fill=TRUE)
df <- tbl3[[1]]
head(df, 10)

ggplot(df, aes(x=df$Score, y=df$`GDP per capita`)) +
  geom_point(color=4) +
  xlab('Score') + ylab('GDP per capita') +
  stat_smooth(level=0.95, color=3) +
  theme_bw()

# Problem 2
hs_trend <- gtrends(keyword=c("SAMSUNG", "삼성", "LG", "엘지"),
                    geo=c("KR", "KR"),
                    time="today 12-m")

trend_df <- hs_trend$interest_over_time
index_samsung <- which(trend_df$keyword=="삼성")
index_lg <- which(trend_df$keyword=="엘지")
trend_df$keyword[index_samsung] = rep("SAMSUNG (Korean)", length(index_samsung))
trend_df$keyword[index_lg] = rep("LG (Korean)", length(index_lg))
ggplot(trend_df, aes(x=date, y=hits, color=keyword)) +
  ggtitle('Prevalence in Korea') +
  xlab('Date') + ylab('Hits') +
  geom_line() +
  theme_bw()

hs_trend_us <- gtrends(keyword=c("SAMSUNG", "LG"),
                    geo=c("US", "US"),
                    time="today 12-m")
ggplot(hs_trend_us$interest_over_time, aes(x=date, y=hits, color=keyword)) +
  ggtitle('Prevalence in US') +
  xlab('Date') + ylab('Hits') +
  geom_line() +
  theme_bw()
