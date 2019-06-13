##### 04 Data Extracting and Loading #####

install.packages(c('data.table', 'foreign', 'xml2', 'rvest', 'XML', 'dplyr', 'gtrendsR', 'reshape2'))

######## 1. Data Import and Export ########
### (1) csv file
# Loading Dataests from the Internet
web = "https://www.jaredlander.com/data/housing.csv"
house <- read.csv(web, header=TRUE)
str(house)

# Save dataset to disk
write.csv(house, file="housing.csv", row.names=FALSE)
# Read dataset from disk
rhouse <- read.csv("housing.csv", header=TRUE)
names(rhouse)
rownames(rhouse)
str(rhouse)
table(rhouse$Boro) # Convert list into table
class(table(rhouse$Boro))
barplot(table(rhouse$Boro), col=4)

# Download csv or txt file from internet
download.file('https://www.jaredlander.com/data/wine.csv',
              destfile='wine.csv') #destfile=destination file
wine <- read.csv('wine.csv', header=TRUE)
dim(wine)
str(wine)
names(wine) # the list of whole column names
rownames(wine) # the list of whole row names
head(wine)

#### Read Big Data ####
library(data.table)
file <- "flights_sep_oct15.csv"
system.time(test1 <- read.csv(file,header=TRUE))
system.time(test2 <- fread(file, stringsAsFactors=TRUE))

dim(test1)
dim(test2)
class(test1)
class(test2)

### (2) text file
url = "https://www.jaredlander.com/data/reactionFull.txt"
dat <- read.table(url, header=TRUE)
names(dat)
str(dat)

# Save dataset to disk
write.table(dat, file="relationFull.txt", row.names=FALSE)
# Read dataset from disk
rdata1 <- read.table("relationFull.txt", header=TRUE)
names(rdata1)


download.file('https://www.jaredlander.com/data/reaction.txt','reaction.txt')
rdat2 <- read.table("reaction.txt",header=TRUE)
head(rdat2)
head(rdat2,2) # head with only 2 rows

rdat3 <- fread(url, stringsAsFactors = TRUE)
head(rdat3)

library(ggplot2)
ggplot(rdat3, aes(x=Age, y=BMI, color=Gender)) +
  geom_point() +
  stat_smooth() +
  theme_bw()

### (3) SPSS file
library(foreign)
cancer <- read.spss(file="Cancer.sav",use.value.labels=TRUE,
                    to.data.frame=TRUE)
head(cancer)
str(cancer)

######## 2. Extracting HTML Tables ########
#page 10
# visit http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
library(xml2); library(rvest);
web <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")
tbl <- html_nodes(web, "table")
head(tbl, 10)

# Extract table 3
tbl3 <- tbl %>% .[4] %>% html_table(fill=TRUE)
tbl3 # class = list

# Remove rows 1 and 24 (headings and footnotes)
table3 <- tbl3[[1]][-c(1,24),] #-c(1, 24) (1, 24 rows deleted)
head(table3,3)

# Remove all the commas
table3 = sapply(table3, FUN=function(x)
  as.character(gsub(",","", as.character(x), fixed=TRUE)))

#Rename table headings
colnames(table3) <- c("CES_Code", "Ind_Title", "Benchmark",
                      "Estimate", "Amt_Diff", "Pct_Diff")

#Create data.frame
table3_df = as.data.frame(table3)
head(table3_df)

#Barplot of Benchmark variable
par(mar=c(4.3,10,1,1))
barplot(as.numeric(table3_df[,3]), names.arg=table3_df[,2],
        horiz=TRUE, col='cornsilk', las=1,
        cex.names=0.7, xlab='Benchmark')


# (2) Scrapping HTML Tables with XML

library(XML)
u="http://apps.saferoutesinfo.org/legislation_funding/state_apportionment.cfm"
w <- htmlParse(u)
class(w)

w.table <- readHTMLTable(w, stringsAsFactors=FALSE)
w.table

# Remove all the commas
money <- sapply(w.table[[1]][,-1], FUN= function(x) 
  as.character(gsub(",", "", as.character(x), fixed=TRUE)))

#remove all the leading $ signs
money <- as.data.frame(substring(money,2), stringsAsFactors=FALSE)

class(money)
str(money)
head(money,2)

#rename the variables
names(money) <- c("Actual2005","Actual2006","Actual2007","Actual2008",
                  "Actual2009","Actual2010","Actual2011","Actual2012","Total")

money
#move the state names to the first column
money$State <- w.table[[1]][,1]
money <- money[,c(10,1:9)]

head(money,2)
tail(money,2)
str(money)
money[52,]

# total(52 row) will be excluded
money_dat <- data.frame(State=money[-52,1],
                        Total=as.numeric(money[-52,10]))

head(money_dat,4)
tail(money_dat,4)

ggplot(money_dat,aes(State,Total)) + 
  theme_bw() +
  geom_bar(stat="identity",fill="blue") + 
  theme(axis.text.x=element_text()) + 
  coord_flip()

# barplot sorted by Total
par(mar=c(4.3,6,1,1))
library(dplyr)
money2 <- money_dat %>% arrange(by=Total)
barplot(money2$Total, names.arg = money2$State,
        horiz=TRUE, col='cyan', las=1,
        cex.names = 0.7, xlab = 'Total')

######## 3. Google Documents and Analytics ########
### (1) Google Search Trend in R
library(gtrendsR)
hs_trend <- gtrends(keyword=c("Happy", "Sad"),
                    geo=c("KR", "KR"),
                    time="now 7-d")
class(hs_trend)
str(hs_trend)
head(hs_trend)
names(hs_trend)

class(hs_trend$interest_over_time)
str(hs_trend$interest_over_time)
head(hs_trend$interest_over_time)
head(hs_trend[[1]])
head(hs_trend[[3]])
head(hs_trend[[5]])
head(hs_trend[[7]])

plot(hs_trend)

library(reshape2)
hs_trend2 = dcast(hs_trend[[1]], date ~ keyword + geo, value.var = "hits")
head(hs_trend2)
names(hs_trend2)
rownames(hs_trend2) = hs_trend2$date
hs_trend2$date = NULL
head(hs_trend2)

google.trends = gtrends(c("skiing"), geo = c("CA", "NZ"),
                        gprop = "web", time = "2013-06-30 2017-06-30")[[1]]
head(google.trends)
ggplot(google.trends, aes(x=date, y=hits, fill=geo)) +
  geom_point(aes(color=geo)) +
  geom_line() +
  facet_grid( geo ~ .) +
  ggtitle("Google trends on 'skiing'") +
  theme(legend.position = "none") +
  theme_bw()
