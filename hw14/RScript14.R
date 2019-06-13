## 14. Association Rule Learning
# library
library(grid); library(vcd); library(arules); library(arulesViz)

## 2. Transaction Dataset: Groceries
data(Groceries)
inspect(Groceries[1:4])

# (1) Most Frequent Items
fItem = eclat(Groceries, parameter = list(supp=0.05, maxlen=15))
sort_fItem = sort(fItem, by='support')
inspect(sort_fItem)

itemFrequencyPlot(Groceries, topN=10, type="absolute")
itemFrequencyPlot(Groceries, topN=15)

# (2) Modeling Process
rules = apriori(Groceries, parameter = list(supp=0.001, conf=0.9, maxlen=4))
rules

options(digits = 4)
lift_rules = sort(rules, by='lift') #high-lift rules
inspect(lift_rules[1:2])

conf_rules = sort(rules, by='confidence') #high-confidence rules
inspect(conf_rules[1:5])

# Create a table with Groceries dataset
dim(Groceries)
tab = crossTable(Groceries)
# diagonal entries of tab is the number of transaction for the item

# Look at first 6 rows and columns
tab[1:6, 1:6]
# Specify the rows and columns
tab['bottled beer', 'bottled beer']
tab['bottled beer', 'canned beer']

tab['bottled beer','red/blush wine']
tab['red/blush wine','red/blush wine']
48 / 189  #0.2539683

tab['white wine','white wine']
tab['bottled beer','white wine']
22 / 187  #0.1176471

# get rules that lead to buying 'bottled beer'
rules <- apriori(Groceries, parameter=list(supp=0.0015,conf=0.3),
                 appearance=list(default="lhs",rhs='bottled beer'))
rules
beer_rules = sort(rules, by='lift')
inspect(beer_rules)

# (3) Visualizing Association Rules
#plot 1
library(arules); library(arulesViz)
plot(rules, method="graph", measure='confidence', shading='lift',
     control=list(type="items"))

#plot 2
plot(rules, method="grouped", control=list(type="items"))

## 3. Tabular Dataset: Titanic
str(Titanic)
class(Titanic)
dim(Titanic)
head(as.data.frame(Titanic), 10)

# (1) Reconstructed titanic raw data
url <- "http://www.rdatamining.com/data/titanic.raw.rdata"
download.file(url, destfile="titanic.raw.RData", mode="wb")
load("titanic.raw.RData")

# (2) Association Rule Mining
library(arules)
titanic_rules <- apriori(titanic.raw, 
    parameter=list(minlen=2,supp=0.005,conf=0.8),
    appearance=list(rhs=c("Survived=No","Survived=Yes"),
    default="lhs"), control=list(verbose=FALSE))
quality(titanic_rules) <- round(quality(titanic_rules), digits=5)

titanic_rules.sort = sort(titanic_rules, by='lift')
inspect(titanic_rules.sort)

# (3) Interpreting Rules
# Removing Redundancy
inspect(titanic_rules.sort[1:2])

# (4) Visualizing Association Rules
plot(titanic_rules, method="graph", measure='lift',
     engine='interactive', shading='confidence')
plot(titanic_rules, method='grouped', measure='lift',
     shading='confidence')

