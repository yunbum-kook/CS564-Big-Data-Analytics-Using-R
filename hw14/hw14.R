##### HW14 #####
###############
# Library
library(grid); library(vcd); library(arules); library(arulesViz);

# Problem 1
# (a)
data(Adult)
itemFrequencyPlot(Adult, topN=12, type="absolute")

# (b)
rules = apriori(Adult, parameter=list(minlen=2))
rules_sorted = sort(rules, by='support')
inspect(rules_sorted[1:12])
plot(rules_sorted[1:12], method="graph",
     measure="confidence", shading="lift")

# (c)
tab = crossTable(Adult)
tab[1:6,1:6]
tab['age=Middle-aged', 'workclass=Private'] / tab['age=Middle-aged', 'age=Middle-aged']
