### HW9 ###
###########
# Library
library(maps); library(mapdata); library(googleVis);

# Problem 1
# Wide version
country = c("UK", "Ireland", "Isle of Man", "Isle of Wight", "Wales");
color = c('White','Blue', 'Yellow','Green', 'Pink')
map('worldHires',region=country[1], col=color[1], fill=TRUE)
title("The British Isles")
for (i in 2:5)
  map('worldHires',region=country[i], col=color[i], add=TRUE, fill=TRUE)
map.cities(country="UK", capitals = 1, col="black", cex=0.25)

# Narrow version
map('worldHires',region=country[1], col=color[1], fill=TRUE, xlim=c(-15,15), ylim=c(45,60))
title("The British Isles")
for (i in 2:5)
  map('worldHires',region=country[i], col=color[i], add=TRUE, fill=TRUE)
map.cities(country="UK", capitals = 1, col="black")
legend("topright", legend=country, fill=color )

# Problem 2
newdf = data.frame(Country=Population$Country,
                Population.in.millions=round(Population$Population/1e6,0),
                Rank=Population$Rank)
head(newdf)
GC <- gvisGeoChart(newdf, 'Country', 'Population.in.millions', 'Rank',
                   options = list(dataMode="regions", width=800, height=600))
GT <- gvisTable(newdf,options=list(width=300,height=340))
Geo <- gvisMerge(GC,GT,horizontal=TRUE)
plot(Geo)
