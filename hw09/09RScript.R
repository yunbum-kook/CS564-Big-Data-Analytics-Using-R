#library: maps, mapdata, googleVis, ggmap, leaflet, igraph, threejs  

library(maps)
map("usa")
map("state", boundary = FALSE, col="blue", add=TRUE)
title('USA and its States')
text(state.center$x, state.center$y, state.abb, cex=0.5)

map("world", fill=TRUE, col="bisque2")
map.axes()
map('world', regions='Australia', fill=TRUE,
    col='cyan', add=TRUE)
#capital= 1 = True, 0=False
map.cities(country = 'Australia', capitals=3, col='red')

library(mapdata) #for high resolution
map('worldHires', 'South Korea', fill=TRUE, col='greenyellow')
title('Republic of Korea')

#page6
map('worldHires',xlim=c(120,145),ylim=c(32,44))
cols = c("cyan","magenta","pink","yellow","red")
country = c("South Korea","North Korea","Japan","China","USSR")
# for loop
for(i in 1:5)
  map("worldHires",region=country[i],col=cols[i],add=TRUE,fill=TRUE)
title(main="Neighboring Countries of Korea",xlab="Longitude (E)",
      ylab="Latitude (N)")
map.axes()

#page 9
library(googleVis)
head(Population[,1:3], 3)
# a way to redefine new data frame from given data frame
class(Population)
WP = data.frame(Country=Population$Country,
                Population.in.millions=round(Population$Population/1e6,0),
                Rank=Population$Rank)
head(WP)

G1 <- gvisGeoChart(WP, 'Country', 'Population.in.millions', 'Rank',
                   options = list(dataMode="regions", width=800, height=600))
plot(G1) # connect to default browser

str(Exports)
Exports

#page 10
GM <- gvisGeoChart(Exports, "Country", "Profit", 
                   options=list(width=500,height=340))
GT <- gvisTable(Exports,options=list(width=300,height=340))
G2 <- gvisMerge(GM,GT,horizontal=TRUE)
plot(G2)

#(1) map without names
library(sp)
gadm <- readRDS("KOR_adm1.rds") 
plot(gadm)

#(2) map with names
rname <- gadm$NAME_1
class(rname)
rname
gadm$rname <- as.factor(rname)
gadm$rname
cols=rainbow(length(levels(gadm$rname)))
spplot(gadm, "rname", col.regions=cols)

#(1) murder crimes
library(ggmap)
names(crime)
# filtering function인듯?
crimem <- subset(crime[,c(5,11,16,17)],offense=="murder")
head(crimem)
class(crimem)

table(crimem$number)
qmplot(lon,lat,data=crimem, colour=I('red'),
       size=number, legend="topleft")

#(2) violent crimes
violent_crimes <- subset(crime[,c(5,16,17)], offense != "auto theft" &
                           offense != "theft" & offense != "burglary")
head(violent_crimes)
dim(violent_crimes)
# restrict to downtown
violent_crimes <- subset(violent_crimes, lon >= -95.39681 & lon <= -95.341 &
                           lat >= 29.73631  & lat <=  29.78400)
dim(violent_crimes)

theme_set(theme_bw())
qmplot(lon,lat,data=violent_crimes,geom=c("point","density2d"))

#(1) Universities in Daejeon
library(leaflet)
du <- read.csv("DaejeonAreaUniversity.csv",header=TRUE)
leaflet() %>% addTiles() %>%
  setView(lng=mean(du$LON), lat=mean(du$LAT), zoom=11) %>%
  addMarkers(lng=du$LON, lat=du$LAT, popup=du$University)

#page 19
data(world.cities)
class(world.cities)
str(world.cities)
head(world.cities)
cities <- world.cities[order(world.cities$pop, decreasing=TRUE)[1:1000],]
head(cities,20)

#page 20
value  <- 100*cities$pop / max(cities$pop)
col <- colorRampPalette(c("cyan","yellow"))(10)[floor(10*value/100)+1]
library(igraph); library(threejs)
globejs(lat=cities$lat, long=cities$long, value=value, 
        color=col, atmosphere=TRUE)

