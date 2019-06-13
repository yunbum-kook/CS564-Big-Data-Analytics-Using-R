library(googleVis)
library(dplyr)
library(gapminder)
library(ggplot2); 
library(plotly)
library(devtools)
library(Rcpp)
install_github('ramnathv/rCharts', force= TRUE)
library(rCharts)
require(reshape2)

head(Fruits)

bubble <- gvisBubbleChart(Fruits)
plot(bubble)
bubble <- gvisBubbleChart(Fruits,
                          xvar='Sales', yvar='Expenses')
plot(bubble)
bubble <- gvisBubbleChart(Fruits,
                          xvar='Sales', yvar='Expenses',
                          sizevar='Profit')
plot(bubble)
bubble <- gvisBubbleChart(Fruits, idvar='Fruit',
                          xvar='Sales', yvar='Expenses',
                          colorvar='Year', sizevar='Profit')
plot(bubble)

###

dat = Fruits %>% group_by(Fruit) %>%
  summarize(Sale=mean(Sales), Expense=mean(Expenses))
dat

?gvisBarChart
bar = gvisBarChart(dat)
plot(bar)
bar = gvisColumnChart(dat)
plot(bar)
bar = gvisBarChart(dat, xvar = 'Fruit', yvar= c("Sale", "Expense"))
plot(bar)
bar = gvisBarChart(dat,
                   options=list(isStacked=TRUE,
                                title="My Bar Chart",
                                gvis.editor="Edit Me"))
plot(bar)

#

head(OpenClose)

ck = gvisCandlestickChart(OpenClose)
plot(ck)
ck = gvisCandlestickChart(OpenClose, xvar = 'Weekday',
                          low='Low', open="Open", close="Close",
                          high="High", options=list(legend='none'))
plot(ck)

#

# na.omit(airquality) = NA 들어간 row를 remove
aq = na.omit(airquality) %>% select(Ozone,Temp,Month) %>%
  group_by(Month) %>%
  summarize(OzoneMean=mean(Ozone), TempMean=mean(Temp))
aq = data.frame(aq); head(aq)

Line = gvisLineChart(aq)
plot(Line)
Line = gvisLineChart(aq, xvar="Month", yvar=c("OzoneMean", "TempMean"),
                     options=list(gvis.editor="Edit me!"))
plot(Line)

#
library(ggvis)
str(mtcars)

mtcars %>%
  ggvis(~wt, ~mpg)

mtcars %>% 
  ggvis(~mpg, ~disp, stroke = ~vs) %>% 
  layer_points()

mtcars %>%
  ggvis(~wt, ~mpg, fill:="red")

mtcars %>%
  ggvis(~wt, ~mpg, fill:="red")

mtcars %>%
  ggvis(~wt, ~mpg, fill=~vs) %>%
  layer_points()

mtcars %>%
  ggvis(~mpg, ~disp, size = ~vs) %>%
  layer_points()

mtcars %>%
  ggvis(~wt, ~mpg, fill:="red", stroke:="black",
        size:=input_slider(10,100,label="point size"),
        opacity:=input_slider(0,1,label="opacity")) %>%
  layer_points()

#

iris %>% group_by(Species) %>%
  summarize(SubTotal=sum(Sepal.Length))

iris %>%
  ggvis(~Species, ~Sepal.Length)
iris %>%
  ggvis(~Species, ~Sepal.Length) %>% layer_bars()
iris %>%
  ggvis(~Species, ~Sepal.Length, fill=~Species) %>%
  layer_bars()
iris %>%
  ggvis(~Species, ~Sepal.Length, fill=~Species) %>%
  layer_bars(width=0.3)
iris %>%
  ggvis(~Species, ~Sepal.Length,
        fill:=input_select(c("red","green","blue"),
                           label="Fill Color")) %>% layer_bars()

#

iris %>%
  ggvis(~Sepal.Length, ~Sepal.Width, fill=~Species)
iris %>%
  ggvis(~Sepal.Length, ~Sepal.Width, fill=~Species,
        size:= input_slider(10,50,label="point size"),
        opacity:=input_slider(0.1,1,label="opacity")) %>%
  layer_points()

#
str(gapminder)
head(gapminder)
class(gapminder)

gap = gapminder %>%
  filter(year==1977) %>% # year==1977인 row만 필터링 됨
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
  geom_point() + scale_x_log10() + theme_bw()
ggplotly(gap)

