### HW10 ###
###########
# Library
library(googleVis); library(ggvis); library(dplyr);

# Problem 1
sc <- read.csv("SeoulClinic.csv", header=TRUE)
str(sc)
head(sc)

# Transpose
clinic = sc$Clinic
sc = as.data.frame(t(sc[,-1]))
colnames(sc) = clinic
################
sc$region = rownames(sc)
plot(gvisColumnChart(sc,xvar="region",options=
                       list(title="Clinics in Seoul Area Distric",
                            height=400, legend="top")))

# Problem 2
head(trees)

trees %>%
  ggvis(~Girth, ~Height, size=~Volume,
        fill:=input_select(c("red", "green", "blue")))
