#####  07 Regression Analysis #####
# library: ggvis, nycflights13, dplyr, coefplot, psych, COUNT, 

### 1. Univariate Linear Regression ###
#######################################
str(longley)
# lm = linear model 
ur <- lm(Employed ~ GNP, data=longley)
summary(ur)

anova(ur)
qf(0.95, 1.14)

# Plot with prediction
with(longley,plot(GNP, Employed,pch=21, bg='cyan'))
lines(longley$GNP, ur$fitted.values, col='red')

# Prediction by interpolation?
predict(ur, list(GNP=300))
predict(ur, list(GNP=c(300,500)))

# Regression line with standard error band
# se in option = standard error
library(ggvis)
longley %>% ggvis(~GNP, ~Employed) %>%
  layer_points() %>%
  layer_model_predictions(model="lm",se=TRUE,stroke:="blue")

longley %>% ggvis(~GNP, ~Employed, fill:="red", stroke:="black") %>%
  layer_points()

### 2. Multivariate Linear Regression ###
#########################################
# Sample Data: flights
library(nycflights13)
flights_df <- as.data.frame(flights)
head(flights_df,2)

# Base R: multivariate regression
library(dplyr)
base_dat <- flights_df %>%
  filter(origin=="JFK", dep_delay>0, arr_delay>0)
form <- dep_delay ~ arr_delay + distance +air_time
mfit1 = lm(form, data=base_dat)
summary(mfit1)

# Coefficient Plot
library(coefplot)
coefplot(mfit1)
coefplot(mfit1, intercept=FALSE)

# Matrix Scatterplot for pairwise correlation
library(psych)
cordat <- base_dat %>% 
  select(dep_delay,arr_delay,distance,air_time)
pairs.panels(cordat)


### 3. Univariate Logistic Regression ###
#########################################
# y : dependent variable = usually categorical
# x : predictor = can be continuous, categorical etc.
Hours <- c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,
           2.75,3.00,3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50)
Pass	<- c(0,0,0,0,0,0,1,0,1,0, 1,0,1,0,1,1,1,1,1,1)
passhour <- data.frame(Hours, Pass)
head(passhour,3); tail(passhour,3)
table(passhour$Pass)

# Base R: Univariate logistic regression
out1 <- glm(Pass~Hours, family=binomial(logit), data=passhour)
summary(out1)

#page 18
b = coef(out1) # coefficient of logistic regression (array처럼 주어짐)
x = 1:5; P = 1.0/(1+exp(-b[1]-b[2]*x))
cat("Probabilities of passing exam:\n",
    round(P,3),"for",x,"hours study")

# logit(P)=0 at P=0.5 -> b[1]+b[2]*H=0, H=-b[1]/b[2]
H = -b[1] / b[2]
cat("Boundary hour to pass exam:",H,"\n")

plot(Pass~Hours, pch=20,col="blue",
     main='Fitted Logistic Regression Line with Observed Data')
lines(Hours, out1$fitted, type="l", col="red")
abline(h=0.5,v=H,col="gray")

### 4. Multivariate Logistic Regression ###
###########################################
data(badhealth, package="COUNT")
head(badhealth,2)
# Look how many unique values
sapply(badhealth, function(x) length(unique(x)))
table(badhealth$badh) #badh의 값에 빈도에 대한 정보를 정리

#page 22
library(psych)
pairs.panels(badhealth)

#Base R
mlr_fit <- glm(badh~numvisit+age, family=binomial(logit), data=badhealth)
summary(mlr_fit)

# Odds Ratio
exp(coef(mlr_fit)[2:3])

# P-value가 낮으면 poor fit (HL Test)
library(vcdExtra)
HLtest(model=mlr_fit)