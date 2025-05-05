########## Econometrics - Manfredi Tirri & Michele Guderzo

library(moments)
library(dplyr)
library(tseries)
library(epiDisplay)
library(tidyverse)
library(car)
library(lmtest)
library(MASS)
library(olsrr)
library(haven)
library(ggplot2)
library(ggpubr)

# DATASET CLEANING

rm(list=ls())

# Import dataset
data <- read.csv("Dataset_row.csv", header=T)
#View(data)
head(data)
dim(data)
attach(data)

# Check any possible NA in our dataset
any(is.na(data))
# There aren't, ok

# Remove not chosen variables (with the exception of districts)
data <- data[-c(4, 6:62)]
#View(data)

# Adjust some values of "floor" variables
data$floor <- round(data$floor)
#View(data)

# Approximation of the variable "years"
data$year_built <- round(data$year_built, digits = 2)
#View(data)

# Subtraction to get the apartments' age
data$year_built <- 2022 - data$year_built
#View(data)

# Rename columns (we will purposely omit some special characters of the Polish
# alphabet to avoid reading problems)
colnames(data)
names(data)[4] <- "age"
names(data)[5] <- "district_Bemowo"
names(data)[6] <- "district_Bialoleka"
names(data)[7] <- "district_Bielany"
names(data)[8] <- "district_Centrum"
names(data)[9] <- "district_Metro.Wilanowska"
names(data)[10] <- "district_Mokotow"
names(data)[11] <- "district_Ochota"
names(data)[12] <- "district_Praga-Poludnie"
names(data)[13] <- "district_Praga-Polnoc"
names(data)[14] <- "district_Rembertow"
names(data)[15] <- "district_Targowek"
names(data)[16] <- "district_Ursus"
names(data)[17] <- "district_Ursynow"
names(data)[18] <- "district_Warszawa"
names(data)[19] <- "district_Wawer"
names(data)[20] <- "district_Wesola"
names(data)[21] <- "district_Wilanow"
names(data)[22] <- "district_Wola"
names(data)[23] <- "district_Wlochy"
names(data)[24] <- "district_Mazowieckie"
names(data)[25] <- "district_Srodmiescie"
names(data)[26] <- "district_Zoliborz"
#View(data)

# There are some negative values in the column "age" --> we remove them
data <- filter(data, age > 0)
#View(data)

# Remove relevant rows of not chosen districts
data <- filter(data, c(district_Centrum != "1" &
                       district_Metro.Wilanowska != "1" &
                       district_Warszawa != "1" &
                       district_Mazowieckie != "1"))

# Remove not chosen districts
data <- data[-c(8:9, 18, 24)]
#View(data)

# Approximation variable "gross_price" (2 decimal places)
data$gross_price <- round(data$gross_price, 2)
#View(data)

#-------------------------------------------------------------------------------

### STATISTICAL ANALYSIS

## Continues variables

attach(data)

#GROSS_PRICE
# Summary
summary(gross_price)
sd(gross_price)
skewness(gross_price)
kurtosis(gross_price)

# Gross Price density distribution
ggplot(data)+
    geom_density(aes(x= gross_price), fill = 'darkorange', color = 'black', alpha =.6)+
    theme_minimal()+
    stat_function(fun = dnorm, colour='blue', size=1,
                  args = list(mean=mean(gross_price),
                              sd = sd(gross_price)))+
    labs(y = "Density", x = "Gross Price")

# Q-Q plot
qqnorm(gross_price) # Empirical
qqline(gross_price, col="red") # Theoretical

# Boxplot
boxplot(gross_price, main= "Boxplot", ylab = "Gross Price")

# Shapiro-Wilk test: H0 - normality of data
shapiro.test(gross_price) # Rejection of H0 -> gross_price is not Normal

# Jarque-Bera tes: H0 - normality of data
jarque.bera.test(gross_price) # Rejection of H0 -> gross_price is not Normal

#AREA
summary(area)
sd(area)
skewness(area)
kurtosis(area)

# Area density distribution
ggplot(data)+
  geom_density(aes(x= area), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(area),
                            sd = sd(area)))+
  labs(y = "Density", x = "Area")

# Q-Q plot
qqnorm(area) # Empirical
qqline(area, col="red") # Theoretical

# Boxplot
boxplot(area, main= "Boxplot", ylab = "Area")

# Shapiro-Wilk test: H0 - normality of data
shapiro.test(area) # Rejection of H0 -> Area is not Normal

# Jarque-Bera tes: H0 - normality of data
jarque.bera.test(area) # Rejection of H0 -> Area is not Normal

#AGE
summary(age)
sd(age)
skewness(age)
kurtosis(age)

# Age density function
age_ggp <- ggplot(data)+
  geom_density(aes(x= age), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(age),
                            sd = sd(age)))+
  labs(y = "Density", x = "Age")
age_ggp

# Log(Age) density function
logage_ggp <- ggplot(data)+
  geom_density(aes(x= log(age)), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(log(age)),
                            sd = sd(log(age))))+
  labs(y = "Density", x = "Log(Age)")
logage_ggp

# Age and Log(Age) density distribution side by side
age_two_ggp <- ggarrange(age_ggp, logage_ggp,
                         nrow = 1, ncol = 2)
age_two_ggp

# Q-Q plot
qqnorm(age) # Empirical
qqline(age, col="red") # Theoretical

# Boxplot
boxplot(age, main= "Boxplot", ylab = "Age")

# Shapiro-Wilk test: H0 - normality of data
shapiro.test(age) # Rejection of H0 -> gross_price is not Normal

# Jarque-Bera tes: H0 - normality of data
jarque.bera.test(age) # Rejection of H0 -> gross_price is not Normal

#----------------------------------------------------------------------------#

# Correlation Matrix
cor_var <- data[-c(5:22)]
#View(cor_var)
cor_mat <- cor(cor_var, method = "spearman")
round(cor_mat, 2)

# Correlation tests
cor.test(age, gross_price, method="spearman")
cor.test(floor, gross_price, method="spearman")
cor.test(room_num, gross_price, method="spearman")
cor.test(area, gross_price, method="spearman")
cor.test(area, room_num, method="spearman")
cor.test(area, floor, method="spearman")
cor.test(area, age, method="spearman")
cor.test(room_num, floor, method="spearman")
cor.test(room_num, age, method="spearman")
cor.test(floor, age, method="spearman")

#scatterplot for relationship between variables
plot(gross_price ~ area, data = data)
plot(gross_price ~ age, data = data)

#-------------------------------------------------------------------------------

## Discrete variables

# Room_num (shows frequency table)
tab1(room_num, sort.group="decreasing", cum.percent=T)

# Floor
tab1(data$floor, sort.group="decreasing", cum.percent=T)

## Dummy variables

# We count the numbers of observation for each district and calculate the
# percentage

# Bemowo 
sum(district_Bemowo)
sum(district_Bemowo)/dim(data)[1]
# Bialoleka
sum(district_Bialoleka)
sum(district_Bialoleka)/dim(data)[1]
# Bielany
sum(district_Bielany)
sum(district_Bielany)/dim(data)[1]
# Mokotow
sum(district_Mokotow)
sum(district_Mokotow)/dim(data)[1]
# Ochota
sum(district_Ochota)
sum(district_Ochota)/dim(data)[1]
# Praga-Poludnie
sum(`district_Praga-Poludnie`)
sum(`district_Praga-Poludnie`)/dim(data)[1]
# Praga-Polnoc
sum(`district_Praga-Polnoc`)
sum(`district_Praga-Polnoc`)/dim(data)[1]
# Rembertow
sum(district_Rembertow)
sum(district_Rembertow)/dim(data)[1]
# Targowek
sum(district_Targowek)
sum(district_Targowek)/dim(data)[1]
# Ursus
sum(district_Ursus)
sum(district_Ursus)/dim(data)[1]
# Ursynow
sum(district_Ursynow)
sum(district_Ursynow)/dim(data)[1]
# Wawer
sum(district_Wawer)
sum(district_Wawer)/dim(data)[1]
# Wesola
sum(district_Wesola)
sum(district_Wesola)/dim(data)[1]
# Wilanow
sum(district_Wilanow)
sum(district_Wilanow)/dim(data)[1]
# Wola
sum(district_Wola)
sum(district_Wola)/dim(data)[1]
# Wlochy
sum(district_Wlochy)
sum(district_Wlochy)/dim(data)[1]
# Srodmiescie
sum(district_Srodmiescie)
sum(district_Srodmiescie)/dim(data)[1]
# Zoliborz
sum(district_Zoliborz)
sum(district_Zoliborz)/dim(data)[1]

#statistical analysis gross_price sorted by districs
by(data$gross_price, data$district_Bemowo, summary)
by(data$gross_price, data$district_Bialoleka, summary)
by(data$gross_price, data$district_Bielany, summary)
by(data$gross_price, data$district_Mokotow, summary)
by(data$gross_price, data$district_Ochota, summary)
by(data$gross_price, data$`district_Praga-Poludnie`, summary)
by(data$gross_price, data$`district_Praga-Polnoc`, summary)
by(data$gross_price, data$district_Rembertow, summary)
by(data$gross_price, data$district_Targowek, summary)
by(data$gross_price, data$district_Ursus, summary)
by(data$gross_price, data$district_Ursynow, summary)
by(data$gross_price, data$district_Wawer, summary)
by(data$gross_price, data$district_Wesola, summary)
by(data$gross_price, data$district_Wilanow, summary)
by(data$gross_price, data$district_Wola, summary)
by(data$gross_price, data$district_Wlochy, summary)
by(data$gross_price, data$district_Srodmiescie, summary)
by(data$gross_price, data$district_Zoliborz, summary)

#dim(data)

#------------------------------------------------------------------------------#
#CASE STUDY #3

#gather city centre districts and suburbs districts into one dummy variable
#and eliminate all dustricts variables
data <- mutate(data, city_centre = 
                 ifelse(data$district_Mokotow == 1 | data$district_Ochota == 1 |
                          data$district_Wola ==1 | data$district_Zoliborz==1 | data$district_Srodmiescie==1 |
                          data$`district_Praga-Polnoc`==1 | `district_Praga-Poludnie`==1,1, 0))


data <- data[-c(5:22)]
View(data)
attach(data)

#statistical analysis new dummies: city_centre
by(data$gross_price, data$city_centre, summary)

sum(city_centre)
sum(city_centre)/dim(data)[1]

#-------------------------------------------------------------------------------

#MODEL ESTIMATION
reg_1 <- lm(gross_price ~ area + room_num + floor + age + data$city_centre, data=data)
summary(reg_1)

#log-transformation of the dependent variable to get a similar behavior to the Normal distribution
log_price <- log(data$gross_price) #just to show
data$gross_price <- log_price #just to show

summary(data$gross_price) #just to show
sd(data$gross_price) #just to show
skewness(data$gross_price) #just to show
kurtosis(data$gross_price) #just to show 

reg_2 <- lm(log(gross_price) ~ area + room_num + floor + age + data$city_centre, data=data)
summary(reg_2)

reg_3 <- lm(log(gross_price) ~ area + room_num + floor + age + I(age^2) + data$city_centre, data=data)
summary(reg_3)

reg_4 <- lm(log(gross_price) ~ area + room_num + area:room_num + floor + age + I(age^2) + data$city_centre, data=data)
summary(reg_4)

reg_5 <- lm(log(gross_price) ~ log(area) + room_num + age + I(age^2) + data$city_centre, data=data)
summary(reg_5)

#BEST MODEL
reg_6 <- lm(log(gross_price) ~ area + room_num + area:room_num + floor + log(age) + data$city_centre, data=data)
summary(reg_6)

rm(reg_1, reg_2, reg_3, reg_4, reg_5)

#DIAGNOSTIC

#RESET test
resettest(reg_6, power = 2:3, type = c("fitted"))

#Normality of residuals
summary(reg_6$residuals)
skewness(reg_6$residuals)
kurtosis(reg_6$residuals)

# Histogram + normal curve
h <- hist(reg_6$residuals, col = "blue", xlab = "Residuals", freq=F, nclass = 50)
xfit <- seq(min(reg_6$residuals), max(reg_6$residuals), length=50)
yfit <- dnorm(xfit, mean=mean(reg_6$residuals), sd=sd(reg_6$residuals))
lines(xfit, yfit, col="red", lwd=2)

# q-q plot for standardized residuals
plot(reg_6, which=2)

res.std <- rstandard(reg_6)
boxplot(res.std)

#tests for Normality yield the rejection of H0
#thus residuals are non-Normal distributed
jarque.bera.test(reg_6$residuals)
shapiro.test(reg_6$residuals)

#HOMOSCEDASTICITY
# Residuals vs fitted - we should have randomly located points
plot(reg_6, which=1)

# Breusch-Pagan test (it yeilds the rejection of H0, thus there is Heteroscedasticity)
#-> H0: homoscedastic residuals
bptest(reg_6, studentize=FALSE)

# robust regression (to solve problem with Heteroscedasticity)
reg_robust <- rlm(log(gross_price) ~ area + room_num + area:room_num + floor + log(age) + data$city_centre, data=data)
summary(reg_robust)

#MULTICOLLINEARITY
#VIF-test results say that no variable should be removed from the model
#given that all results are less than 10 (only area:room_num, but it's normal)
vif(reg_robust)

#TRYING AGAIN TO VERIFY CLRM AFTER ROBUST VARIANCE-COVARIANCE MATRIX

#HOMOSCEDASTICITY
plot(reg_robust, which=1)
bptest(reg_robust, studentize=FALSE)

#RESIDUALS ANALYSIS
summary(reg_robust$residuals)
skewness(reg_robust$residuals)
kurtosis(reg_robust$residuals)

h <- hist(reg_robust$residuals, col = "blue", xlab = "Residuals", freq=F, nclass = 50)
xfit <- seq(min(reg_robust$residuals), max(reg_robust$residuals), length=50)
yfit <- dnorm(xfit, mean=mean(reg_robust$residuals), sd=sd(reg_robust$residuals))
lines(xfit, yfit, col="red", lwd=2)

plot(reg_robust, which=2)

res.std <- rstandard(reg_robust)
boxplot(res.std)

jarque.bera.test(reg_robust$residuals)
shapiro.test(reg_robust$residuals)

#RESET
resettest(reg_robust, power = 2:3, type = c("fitted"))

# 5. CHOW TEST (do flats inside city centre cost the same as flats outside?)
data$centre <- 0
data$centre[data$city_centre==1] <-1

data$other <- 0
data$other[data$city_centre==0] <-1

data$area_centre <- data$area*data$centre
data$room_centre <- data$room_num*data$centre
data$age_centre <- data$age*data$centre
data$floor_centre <- data$floor*data$centre

data$area_other <- data$area*data$other
data$room_other <- data$room_num*data$other
data$age_other <- data$age*data$other
data$floor_other <- data$floor*data$other

reg_chow <- lm(log(gross_price) ~ other + area_other + room_other + age_other + floor_other +
                 centre + area_centre + room_centre + age_centre + floor_centre -1, data=data)

linearHypothesis(reg_chow, c("other=centre", "area_centre=area_other", "age_centre=age_other",
                             "floor_centre=floor_other"))

#H0 rejected, we should estimate on subsamples

#-------------------------------------------------------------------------------

#PROBLEMS WITH DATA

# residuals 
data$resids <- residuals(reg_6)
# leverage
data$lev <- hatvalues(reg_6)
# standardized residuals
data$rstd <- rstandard(reg_6)
# Cook distance
data$cookd <- cooks.distance(reg_6)
# fitted values
data$yhat <- fitted(reg_6)

# For how many observations leverage is > 2k/n?
# threshold = 0.00430372
length(data$lev[data$lev > (2*(length(reg_6$coefficients)/nrow(data)))])
# 133 obs.

# For how many observations |stand.residuals| >2?
length(data$rstd[abs(data$rstd)>2])
# 144 obs.

# For how many observation Cook's distance is > 4/n?
# 4/n = 0.00122963
length(data$cookd[data$cookd > 4/nrow(data)])
# 155 observations

nontypical <- data[data$lev > 0.00430372 & abs(data$rstd)>2
                      & data$cookd > 0.00122963, ]

#View(nontypical)

# Cook's distance plot for subsequent observations
# abline -- adds a threshold line
plot(reg_6, which=4, cook.level=(4/nrow(data)))
abline(h=4/nrow(data), lty=2, col= "red")

# Leverage vs standardized residuals
plot(reg_6, which=5)

# or:
ols_plot_resid_lev(reg_6)

# id.method="noteworthy" identifies a couple of suspicious observations
influencePlot(reg_6, id.method="noteworthy", 
              main="Leverage and residuals", 
              sub= "Circle size is proportional to Cook D value")

nontypical_numb <- data[c(673, 1070, 1810), c(1:10)]

#View(nontypical_numb)

# COLLINEARITY
ols_vif_tol(reg_6)


