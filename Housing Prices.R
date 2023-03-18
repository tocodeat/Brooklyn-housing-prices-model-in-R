install.packages('readr')
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("stringr")
install.packages('data.table')
install.packages('fitdistrplus')
install.packages('psych')
library('readr')
library(dplyr)
library(tidyr)
library(stringr) 
library('data.table')
library(fitdistrplus)
library('psych')

#step1:Import and prepare the data for analysis
#1.1 Bring the data into R 
data_2016 <- read_csv("/Users/ksr/Downloads/2016_brooklyn.csv")
data_2017 <- read_csv("/Users/ksr/Downloads/2017_brooklyn.csv")
data_2018 <- read_csv("/Users/ksr/Downloads/2018_brooklyn.csv")
data_2019 <- read_csv("/Users/ksr/Downloads/2019_brooklyn.csv")
data_2020 <- read_csv("/Users/ksr/Downloads/2020_brooklyn.csv")

colnames(data_2016) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
colnames(data_2017) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
colnames(data_2018) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
colnames(data_2019) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
colnames(data_2020) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

data_2016_1 <- data_2016[-c(1, 2, 3, 4), ]
data_2017_1 <- data_2017[-c(1, 2, 3, 4), ]
data_2018_1 <- data_2018[-c(1, 2, 3, 4), ]
data_2019_1 <- data_2019[-c(1, 2, 3, 4), ]
data_2020_1 <- data_2020[-c(1:7), ]

#Correcting data type of date column
data_2016_1$date <- as.Date(data_2016_1$date, "%m/%d/%Y")
data_2017_1$date <- as.Date(data_2017_1$date, "%m/%d/%y")
data_2018_1$date <- as.Date(data_2018_1$date, "%m/%d/%y")
data_2019_1$date <- as.Date(data_2019_1$date, "%m/%d/%y")
data_2020_1$date <- as.Date(data_2020_1$date, "%m/%d/%y")

#Removing special characters from price columns
data_2016_1$price <- as.numeric(gsub(",|[$]|-","",data_2016_1$price))
data_2017_1$price <- as.numeric(gsub(",|[$]|-","",data_2017_1$price))
data_2018_1$price <- as.numeric(gsub(",|[$]|-","",data_2018_1$price))
data_2019_1$price <- as.numeric(gsub(",|[$]|-","",data_2019_1$price))
data_2020_1$price <- as.numeric(gsub(",|[$]|-","",data_2020_1$price))

#Removing white spaces from bldclasssale, taxclasscurr, bldclasscurr
data_2018_1$bldclasssale <- trimws(data_2018_1$bldclasssale, which="both")
data_2016_1$taxclasscurr <- gsub("  ","",data_2016_1$taxclasscurr)
data_2017_1$taxclasscurr <- gsub(" ","",data_2017_1$taxclasscurr)
data_2016_1$bldclasscurr <- gsub("  ","",data_2016_1$bldclasscurr)
data_2017_1$bldclasscurr <- gsub(" ","",data_2017_1$bldclasscurr)

#Removing white spaces from bldclasscat
data_2016_1$bldclasscat <- gsub("  "," ",data_2016_1$bldclasscat)
data_2016_1$bldclasscat <- trimws(data_2016_1$bldclasscat, which="both")
data_2017_1$bldclasscat <- trimws(data_2017_1$bldclasscat, which="both")
data_2018_1$bldclasscat <- trimws(data_2018_1$bldclasscat, which="both")
data_2019_1$bldclasscat <- trimws(data_2019_1$bldclasscat, which="both")
data_2020_1$bldclasscat <- trimws(data_2020_1$bldclasscat, which="both")

#Removing special characters and whitespaces from landsqft
data_2016_1$landsqft <- as.numeric(gsub(",|[-]| ","",data_2016_1$landsqft))
data_2017_1$landsqft <- as.numeric(gsub(",|[-]| ","",data_2017_1$landsqft))
data_2018_1$landsqft <- as.numeric(gsub(",| ","",data_2018_1$landsqft))
data_2019_1$landsqft <- as.numeric(gsub(",| ","",data_2019_1$landsqft))
data_2020_1$landsqft <- as.numeric(gsub(",| ","",data_2020_1$landsqft))

#Removing special characters and whitespaces from grosssqft
data_2016_1$grosssqft <- as.numeric(gsub(",|[-]| ","",data_2016_1$grosssqft))
data_2017_1$grosssqft <- as.numeric(gsub(",|[-]| ","",data_2017_1$grosssqft))
data_2018_1$grosssqft <- as.numeric(gsub(",| ","",data_2018_1$grosssqft))
data_2019_1$grosssqft <- as.numeric(gsub(",| ","",data_2019_1$grosssqft))
data_2020_1$grosssqft <- as.numeric(gsub(",| ","",data_2020_1$grosssqft))

#Removing special characters and whitespaces from resunits
data_2016_1$resunits <- as.numeric(gsub(" | |[-]|   ","",data_2016_1$resunits))
data_2017_1$resunits <- as.numeric(data_2017_1$resunits)
data_2018_1$resunits <- as.numeric(gsub(" ","",data_2018_1$resunits))
data_2019_1$resunits <- as.numeric(data_2019_1$resunits)
data_2020_1$resunits <- as.numeric(data_2020_1$resunits)

#Removing special characters and whitespaces from comunits
data_2016_1$comunits <- as.numeric(gsub(" |[-]|   | ","",data_2016_1$comunits))
data_2017_1$comunits <- as.numeric(data_2017_1$comunits)
data_2018_1$comunits <- as.numeric(data_2018_1$comunits)
data_2019_1$comunits <- as.numeric(data_2019_1$comunits)
data_2020_1$comunits <- as.numeric(data_2020_1$comunits)

#Removing special characters and whitespaces from totunits
data_2016_1$totunits <- as.numeric(gsub(" | |[-]|   ","",data_2016_1$totunits))
data_2017_1$totunits <- as.numeric(data_2017_1$totunits)
data_2018_1$totunits <- as.numeric(gsub(" ","",data_2018_1$totunits))
data_2019_1$totunits <- as.numeric(data_2019_1$totunits)
data_2020_1$totunits <- as.numeric(data_2020_1$totunits)

#Dropping columns easement and aptnum 
data_2016_1 <- data_2016_1[,c(-7,-10)]
data_2017_1 <- data_2017_1[,c(-7,-10)]
data_2018_1 <- data_2018_1[,c(-7,-10)]
data_2019_1 <- data_2019_1[,c(-7,-10)]
data_2020_1 <- data_2020_1[,c(-7,-10)]

#1.2 Join the data and make it usable for analysis 
All_Data <-  rbind(data_2016_1, data_2017_1, data_2018_1, data_2019_1, data_2020_1)
#At this point there are 119351 rows

#Converting taxclasssale to correct datatype
All_Data$taxclasssale <- as.character(All_Data$taxclasssale)

#1.3 Filter the data and make transformations specific to this analysis 
to_filter<-All_Data

to_filter <- to_filter[grep('^A|^R',to_filter$bldclasssale),]
to_filter <- to_filter[to_filter$totunits == 1 & to_filter$resunits == 1,]
to_filter <- to_filter[to_filter$grosssqft>0,]
to_filter <- to_filter[!is.na(to_filter$price),]
to_filter <- to_filter[to_filter$price!=0,]
to_filter<-subset(to_filter, subset = bldclasscat!='11 SPECIAL CONDO BILLING LOTS')
#The bldclasscat is made sure to contain only single-family residences and condos
# At this point there are 13974 rows

#Step 2: EDA and feature engineering 
EDA_data<-to_filter
#histograms of dependent and transformed dependent variables
hist(EDA_data$block, breaks = 100, main = 'block distribution', xlab = 'block')
hist(EDA_data$lot, breaks = 100, main = 'lot distribution', xlab = 'lot')
hist(EDA_data$landsqft, breaks = 100, main = 'distribution of landsqft', xlab = 'landsqft')
hist(log(EDA_data$landsqft), breaks = 100, main = 'distribution of log of landsqft', xlab = 'log(landsqft)')
hist(EDA_data$grosssqft, breaks = 100, main = 'grosssqft distribution', xlab = 'grosssqft')
hist(log(EDA_data$grosssqft), breaks = 100, main = 'distribution of log of grosssqft', xlab = 'log(grosssqft)')

#Association of price with other variables
plot(EDA_data$grosssqft, EDA_data$price, main="price vs grosssqft", 
     xlab="grosssqft", ylab="price", pch=19)
plot(EDA_data$landsqft, EDA_data$price, main="price vs landsqft", 
     xlab="landsqft", ylab="price", pch=19)
plot(EDA_data$yrbuilt, EDA_data$price, main="price vs yrbuilt", 
     xlab="yrbuilt", ylab="price", pch=19)
plot(EDA_data$zip, EDA_data$price, main="price vs zip", 
     xlab="zip", ylab="price", pch=19)

# Replacing yrbuilt == 0 with the next least value
EDA_data$yrbuilt[which(EDA_data$yrbuilt == 0)]<- min(EDA_data[which(EDA_data$yrbuilt != 0), ]$yrbuilt)

# Analysis of price variable
describe(EDA_data$price)
hist(EDA_data$price, breaks = 100, main = 'Price Distribution', col = 'blue', xlab = 'Price') 
hist(log(EDA_data$price), breaks = 100, main = 'Distribution of log price', col = 'blue', xlab = 'log(price)')
plot(density(EDA_data$price), main = 'Price Density', col = 'green') 
boxplot(EDA_data$price, main = 'Boxplot of Price')

#Removing outliers in price
price_filtered_data<-EDA_data
price_filtered_data <- subset(price_filtered_data, price_filtered_data$price>100000 & price_filtered_data$price<7000000)
describe(price_filtered_data$price)
hist(price_filtered_data$price, breaks = 100, main = 'Filtered Price Distribution', col = 'blue', xlab = 'Filtered Price') 
hist(log(price_filtered_data$price), breaks = 100, main = 'Distribution of log of filtered price', col = 'blue', xlab = 'log(filtered price)')
plot(density(price_filtered_data$price), main = 'Filtered Price Density', col = 'green') 
boxplot(price_filtered_data$price, main = 'Boxplot of Filtered Price') 

## Correlation of price(with other numeric variables):
str(price_filtered_data)
cor(price_filtered_data %>% dplyr::select(5,6,9,13,14,15,18))

#checking levels of categorical variables
str(factor(price_filtered_data$borough))
str(factor(price_filtered_data$neighborhood))
str(factor(price_filtered_data$bldclasscat))
str(factor(price_filtered_data$taxclasscurr))
str(factor(price_filtered_data$bldclasscurr))
str(factor(price_filtered_data$taxclasssale))
str(factor(price_filtered_data$bldclasssale))

# transforming relevant variables to factors
data_with_factors<-price_filtered_data
names <- c('neighborhood', 'taxclasscurr', 'bldclasscurr', 'taxclasssale', 'bldclasssale', 'zip')
data_with_factors[,names] <- lapply(data_with_factors[,names], factor)
data_with_factors$bldclasscat <- as.factor(substr(data_with_factors$bldclasscat,1,2))

# creating a quarter column
require(zoo)
data_with_factors$quarter <- as.yearqtr(data_with_factors$date, format = "%Y-%m-%d")
data_with_factors$quarter <- as.factor(data_with_factors$quarter)
plot(price~quarter,data_with_factors,main='price vs quarter')

#2.2 Pre-modeling and feature engineering 
#General model
rel_vars <- dplyr::select(data_with_factors, c('taxclasssale','taxclasscurr','neighborhood','price', 'block', 'bldclasscat','lot', 'bldclasscurr', 'zip', 'landsqft', 'grosssqft', 'yrbuilt', 'bldclasssale','quarter'))
check_point <- rel_vars
lin_mod <- lm(price~.,rel_vars)
summary(lin_mod)
#Multiple R-squared:  0.6604,	Adjusted R-squared:  0.6567 ; Model DF 142

#Searching best model through forward selection and backward elimination
step(lin_mod,direction='backward')
#lm(formula = price ~ neighborhood + block + bldclasscurr + zip + landsqft + grosssqft + yrbuilt + quarter, data = rel_vars)
step(lm(price~1,rel_vars),scope=formula(lin_mod),direction='forward')
#lm(formula = price ~ neighborhood + grosssqft + bldclasscurr + quarter + zip + landsqft + yrbuilt + block, data = rel_vars)
#Both forward selection and backward elimination return the same model
summary(lm(formula = price ~ neighborhood + grosssqft + bldclasscurr + quarter + zip + landsqft + yrbuilt + block, data = rel_vars))
#Multiple R-squared:  0.6602,	Adjusted R-squared:  0.6568; 132 Model DF

#Retaining zip for geography (and leaving block) and dropping bldclasscurr from the model as the explanation it is adding doesn't seem much when compared to the model DF it is adding. Same goes for neighborhood.
#Similarly quarter is adding very little explanation for 19 model DF it is taking

#To reduce degrees of freedom, zips of 11232 and 11237 are being removed because these group sizes are less than 100
rel_vars <- rel_vars[-which(rel_vars$zip == 11232 | rel_vars$zip == 11237 | rel_vars$zip == 11233),]
rel_vars$zip <- as.factor(rel_vars$zip)

#Trying models with transformed variables
summary(lm(log(price)~zip+sqrt(landsqft)+sqrt(grosssqft)+yrbuilt, rel_vars))#Multiple R-squared:  0.62,	Adjusted R-squared:  0.6189 
summary(lm(log(price)~zip+sqrt(landsqft)+log(grosssqft)+yrbuilt, rel_vars))#Multiple R-squared:  0.6247,	Adjusted R-squared:  0.6237
summary(lm(sqrt(price)~zip+sqrt(landsqft)+log(grosssqft)+yrbuilt, rel_vars))#Multiple R-squared:  0.6396,	Adjusted R-squared:  0.6386 
summary(lm(sqrt(price)~zip+sqrt(landsqft)+sqrt(grosssqft)+yrbuilt, rel_vars)) #Multiple R-squared:  0.6461,	Adjusted R-squared:  0.6451 
#Though second and third models have better R-squared and	Adjusted R-squared values, it is the second model that has RMSE<= $450,000 and 37 DF
final_mod<-lm(log(price)~zip+sqrt(landsqft)+log(grosssqft)+yrbuilt, rel_vars)#Multiple R-squared:  0.62,	Adjusted R-squared:  0.6189 

### calculate RMSE
RMSE<-sqrt(mean((rel_vars$price - exp(predict(final_mod, rel_vars)))^2))
RMSE
# RMSE = 444802.3

#OLS Assumptions
require(lmtest)
hist(final_mod$residuals, breaks=50, xlab = 'residuals', main='distribution of residuals')
dwtest(final_mod, alternative = "two.sided")
bptest(final_mod)
ks.test(final_mod$residuals/summary(final_mod)$sigma, pnorm)
plot(final_mod$fitted.values, final_mod$residuals)
#Though the distribution of residuals look normally distributed, ks test shows that the model doesn't pass normality check
#DW test p-value<0.05. So can't say that residuals are not auto-correlated.
#Also BP test shows that the model fails homoskedasticity test

#data for linear regression
lm_data <- dplyr::select(rel_vars, c('zip','landsqft','grosssqft','yrbuilt'))

#save the model and data
saveRDS(list(model=final_mod, data=lm_data), file='ksr.RDS') 

#Additional code to answer assignment Part 2
#quarter into factors 

plot(price~quarter,check_point,main='price vs quarter')
table(check_point$quarter)

# basic linear regression
quarter.lm<-lm(price~quarter,check_point)
summary(quarter.lm)

anova(quarter.lm)

#'Hacky' method
check_point$alt.quarter <- levels(check_point$quarter)[check_point$quarter]
check_point$alt.quarter[check_point$quarter=='2020 Q4'] <- '1.2020 Q4'
summary(lm(price~alt.quarter,check_point))

#Tukey's HSD
TukeyHSD(aov(price~quarter,check_point))

(group.means <- tapply(check_point$price,check_point$quarter,mean))
group.means[20]-group.means[19]

#running a contrast
(group.sizes <- tapply(check_point$price,check_point$quarter,length))
C <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1)
(v_c <- rep(C,times=group.sizes)/rep(group.sizes,times=group.sizes))
(u_c <- v_c/sqrt(c(v_c%*%v_c)))

(F_c <- (check_point$price%*%u_c)^2/(summary(quarter.lm)$sigma^2))
pf(F_c,1,quarter.lm$df.residual,lower.tail=FALSE)
