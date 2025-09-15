library(dplyr)
## creating a dataframe 
df<-read.csv("{Your directory}/time-series/dataset/opsd_germany_daily.csv",header = T,row.names = "Date")

head(df)

tail(df)

# view tabular form 
View(df)

# retrive dimension of object 
dim(df)

# check the each column in dataframe
str(df)

# summary
summary(df)


#______________________________________________________________


# without parsing date
df2<-read.csv("{your directory }/time-series/dataset/opsd_germany_daily.csv",header = T)
head(df2)
df2$Date

# look in the date column
str(df2$Date)

# convert into date formate
x<-as.Date(df2$Date)
head(x)

# create year , month and day column
year<-as.numeric(format(x,'%Y'))

month<-as.numeric(format(x,'%m'))

day<- as.numeric(format(x,'%d'))

# add columns to data frame
df3<-cbind(df2,year,month,day)

head(df3)

df3[1:2,6:7]

# give random sample
head(sample(df3,8)) # 8 no of columns

# let's create line plot of full time series of germany's
# daily electricity consumption , using the dataframe plot method

plot(df3$year,df$Consumption,type = 'l',
     xlab = "year",
     ylab = "Consumption")
max(df3$Consumption)
min(df3$Consumption)

# 2 way 
plot(df3$year,df$Consumption,type = 'l',
     main = "Time Series",
     xlab = "year",
     ylab = "consumption",
     ylim = c(800,1750),
     xlim = c(2006,2018))
#____________________________________________________________________________

## handle missing values
colSums(is.na(df3)) 
# wind : 1463 null value
# solar : 2195
#wind.solar : 2196

## total missing value in data frame
sum(is.na(df3))

# total : 5854

#_____________________________________________________________________________


# doing yearly analysis 
## goal : identify trends in electricity consumption over the year

yearly_consumption <- aggregate(Consumption ~ year , data = df3,FUN = mean)

## plot yearly average consumption
library(ggplot2)

ggplot(yearly_consumption, aes(x = year, y = Consumption, group = 1))+
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("Yearly Average Electricity Consumption in Germany") +
  xlab("Year") +
  xlim(range(df3$year))+
  ylim(range(df3$Consumption))+
  ylab("Average Consumption") +
  theme_minimal()

min_consupstion_year<-df3[df3$Consumption==min(df3$year),]
View(min_consupstion_year)
print(paste("Minimum value Consumption of electricity in Year ",min(df3$Consumption),min_consupstion_year$year))
cor(df3$Consumption,df3$year,method = 'pearson')
#___________________________________________________________________________________________________________________


## plot month average consumption
## do some stats to understand data
monthly_consumption<-aggregate(Consumption ~ month ,df3,FUN = mean)
range(df3$month)
library(dplyr)

## corvariance to know trend of the data 
correlation<-cor(df3$Consumption,df3$month,method = "pearson")
correlation
## correlation is -ve that show -ve trend low corelation 
colSums(is.na(df3))

## show relation like month increase perportion to deacrease the consumption

ggplot(monthly_consumption, aes(x = month, y = Consumption, group = 1)) +
  geom_line(color = "green") +
  geom_point(color = "orange") +
  ggtitle("Monthly Average Electricity Consumption in Germany") +
  xlab("Month") +
  ylab("Average Consumption") +
  theme_minimal()

#_______________________________________________________________________________________________________________

## daily analysis
range(df3$day)

# correlation
library(Hmisc)

rcorr(as.matrix(df3$day,df3$Consumption,type="pearson"))
# didnot understand lets try basic way to find trends
cor(day,df3$Consumption,method = "pearson")
# give -ve trend 

# aggregate consumption by day of the week 
df3$day_of_week<-as.factor(weekdays(as.Date(paste(df3$year,df3$month,df3$day,sep = "-"))))
daily_consumption<-aggregate(Consumption ~day_of_week,df3,FUN =mean)
##plot daily average
ggplot(daily_consumption,aes(x=day_of_week,y=Consumption,group = 1))+
  geom_line(color="blue")+
  geom_point(color="red")+
  ggtitle("Average electricity daily basis")+
  xlab("day of week")+
  ylab("Average Consumption")+
  theme_minimal()

## find min and max consumption yearly, monthly and daily
mx_consumption_yearly<-df3[df3$Consumption==max(df3$Consumption),]
mn_con_yearly<-df3[df3$Consumption == min(df3$Consumption), ]

mn_con_yearly
mx_consumption_yearly
mini<-c(mn_con_yearly$day_of_week,mn_con_yearly$month,mn_con_yearly$year)
maxi<-c(mx_consumption_yearly$day_of_week,mx_consumption_yearly$month,mx_consumption_yearly$year)
lab<-c("date","month","year")
result<-data.frame(mini,maxi,lab,row.names ="lab" )
write.csv(result,"trends.csv")

## check the corelation between wind, solar ,consumption
cor_matrix <- cor(df3[, c("Consumption", "Wind", "Solar")], use = "complete.obs")
cor_matrix

## visulize
library(corrplot)
corrplot(cor_matrix,method = "circle")

colnames(df3)

head(df3)

## let's do some statistics hahah

## our hypothesis : is power consumption similar all year?
### this means we have to do annova cause we have multiple group need to compare means across it.
#### steps to do that 
# null hypothesis h0 : meanse consumption is the same for all year
# alternative hypothesis h1: means at leat one year is different mean consumption 
# interpretion : if p-value <0.05 ,reject h0 (evidence of differences)

annova_test<<-aov(Consumption ~factor(year),data = df3)
summary(annova_test)

## to know significant differance we have to do tukey hsd(honeslty significant differance) : do when comparisions by pair in that case it is ideal
## there is other method like bonferroni part of corelation overall we find significant differance do when you compair any set
hsd<-TukeyHSD(annova_test)

## datable()## data frame to show how the each year have significant differance or by oberserving p-value we decide power consumption same in all year

df_hsd<-as.data.frame(hsd$`factor(year)`)

df_hsd$significant<- ifelse(df_hsd$`p adj` < 0.05, "Different","Similar")

colnames(df_hsd)

## extract similar year consumption or not similar year consumption
similar_pairs<-df_hsd[df_hsd$significant=="Similar",]
print(similar_pairs[, c("diff", "p adj", "significant")])

## alernatily different year
diff_pairs<-df_hsd[df_hsd$significant=="Different",]
print(diff_pairs[,c("diff","p adj","significant")])

# Export to csv
write.csv(similar_pairs,"similar_year.csv")
write.csv(diff_pairs,"Different_year.csv")

df4=read.csv("Different_year.csv")
df4$X


## another hypothesis: is there is relationship b/w consumption and renewable source (Wind,Solar,wind+solor)?
## this is about corelation and relationship 
## there is two way find correlation strenth of linear relationship 
## other is linear regression it tell how much it fit 
## we go with linear regression

### null hypothesis No linear relationship between consumption and renewables.
### alternative hypothesis at leat one predictor has significant effect
### it takes to interpretion (compare p-value)
colnames(df3)
model<-lm(Consumption ~ Wind+Solar,data = df3)
summary(model)

## p-value is smaller show that both predictors are statically significant

##________________________________________________________________________________________
### do one more hypothesis
## is Consumption Different Between Seasons?
## If you want to compare consumption across seasons (e.g., winter vs. summer)
## like it has group winter and summer need to annova test


#steps
#Create a season column (e.g., winter, spring, summer, fall).
#Run ANOVA or t-tests to compare means.

get_season <- function(month) {
  season <- ifelse(month %in% c(12, 1, 2), "Winter",
                   ifelse(month %in% c(3, 4, 5), "Spring",
                          ifelse(month %in% c(6, 7, 8), "Summer",
                                 ifelse(month %in% c(9, 10, 11), "Autumn", NA))))
  return(season)
}

df3$season<-get_season(df3$month)

## do the filter the values
levels(as.factor(df3$season))
head(df3)
winter<- df3 %>% filter(season=="Winter")
summer<- df3 %>% filter(season=="Summer")
spring<- df3 %>% filter(season=="Spring")
autumn<- df3 %>% filter(season=="Autumn")
dimnames(df3)
# do the test
test_result<-aov(Consumption ~ season ,data = df3 )
summary(test_result)
T_annova<-TukeyHSD(test_result)
T_annova

# here we our small palue is very small so we reject the null hypothesis that means
# we conclude the consumption are different in every season , hurray
##______________________________________________________________________________________________


## handling missing values
colSums(is.na(df3))

## choosing method for handling missing values
## for time series ( interpolation , forward /backward fill, model-based imputation, removal )
## use linear interpolation  
install.packages('zoo')
library(zoo)
df3$Wind<-na.approx(df$Wind, rule =2)
df3$Solar<-na.approx(df$Solar,rule =2)

df$Wind.Solar<-df3$Wind +df3$Solar

colnames(df3)
## to check complete.case
cor_result <- cor(df3[, c("Consumption", "Wind", "Solar")], use = "complete.obs")
print(cor_result)

colSums(is.na(df3))

## after handling missig values we fit the line to the data
model_updated<-lm(Consumption ~Wind+Solar,data = df3)
summary(model_updated)

## summary
# we got small pvalue less than 0.05 , we conclude the relationship is there 

# Intercept (1333.33): When both Wind and Solar are zero, 
# average consumption would be about 1333 units. This is the baseline demand level.

# Statistical Significance: Both Wind and Solar have very low p-values,
# meaning we are highly confident that these predictors affect consumption.

# F-statistic (64.97, p < 2.2e-16): The overall regression model is statistically significant. 
# At least one predictor (Wind or Solar) contributes meaningfully to explaining variation in Consumption.

# Model Fit (R² = 0.0288): Only about 2.9% of the variation in consumption is explained by Wind and Solar together.
# This means other factors (not included in the model) are driving the majority of variation in energy consumption.

#_______________________________________________________________________________________________________________________________
when_use<-read.csv("table.csv")
View(when_use)

### for forcasting of time series use ARIMA for adding external parameter we use ARIMAX
df3$Date<-as.Date(df3$Date)

df3<-df3 %>% arrange(Date)

ts_consumption<-ts(df3$Consumption, frequency = 365,start = c(2006,1))
## check for stationarity 
## arima requires stationary data like constant mean and variance over time 

plot(ts_consumption,main="electricity Consumption Over Time",
     ylab="consumption")

# augmented Dickey-fuller Test(ADF)
install.packages('tseries')
library(tseries)
# perform adf test
adf_test<-adf.test(ts_consumption)
print(adf_test)
# hence our data is stationary then we go to fit the data
install.packages('forecast')
library(forecast)

## fit an arima model
arima_model<-auto.arima(ts_consumption)
## summaryy
summary(arima_model)


## forecast future values

forecast_result<-forecast(arima_model,h=30)

# plot the forecastt
plot(forecast_result,main = "electricity consumption forecast",ylab = "consumption")

# evaluate the model 
accuracy(forecast_result)


#____________________________________

## validate our forecasting model
# a. check residuals : residuals should be random and normally distributed 

plot(forecast_result,include = 200) # include last 200 observations
checkresiduals(arima_model) # diagonstic plot for residuals


## modeling by ets(exponential smoothing)\
# fit an ets model and compare performance with othher 
ts_consumption1<-ts(df3$Consumption,frequency = 7,start = c(2006,1))
ets_model<-ets(ts_consumption1)
forecast_ets<-forecast(ets_model,h=30)
accuracy(forecast_ets)


## check which model has better fit 
accuracy(forecast_result)
accuracy(forecast_ets)

## compare 
# Plot ARIMA forecast
plot(forecast_result, main = "ARIMA Forecast")

# Plot ETS forecast
plot(forecast_ets, main = "ETS Forecast")

## for comparing ets better fit to data casue its rmse,mae,mape is lower than arims

### hence ets model fit give better prediction

## final forecast
final_forecast<-forecast(ets_model,h=90)

### plot the forecast
plot(final_forecast, main = "Electricity Consumption",
     ylab = "Consumption",
     xlab = "time",
     xlim=c(2000,2700))
colnames(df3)

df3$Wind.Solar<-df3$Wind+df3$Solar
head(df3)
#____________________________________________________________

