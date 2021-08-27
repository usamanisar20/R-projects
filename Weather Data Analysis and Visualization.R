#This dataset used in this analysis and visualization is from https://www.r-project.org/nosvn/pandoc/weatherData.html


#Libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#import dataset

library(readr)

Weatherdata=read.csv(file.choose())

Weatherdata <- read_csv("Weatherdata.csv") #Automated code from the import file option




#Show or view dataset
    View(Weatherdata)
    
    str(Weatherdata)
    
    
    #summary of dataset

summary(Weatherdata)

#Attributes

attributes(Weatherdata)

#Column names of the dataset
colnames(Weatherdata)


#Date formats

format(as.Date("2013-01-02"), "%A, %d-%b. %Y")

format(Sys.Date(), "%a %b %d")

NewDate <- as.Date(paste(as.numeric(Weatherdata$month), "01", Weatherdata$year, sep="-"), 
                   format = "%m-%d-%Y")
format(NewDate, format = "%b-%d-%Y")
fulldate =str(format(NewDate, format = "%b-%d-%Y"))

#Formatting month from int to string (abbreviation)

month.abb

head(month.abb[Weatherdata$month]) #shows the first 6 months
tail(month.abb[Weatherdata$month]) #shows the last 6 months

paste(Weatherdata$day, month.abb[Weatherdata$month], Weatherdata$year)
paste(Weatherdata$day, month.abb[Weatherdata$month])

newdates = paste(Weatherdata$month, month.abb[Weatherdata$month],Weatherdata$year)

cbind(Weatherdata, newdates) #Binding Newdate to existing database (weatherdata).

# Labelling months

Weatherdata$month = factor(Weatherdata$month,
                           labels = c("Jan","Fev","Mar","Apr",               
                                      "May","Jun","Jul","Aug","Sep",
                                      "Oct","Nov","Dec"))
#Temperature

#Frequency polygon of the temperature.
Weatherdata %>% ggplot(aes(temp)) + geom_freqpoly(bins = 50) + theme_bw()+ labs(x="Temperature ", y="Frequency",title = "Frequency of temperature")

# Frequency of the temperatures recorded.
ggplot(Weatherdata,aes(x=temp))+ geom_histogram(col="white", fill="red")

# Temperature throughout the year

qplot(Weatherdata$month[1:9600], Weatherdata$temp[1:9600],xlab='Month',ylab='temp',geom="point" , color= "red")


# Temperature throughout the first six months

qplot(Weatherdata$month[1:3650], Weatherdata$temp[1:3650],xlab='Month',ylab='temperature',geom="point" , color= "red")

# Temperature in last six months

qplot(Weatherdata$month[3650:8000], Weatherdata$temp[3650:8000],xlab='Month',ylab='temperature',geom="point" , color= "red")


#Boxplot of temperatures
Weatherdata %>% ggplot(aes(x = temp)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + scale_x_continuous(breaks = seq(0,1100,10))


#Humidity

#Frequency polygon of the humidity
Weatherdata %>% ggplot(aes(humid)) + geom_freqpoly(bins = 50) + theme_bw()+ labs(x="Humidity ", y="Frequency",title = "Frequency of the humidity")

# Frequency of the humidity recorded.
ggplot(Weatherdata,aes(x=humid)) + 
  geom_histogram(col="white", fill="orange") 

#Humidity in the first six months

qplot(Weatherdata$month[1:3650], Weatherdata$humid[1:3650],xlab='Month',ylab='Humidity',geom="point" , color= "red")

#Humidity in the last six months

qplot(Weatherdata$month[3650:8000], Weatherdata$humid[3650:8000],xlab='Month',ylab='humidity',geom="point" , color= "red")


#Boxplot of humidity
Weatherdata %>% ggplot(aes(x = humid)) + geom_boxplot(color="orange", fill="orange", alpha=0.2) + scale_x_continuous(breaks = seq(0,1100,10))

#Relationship between temperature and humidity.

ggplot(data = Weatherdata,aes(x = temp, y = humid)) + 
  geom_point(aes(color=temp)) +
  labs(title="Relation between Humidity and Temperature", x="Temperature", y="Humidity")

#Relationship between humdity and temperaure with visibility. 

ggplot(data = Weatherdata,aes(x = temp, y = humid)) + 
  geom_point(aes(color=visib)) +
  labs(title="Relation between Humidity vs Temperature and Visbility", x="Temperature", y="Humidity")

#Relationship between dewpoint and temperature

ggplot(data = Weatherdata,aes(x = temp, y = dewp)) + 
  geom_point(aes(color=temp)) +
  labs(title="Relation between Dew Point and Temperature", x="Temperture", y="Dew Point") 

#Relationship between dewpoint and humidity

ggplot(data = Weatherdata,aes(x = humid, y = dewp)) + 
  geom_point(aes(color=humid)) +
  labs(title="Relation between Dew Point and Humidity", x="Humidity", y="Dew Point") 


#Relationship between pressure and precipitation
ggplot(data = Weatherdata,aes(x = precip, y = pressure)) + 
  geom_point(aes(color=pressure)) +
  labs(title="Relation between Pressure and precipitation", x="Precipitaion", y="Pressure") 


#Relationship between wind direction and wind speed
ggplot(data = Weatherdata,aes(x = wind_speed, y = wind_dir)) + 
  geom_point(aes(color=wind_speed)) +
  labs(title="Relation between wind direction and wind speed", x="wind speed", y="wind direction") 

#Visibility

Weatherdata %>% ggplot(aes(visib)) + geom_freqpoly(bins = 50) + theme_bw()+ labs(x="visib ", y="Frequency",title = "Frequency of visibilty")


#Additional graphs

#Temperature tile graph in a day throught the year
ggplot(Weatherdata, aes(month , day)) + geom_tile(aes(fill = temp), color="white") + 
  scale_fill_gradient(low ="yellow", high = "red")

#Humidity tile graph
ggplot(Weatherdata, aes(month , day)) + geom_tile(aes(fill = humid), color="white") + 
  scale_fill_gradient(low ="yellow", high = "red")

#Dew point - tile graph
ggplot(Weatherdata, aes(month , day)) + geom_tile(aes(fill = dewp), color="white") + 
  scale_fill_gradient(low ="white", high = "blue")




