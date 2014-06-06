library(jsonlite)
library(httr)

## Set Developer Forecast API key via separate R file (for API key privacy)
source('setAPIKey.R')

## Set latitude and longitude
lat = '42.7317'
long = '-73.6925'

## sets hours for lookahead
hours = 11

## Set delay between each light turn on
delay =  0

## Set trasition time for each light
transitiontime = 50

## Set time values for API calls
today = Sys.time()
yesterday = today - 60*60*24
current.hour = as.integer(as.POSIXct(format(today, "%Y-%m-%d %H:00:00")))
yesterday.hour = as.integer(as.POSIXct(format(yesterday, "%Y-%m-%d %H:00:00")))
today = as.integer(today) 
yesterday = as.integer(yesterday)
tomorrow = today + 60*60*24


## Set which parts to exclude from API call
exclude = paste('minutely', 'daily', 'alerts', 'flags', 'currently', sep = ',')

## Get weather from yesterday and predictions for today/tomorrow
weatherYesterday = fromJSON(paste("https://api.forecast.io/forecast/" , api.key , '/' , lat , ',' , long,  ',' , yesterday, '?exclude=' , exclude, sep = ''))$hourly$data
weatherToday = fromJSON(paste("https://api.forecast.io/forecast/" , api.key , '/' , lat , ',' , long, ',' , today,'?exclude=' , exclude, sep = ''))$hourly$data
weatherTomorrow =  fromJSON(paste("https://api.forecast.io/forecast/" , api.key , '/' , lat , ',' , long, ',' , tomorrow,'?exclude=' , exclude, sep = ''))$hourly$data
weather = rbind(weatherYesterday, weatherToday, weatherTomorrow)


## Get high temp from today's forecast in lookahead window starting from current hour

temp.today = weather[weather$time >= current.hour,"temperature"][1:hours]
high.today = max(temp.today, na.rm = T)

temp.yesterday = weather[weather$time >= yesterday.hour,"temperature"][1:hours]
high.yesterday = max(temp.yesterday, na.rm = T)

real.high.delta = high.today - high.yesterday

precip = max(weather[weather$time >= current.hour, "precipProbability"][1:hours], na.rm = T)

cover = mean(weather[weather$time >= current.hour, "cloudCover"][1:hours], na.rm = T)


## Determine light settings for cloud cover

light.cover = round((1 - cover)*255)

cover.message = paste('{"on":true,"ct":235,"bri":', light.cover, ',"transitiontime":', transitiontime, '}')

PUT('http://169.254.42.178/api/theknockah/lights/1/state', body = cover.message)

## Delay

Sys.sleep(delay/10)

transitiontime = transitiontime - delay

## Determine light settings for temperature

if (real.high.delta > 10) {high.delta = 10
}  else if (real.high.delta < -10) {high.delta = -10
        } else high.delta = real.high.delta

light.high.sat = round((abs(high.delta)/10)* 155) + 100

if (high.delta > 0) light.high.hue = 65280

if (high.delta <= 0) light.high.hue = 46920

high.message = paste('{"on":true,"bri":255,"hue":', light.high.hue, ',"sat":', light.high.sat, ',"transitiontime":', transitiontime, '}', sep="")

PUT('http://169.254.42.178/api/theknockah/lights/2/state', body = high.message)

## Delay

Sys.sleep(delay/10)

transitiontime = transitiontime - delay

## Determine light settings for precipitation
light.precip.sat = round(sqrt(precip)*255)

light.precip.hue = 46920

precip.message = paste('{"on":true,"bri":255,"hue":', light.precip.hue, ',"sat":', light.precip.sat, ',"transitiontime":', transitiontime, '}', sep="")

PUT('http://169.254.42.178/api/theknockah/lights/3/state', body = precip.message)

weather$time = as.POSIXct(weather$time, origin="1970-01-01")

colsToShow = c("time","precipProbability","temperature","humidity","cloudCover")


## Print used weather data to stdout
weather$time = as.POSIXct(weather$time, origin="1970-01-01")
print('WeatherData:')
weather
print('Temperature Difference:')
real.high.delta
print('Average Cloud Cover:')
paste(cover*100, '%', sep = "")
print('Chance of Rain:')
paste(precip*100, '%', sep = "")