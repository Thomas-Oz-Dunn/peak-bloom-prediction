# Cherry Blossom Prediction
# Thomas Dunn
# 2/11/2022 

# Imports
library(tidyverse)
library(rnoaa)

# Mappings
stations <- ghcnd_stations()

# Helper Functions
doy_to_date <- function (year, doy) {
  strptime(paste(year, doy, sep = '-'), '%Y-%j') %>% # create date object
    strftime('%Y-%m-%d') # translate back to date string in ISO 8601 format
}

get_max_seasonal_temp <- function (stationid, date) {
    # Return maximum temperature per season over the year preceding a date
    # @params: stationdid, date (YYYY/MM/DD)
    # @return: max temperature of each of the 4 seasons
    year = as.integer(strftime(date, %Y))
    ghcnd_search(stationid = stationid, 
                var = c("tmax"),
                date_min = doy_to_date(year - 1, doy), 
                date_max = date)[[1]] %>%
    mutate(month = as.integer(strftime(date, %m)) %% 12,
            season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include_lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Autumn"))) %>%
    group_by(season)
}

get_min_seasonal_temp <- function (stationid, date) {
    # Return minimum temperature per season over the year preceding a date
    # @params: stationdid, date (YYYY/MM/DD)
    # @return: min temperature of each of the 4 seasons
    year = as.integer(strftime(date, %Y))
    ghcnd_search(stationid = stationid, 
                var = c("tmin"),
                date_min = doy_to_date(year - 1, doy), 
                date_max = date)[[1]] %>%
    mutate(month = as.integer(strftime(date, %m)) %% 12,
            season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include_lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Autumn"))) %>%
    group_by(season)
}

get_temp_day <- function (stationid, date) {

}

get_max_seasonal_prec <- function (stationid, date){

}

get_min_seasonal_prec <- function (stationid, date){

}

get_prec_day <- function (stationid, date){

}

# Read data
cherry <- read.csv("data/washingtondc.csv") %>%
  bind_rows(read.csv("data/liestal.csv")) %>%
  bind_rows(read.csv("data/kyoto.csv")) %>%
  bind_rows(read.csv("data/meteoswiss.csv")) %>%
  bind_rows(read.csv("data/south_korea.csv")) %>%

# First Order fit
ls_fit <- lm(bloom_doy ~ location * year, data = cherry, subset = year >= 1880)

# Temperature Model

# Precipitaiton Model


# Solar Irradiance Model


# Need a prediction for vancouver WITHOUT historical data. 
# Well, there is historical weather data!
# Use NOAA Covariants

# Compate the correlation factors
# weight all of the models together propotional to their accuracy

# NOAA Variables
# PRCP = Precipitation (tenths of mm)
# TMAX = Maximum temperature (tenths of degrees C)
# TMIN = Minimum temperature (tenths of degrees C)
# SN*# = Minimum soil temperature (tenths of degrees C)
# SX*# = Maximum soil temperature (tenths of degrees C) 
#	     * = ground cover
#		        0 = unknown
#		        1 = grass
#		        2 = fallow
#		        3 = bare ground
#		        4 = brome grass
#		        5 = sod
#		        6 = straw multch
#		        7 = grass muck
#		        8 = bare muck
#		  
#	    # =  soil depth  
#		        1 = 5 cm
#		        2 = 10 cm
#		        3 = 20 cm
#		        4 = 50 cm
#		        5 = 100 cm
#		        6 = 150 cm
#		        7 = 180 cm
# WESF = Water equivalent of snowfall (tenths of mm)
