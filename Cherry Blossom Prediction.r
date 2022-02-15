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

get_max_temperature <- function (stationid) {
    # Return max seasonal temperatures
    # @params: stationdid
    # @return: max temperatures of each of the 4 seasons
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
  mutate(year = as.integer(format(date, "%Y")),
         month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
         season = cut(month, breaks = c(0, 2, 5, 8, 11),
                      include.lowest = TRUE,
                      labels = c("Winter", "Spring", "Summer", "Fall")),
         year = if_else(month == 0, year + 1L, year)) %>%
  group_by(year, season) %>%
  summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}

get_min_temperature <- function (stationid) {
    # Return min seasonal temperatures
    # @params: stationdid
    # @return: min temperatures of each of the 4 seasons
  ghcnd_search(stationid = stationid, var = c("tmin"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
  mutate(year = as.integer(format(date, "%Y")),
         month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
         season = cut(month, breaks = c(0, 2, 5, 8, 11),
                      include.lowest = TRUE,
                      labels = c("Winter", "Spring", "Summer", "Fall")),
         year = if_else(month == 0, year + 1L, year)) %>%
  group_by(year, season) %>%
  summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}

get_precipitation <- function (stationid, date){
    # Return daily precipitation over the year preceding a date
    # @params: stationdid, date (YYYY/MM/DD)
    # @return: precipitation of each of the 4 seasons
    year = as.integer(strftime(date, %Y))
    ghcnd_search(stationid = stationid, 
                var = c("prcp"),
                date_min = doy_to_date(year - 1L, doy), 
                date_max = date)[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
          month = as.integer(strftime(date, %m)) %% 12,
          day = as.integer(strftime(date, %d),
          year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, month, day) %>%

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
historic_temperatures <-
  tibble(location = "washingtondc", get_max_temperature("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_max_temperature("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_max_temperature("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_max_temperature("CA001108395")))


# Precipitaiton Model

# Solar Irradiance Model

# fft(z)

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
