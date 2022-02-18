# Cherry Blossom Prediction
# Thomas Dunn
# 2/11/2022  -> 2/28/2022

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
  summarize(tmin_avg = mean(tmin, na.rm = TRUE))
}

get_precipitation <- function (stationid){
    # Return daily precipitation 
    # @params: stationdid
    # @return: precipitation 
    ghcnd_search(stationid = stationid, 
                var = c("prcp"),
                date_min = "1950-01-01", 
                date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
          month = as.integer(strftime(date, %m)) %% 12,
          day = as.integer(strftime(date, %d),
          year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, month, day) %>%
    summarize(prec_avg = mean(prcp, na.rm = TRUE))
}

get_snowfall <- function (stationid){
    # Return daily water equivalent snowfall
    # @params: stationdid
    # @return: snowfall
    ghcnd_search(stationid = stationid, 
                var = c("wesf"),
                date_min = "1950-01-01", 
                date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
          month = as.integer(strftime(date, %m)) %% 12,
          day = as.integer(strftime(date, %d),
          year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, month, day) %>%
    summarize(snow_avg = mean(wesf, na.rm = TRUE))
}

get_sunshine <- function (stationid){
    # Return daily sunshine 
    # @params: stationdid
    # @return: sunshine
    ghcnd_search(stationid = stationid, 
                var = c("tsun"),
                date_min = "1950-01-01", 
                date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
          month = as.integer(strftime(date, %m)) %% 12,
          day = as.integer(strftime(date, %d),
          year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, month, day) %>%
    summarize(sun_avg = mean(tsun, na.rm = TRUE))
}

# Read data
cherry <- read.csv("data/washingtondc.csv") %>%
  bind_rows(read.csv("data/liestal.csv")) %>%
  bind_rows(read.csv("data/kyoto.csv")) %>%
  bind_rows(read.csv("data/meteoswiss.csv")) %>%
  bind_rows(read.csv("data/south_korea.csv"))

# First Order fit
ls_fit <- lm(bloom_doy ~ location * year, data = cherry, subset = year >= 1880)

# Temperature Model
historic_max_temperatures <-
  tibble(location = "washingtondc", get_max_temperature("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_max_temperature("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_max_temperature("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_max_temperature("CA001108395")))

historic_min_temperatures <-
  tibble(location = "washingtondc", get_min_temperature("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_min_temperature("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_min_temperature("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_min_temperature("CA001108395")))

# Precipitaiton Model
historic_precipitation <-
  tibble(location = "washingtondc", get_precipitation("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_precipitation("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_precipitation("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_precipitation("CA001108395")))

historic_snowfall <-
  tibble(location = "washingtondc", get_snowfall("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_snowfall("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_snowfall("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_snowfall("CA001108395")))

# Solar Irradiance Model
historic_sunshine <-
  tibble(location = "washingtondc", get_sunshine"USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_sunshine("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_sunshine("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_sunshine("CA001108395")))

# Plan:
# Run least square fit models on each for linear approximations
ls_fit_max_temperature <- lm(tmax_avg ~ year * season + location, 
                            data = historic_max_temperatures)
ls_fit_min_temperature <- lm(tmin_avg ~ year * season + location, 
                            data = historic_min_temperatures)
ls_fit_precipitation <- lm(prec_avg ~ year * month * day + location, 
                            data = historic_precipitation)
ls_fit_snowfall <- lm(snow_avg ~ year * month * day + location, 
                            data = historic_snowfall)
ls_fit_sunshine <- lm(sun_avg ~ year * month * day + location, 
                            data = historic_sunshine)

# Subtract linear value (time) form each set to flatten before fft
# Divide entire dataset by 1/2*(max - min)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norm_max_temp <-as.data.frame(lapply(historix_max_temperatures, min_max_norm))



normalized_max_temperature <- 2*(historic_max_temperatures - ls_fit_max_temperature) /
                        (max(historic_max_temperatures) - min(historic_max_temperatures))
normalized_min_temperature <- 2*(historic_min_temperatures - ls_fit_min_temperature) /
                        (max(historic_min_temperatures) - min(historic_min_temperatures))
normalized_precipitation <- 2*(historic_precipitation - ls_fit_precipitation) /
                        (max(historic_precipitation) - min(historic_precipitation))
normalized_snowfall <- 2*(historic_snowfall - ls_fit_snowfall) /
                        (max(historic_snowfall) - min(historic_snowfall))
normalized_sunshine <- 2*(historic_sunshine - ls_fit_sunshine) /
                        (max(historic_sunshine) - min(historic_sunshine))

# Perform fft on normalized data to run frequency analysis (find major cycles)
max_temp_freqs <- fft(normalized_max_temperature)
min_temp_freqs <- fft(normalized_min_temperature)
prec_freqs <- fft(normalized_precipitation)
snowfall_freqs <- fft(normalized_snowfall)
sunshine_freqs <- fft(normalized_sunshine)

freq_series <- cbind(0:(length(max_temp_freqs)-1), Mod(max_temp_freqs))
freq_series[2:length(max_temp_freqs),2] <- 2*freq_series[2:length(max_temp_freqs),2] 
plot(freq_series, 
      t="h", 
      lwd=2, 
      main = "", 
      xlab = "Frequency", 
      ylab = "Amplitude",
      xlim = c(0,length(max_temp_freqs))
      ylim = c(0,max(Mod(freq_series[,2]))))
      
# Filter out all values of slower frequency than the collection length
# Filter out all value of faster frequency than daily
# Noise floor, keep all with abs() > x dB
# Set annual as "zero" frequency

# Phase offset in days = sin-1(func(day)) - day

# InvFFT back to time series data, what remains is "ahead" and "behind" yearly cycle (phase)
# Find covariance factor with each variable with bloom day "ahead" and "behind" linear (dotproduct)
# Synthesize all models based on relative covariance weighting for training data
# Compare predictive errors rates on testing data
# Simple perceptron?
