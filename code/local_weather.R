library(tidyverse)
library(glue)
library(lubridate)

inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url,
                        col_names = c("station", "lat", "lon",
                                      "variable", "start", "end"))


#Coordinates for Dexter, MI
my_lat <- 42.33831964441621 * 2 * pi / 360
my_lon <- -83.88938389977316 * 2 * pi / 360


# Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
# 
# The obtained distance, d, is in miles. If you want your value to be in units of kilometers, multiple d by 1.609344.
# d in kilometers = 1.609344 * d in miles

my_station <- inventory %>%
  mutate(lat_r = lat *2 *pi/360,
         lon_r = lon *2 *pi/360,
         d = 1.609344 * 3963 * acos((sin(lat_r) * sin(my_lat)) +
                                      cos(lat_r) * cos(my_lat) *
                                      cos(my_lon - lon_r))
         ) %>%
  filter(start < 1960 & end > 2020) %>%
  top_n(n = -1, d) %>%
  distinct(station) %>%
  pull(station)

station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

local_weather <- read_csv(station_daily,
         col_names = c("station", "date", "variable", "value",
                       "a", "b", "c", "d")) %>%
  select(date, variable, value) %>%
  pivot_wider(names_from = "variable", values_from="value") %>%
  select(date, TMAX, PRCP, SNOW) %>%
  mutate(date = ymd(date),
         TMAX = TMAX / 10,
         PRCP = PRCP / 10) %>%
  rename_all(tolower) %>%
  mutate(snow = if_else(snow < 500, snow, NA_real_),
         prcp = if_else(prcp < 200, prcp, NA_real_)
  )
