library(tidyverse)
library(R.utils)
library(ncdf4) 
library(data.table)
library(lubridate)
library(ggridges)

url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp250_GHCNv4.nc.gz"
download.file(url, destfile = "gistemp250_GHCNv4.nc.gz")
gunzip("gistemp250_GHCNv4.nc.gz")


nc_data <- nc_open('gistemp250_GHCNv4.nc')
# Save the print(nc) dump to a text file
{
  sink('gistemp250_GHCNv4.txt')
  print(nc_data)
  sink()
}


lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

t_anomaly.array <- ncvar_get(nc_data, "tempanomaly") # store the data in a 3-dimensional array
dim(t_anomaly.array) 

fillvalue <- ncatt_get(nc_data, "tempanomaly", "_FillValue")
t_anomaly.array[t_anomaly.array == fillvalue$value] <- NA

t_data <- as.data.table(t_anomaly.array) %>%
  as_tibble() %>%
  select(longitude = V1, latitude = V2, time = V3, t_diff = value) %>%
  mutate(longitude = lon[longitude],
         latitude = lat[latitude],
         time = t[time] + as.Date("1800-01-01"),
         year = year(time)) %>% 
  group_by(year, longitude, latitude) %>%
  summarize(t_diff = mean(t_diff), .groups = "drop") %>%
  filter(year >= 1950 & year < 2022) %>%
  group_by(year) %>%
  mutate(t_ave = mean(t_diff))

t_data %>%
  ggplot(aes(x = t_diff,
             y = factor(year, levels = seq(2021, 1950, -1)),
             fill = t_ave)) +
  geom_density_ridges(bandwidth = 0.3, scale = 3,
                      size = 0.2, color="white") +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
                       midpoint = 0, guide = "none") +
  coord_cartesian(xlim=c(-5, 5)) +
  scale_x_continuous(breaks = seq(-4, 4, 2)) +
  scale_y_discrete(breaks = seq(1950, 2020, 10)) +
  labs(y = NULL, 
       x = "Temperature anomaly (\u00B0 C)",
       title = "Land Temperature Anomaly Distribution") +
  theme(
    text = element_text(color="white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color="black"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color = "white"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color="white"),
    axis.line.y = element_blank()
  )
  
ggsave("figures/temp_distribution.png", height = 6, width = 4)

nc_close(nc_data)
unlink("gistemp250_GHCNv4.nc")
unlink("gistemp250_GHCNv4.txt")