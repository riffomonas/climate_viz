library(tidyverse)
library(R.utils)
library(ncdf4) 
library(data.table)
library(lubridate)

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
  mutate(decade = 10 * floor(year / 10),
         single  = year %% 10)


t_data %>%
  mutate(t_diff = case_when(t_diff < -4 ~ -4,
                            t_diff > 4 ~ 4,
                            TRUE ~ t_diff)) %>%
  # filter(year == 2000) %>%
  ggplot(aes(x=longitude, y = latitude, fill = t_diff)) +
  geom_raster() +
  scale_fill_gradient2(name = "Anomaly (\u00B0 C)",
                       low = "darkblue", mid = "white", high ="darkred", 
                       midpoint = 0,
                       limits = c(-5, 5),
                       breaks = c(-4, -2, 0, 2, 4)) + 
  facet_grid(decade~single, switch = "y") +
  coord_fixed(expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "Global annual temperature anomalies between 1950 and 2021") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(color = "white", face = "bold"),
        panel.grid = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0, color ="white"),
        strip.background = element_blank(),
        legend.position = c(0.75, 0.05),
        legend.direction = "horizontal",
        legend.text = element_text(color="white", size = 5),
        legend.title = element_text(color="white", size = 6),
        legend.background = element_rect(fill ="black")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5))

ggsave("figures/global_anomaly.png", width = 10, height = 4.5, units =  "in")


nc_close(nc_data)
unlink("gistemp250_GHCNv4.nc")
unlink("gistemp250_GHCNv4.txt")