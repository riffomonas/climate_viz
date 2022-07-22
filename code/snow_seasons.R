source("code/local_weather.R")

local_weather %>%
  select(date, snow) %>%
  mutate(snow = if_else(is.na(snow), 0, snow),
         year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{year}-08-01")),
                             year - 1,
                             year)) %>% 
  filter(snow_year != 1891 & snow_year != 1955) %>%
  group_by(month, snow_year) %>%
  summarize(snow = sum(snow), .groups = "drop") %>%
  mutate(month = factor(month, levels = c(8:12, 1:7)),
         is_this_snow_year = snow_year == 2021) %>%
  ggplot(aes(x = month, y = snow, group = snow_year,
             color = is_this_snow_year, size = is_this_snow_year)) +
  geom_line() +
  scale_x_discrete(breaks = c(9, 11, 1, 3, 5),
                   labels = month.name[c(9, 11, 1, 3, 5)],
                   expand = c(0,0)) +
  scale_color_manual(breaks = c(F, T),
                     values = c("lightgray", "dodgerblue"), guide = "none") +
  scale_size_manual(breaks = c(F, T),
                    values = c(0.25, 1), guide = "none") +
  labs(x = NULL, y = "Precipitation by month for each snow season (mm)") +
  theme_classic()

ggsave("figures/snow_by_snow_year.png", width=6, height =4)
