source("code/local_weather.R")

this_year <- year(today())

local_weather %>%
  select(date, tmax) %>%
  drop_na(tmax) %>%
  mutate(year = year(date)) %>%
  filter(year != 1891 & year != this_year) %>%
  group_by(year) %>%
  summarize(tmax = mean(tmax)) %>%
  mutate(normalize_range = (year >=1951 & year <= 1980),
         normalize_mean = sum(tmax * normalize_range)/sum(normalize_range),
         t_diff = tmax - normalize_mean) %>%
  ggplot(aes(x = year, y = t_diff)) +
  geom_line() +
  geom_smooth()



local_weather %>%
  select(date, tmax) %>%
  drop_na(tmax) %>%
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year != 1891) %>%
  group_by(year, month) %>%
  summarize(tmax = mean(tmax), .groups = "drop") %>%
  group_by(month) %>%
  mutate(normalized_range = year >= 1951 & year <=1980,
         normalized_temp = sum(tmax * normalized_range)/sum(normalized_range),
         t_diff = tmax - normalized_temp,
         is_this_year = year == this_year) %>%
  ungroup() %>%
  ggplot(aes(x = month, y = t_diff, group =year, color=is_this_year)) +
  geom_line() +
  scale_color_manual(breaks = c(F, T),
                     values = c("lightgray", "dodgerblue"),
                     guide = "none") +
  theme_classic()
  