source("code/local_weather.R")
library(ggtext)

threshold <- 0

drought_by_year <- local_weather %>%
  select(date, prcp) %>%
  mutate(prcp = replace_na(prcp, 0)) %>%
#  replace_na(list(prcp = 0)) %>%
  filter(prcp > threshold) %>%
  mutate(prev_date = lag(date, n=1)) %>%
  drop_na() %>%
  mutate(drought_length = as.numeric(date - prev_date) - 1,
         year = year(date)) %>%
  select(year, length = drought_length)


drought_by_year %>% 
  filter(year == 1976) %>%
  ggplot(aes(x = length)) + geom_histogram()


drought_by_year %>%
  filter(year != 1891) %>%
  group_by(year) %>%
  summarize(n = n(),
            median = median(length),
            mean = mean(length),
            max = max(length),
            uquartile = quantile(length, prob = 0.75)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Year",
       y = "Average number of days\nbetween precipitation events",
       title = "The length of drought has been <span style = 'color:blue'>decreasing</span> over the past 130 years in Southeastern Michigan") +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  theme_classic() +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 18, margin = margin(b=10)))


ggsave('figures/drought_lengths.png', width = 5, height = 4)
