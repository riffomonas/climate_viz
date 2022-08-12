library(ggtext)

source("code/local_weather.R")

snow_data <- local_weather %>%
  select(date, snow) %>%
  drop_na(snow) %>%
  mutate(cal_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{cal_year}-08-01")),
                             cal_year - 1,
                             cal_year)) %>%
  select(month, snow_year, snow) %>%
  filter(snow_year != 1891 & snow_year != 2022) %>%
  mutate(snow_year = factor(snow_year, levels = 1892:2021),
         month = factor(month, levels = c(8:12, 1:7)))
  


snow_data %>%
  group_by(snow_year, .drop = FALSE) %>%
  summarize(total_snow = sum(snow)) %>%
  mutate(snow_year = as.numeric(levels(snow_year))) %>%
  ggplot(aes(x = snow_year, y = total_snow)) +
  geom_line()

snow_data %>%
  filter(snow > 0) %>%
  count(snow_year, .drop= FALSE) %>%
  mutate(snow_year = as.numeric(levels(snow_year))) %>%
  ggplot(aes(x = snow_year, y = n)) +
  geom_line()


total_snow <- snow_data %>%
  group_by(snow_year) %>%
  summarize(total_snow = sum(snow)) %>%
  filter(snow_year == 2021) %>%
  mutate(total_snow = total_snow/10) %>%
  pull(total_snow)


snow_data %>%
  group_by(snow_year, month, .drop = FALSE) %>%
  summarize(snow = sum(snow), .groups = "drop") %>%
  mutate(is_this_year = 2021 == snow_year) %>%
  ggplot(aes(x = month, y = snow, group = snow_year, color = is_this_year)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(name = NULL,
                     breaks = c(T, F),
                     values = c("dodgerblue", "gray")
                     ) +
  scale_x_discrete(breaks = c(9, 11, 1, 3, 5),
                   labels = month.abb[c(9, 11, 1, 3, 5)],
                   expand = c(0, 0 )) +
  scale_y_continuous(breaks = seq(0, 1500, 500),
                     labels = seq(0, 150, 50)) +
  labs(x = NULL, y = "Total monthly snowfall (cm)",
       title = glue("The <span style = 'color:dodgerblue'>snow year 2021</span> had a total of {total_snow} cm of snow")) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        plot.title.position = "plot",
        plot.title = element_markdown())

ggsave("figures/snow_by_snow_year.png", width = 6, height = 4)
