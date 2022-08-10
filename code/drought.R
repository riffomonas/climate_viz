source("code/local_weather.R")
library(slider)
library(ggtext)

local_weather %>%
  select(date, prcp) %>%
  mutate(prcp = if_else(is.na(prcp), 0, prcp)) %>%
  mutate(one_day_lag = lag(prcp),
         two_day_lag = lag(prcp, n = 2),
         one_day_lead = lead(prcp),
         two_day_lead = lead(prcp, n = 2))

x <- 1:10
slide(x, ~.x, .before = 2)
slide(x, ~.x, .after = 2, .complete=TRUE)

slide(x, ~sum(.x), .before = 2, .complete = TRUE)
slide_dbl(x, ~sum(.x), .before = 2, .complete = TRUE)

tibble(x = 1:10) %>%
  mutate(total = slide_dbl(x, ~sum(.x), .before = 2, .complete = TRUE))


drought_data <- local_weather %>%
  select(date, prcp) %>%
  mutate(prcp = if_else(is.na(prcp), 0, prcp)) %>%
  arrange(date) %>%
  mutate(window_prcp = slide_dbl(prcp, ~sum(.x),
                                 .before = 99, .complete = TRUE)) %>%
  drop_na(window_prcp) %>%
  mutate(start = date - 29) %>%
  select(start, end = date, window_prcp) %>%
  mutate(end_month = month(end),
         end_day = day(end),
         end_year = year(end)) %>%
  group_by(end_month, end_day) %>%
  mutate(threshold = quantile(window_prcp, prob = 0.05)) %>%
  ungroup()

drought_line <- drought_data %>%
  select(end_month, end_day, threshold) %>%
  distinct() %>%
  mutate(fake_date = ymd(glue("2020-{end_month}-{end_day}")))
  

drought_data %>%
  mutate(fake_date = ymd(glue("2020-{end_month}-{end_day}"))) %>%
  select(-start, -end) %>%
  mutate(is_drought_year = end_year == 2012,
         end_year = fct_reorder(factor(end_year), is_drought_year)) %>%
  ggplot(aes(x= fake_date, y=window_prcp, group = end_year, color = is_drought_year)) +
  geom_line(show.legend = FALSE) +
  geom_line(data = drought_line, aes(x = fake_date, y = threshold),
            inherit.aes = FALSE, color = "red") +
  scale_color_manual(breaks = c(T, F),
                     values = c("dodgerblue", "gray")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%B") +
  labs(x = NULL,
       y = "Total precipitation over previous 100 days (mm)",
       title = "The summer of <span style='color:dodgerblue'>2012</span> had less precipitation than <span style='color:red'>95% of previous years dating back to 1892</span>") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        plot.title = element_textbox_simple(margin = margin(b= 10)),
        plot.title.position = "plot")

ggsave("figures/drought.png", width = 6, height = 4)
