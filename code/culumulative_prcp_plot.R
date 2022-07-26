source("code/local_weather.R")

library(ggtext)
library(scales)

this_year <- year(today())
this_month <- month(today(), label = TRUE, abbr = FALSE)
this_day <- ordinal(day(today()))

local_weather %>%
  select(date, prcp) %>%
  drop_na(prcp) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         is_this_year = year == this_year) %>%
  filter(year != 1891 & !(month == 2 & day == 29)) %>%
  arrange(date) %>%
  group_by(year) %>%
  mutate(cum_prcp = cumsum(prcp)) %>%
  ungroup() %>%
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>%
  ggplot(aes(x = new_date, y =cum_prcp, group = year,
             color=is_this_year, size=is_this_year)) +
  geom_step(show.legend = FALSE) +
  geom_smooth(aes(group = 1), color = "black", size = 0.3, se= FALSE) +
  scale_color_manual(breaks = c(F, T),
                     values = c("lightgray", "dodgerblue")) +
  scale_size_manual(breaks = c(F, T),
                    values = c(0.3, 1)) +
  scale_x_date(date_labels = "%B", date_breaks="2 months") +
  scale_y_continuous(breaks = seq(0, 1500, 300),
                     labels = seq(0, 150, 30),
                     limits = c(0, 1500),
                     expand= c(0,0)) +
  labs(x = NULL,
       y = "Cumulative precipitation (cm)",
       title = glue("Through {this_month} {this_day}, the cumulative precipitation near Ann Arbor, MI is <span style = 'color: dodgerblue'>above average</span> for {this_year}")) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin=margin(b=10), size = 16),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line()

  )

ggsave("figures/cumulative_prcp.png", width = 6, height =5)
