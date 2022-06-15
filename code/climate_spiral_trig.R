library(tidyverse)
library(gganimate)

t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

# next_jan <- t_diff %>%
#   filter(month == "Jan") %>%
#   mutate(year = year - 1,
#          month = "next_Jan")

radius_bump <- 1.5


t_data <- t_diff %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(step_number = 1:nrow(.),
         radius = t_diff + radius_bump,
         theta = 2 * pi * (month_number - 1) / 12,
         x = radius * sin(theta),
         y = radius * cos(theta))

annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)

temp_lines <- tibble(
  x = 0,
  y = c(1, 0, -1) + radius_bump,
  labels = c("+1\u00B0 C", "0\u00B0 C", "-1\u00B0 C")
)

month_labels <- tibble(
  theta = 2*pi*(1:12 - 1)/12,
  radius = 1.5 + radius_bump, 
  labels = toupper(month.abb),
  x = radius * sin(theta),
  y = radius * cos(theta)
)

# gridlines <- tibble(
#   x = c(1.2, 1.3, 1.6),
#   xend = c(12.8, 12.7, 12.4),
#   y = c(1, 0, -1), 
#   yend = y
# )

gridlines <- tibble(theta = rep(2*pi * seq(0, 1, 0.01), each = 3),
       radius = rep(c(1, 0, -1) + radius_bump, length.out = length(theta)),
       line = rep(c("a", "b", "c"), length.out = length(theta)),
       x = radius * sin(theta),
       y = radius * cos(theta)) %>%
  filter((line == "a" & theta > 0.01 * 2 *pi & theta < 0.99 *2 *pi) |
           (line == "b" & theta > 0.025 * 2 *pi & theta < 0.975 *2 *pi) |
           (line == "c" & theta > 0.05 * 2 *pi & theta < 0.95 *2 *pi))

a <- t_data %>% 
  ggplot(aes(x=x, y=y, color=t_diff)) +
  geom_label(aes(x = 0, y= 0, label = year),
             fill="black",
             label.size = 0,
             size=6) +
  geom_path() +
  geom_path(data=gridlines %>% filter(radius != radius_bump),
            aes(x=x, y=y, group = line),
            color="yellow",
            inherit.aes = FALSE) +
  geom_path(data=gridlines %>% filter(radius == radius_bump),
            aes(x=x, y=y, group = line),
            color="green",
            inherit.aes = FALSE) +
  geom_text(data = temp_lines, aes(x=x, y=y, label=labels),
            color=c("yellow", "green", "yellow"), size=2, fontface="bold",
            inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x=x, y=y, label = labels),
            inherit.aes = FALSE, color="yellow"
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, -0.3)) +
  scale_x_continuous(limits = c(-4, 4), expand = c(0, -0.3)) +
  coord_fixed() +
  scale_color_gradient2(low = "blue", high = "red", mid="white", midpoint = 0,
                        guide = "none") +
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill = "black", color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  ) +
  transition_reveal(step_number)

ggsave("figures/climate_spiral_trig.png", width=4.5, height=4.5, unit="in")

animate(a, width=4.5, height=4.5, unit="in", res=300)
anim_save("figures/climate_spiral_trig.gif")
# 
# 
# animate(a, width=4.155, height=4.5, unit="in", res=300,
#         renderer = av_renderer("figures/climate_spiral_trig.mp4")
#         )
