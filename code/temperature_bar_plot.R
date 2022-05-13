library(tidyverse)
library(scales)
library(glue)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na()

annotation <- t_data %>%
  arrange(year) %>%
  slice(1, n()) %>%
  mutate(t_diff = 0,
         x = year + c(-5, 5))

max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall=1)

t_data %>%
  ggplot(aes(x=year, y=t_diff, fill=t_diff)) +
  geom_col(show.legend=FALSE) +
  geom_text(data = annotation, aes(x=x, label=year), color="white") +
  geom_text(x=1880, y=1, hjust=0,
            label=glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"),
            color="white") +
  # scale_fill_gradient2(low="darkblue", mid="white", high="darkred",
  #                     midpoint = 0, limits= c(-0.5, 1.5)) +
  # scale_fill_gradientn(colors=c("darkblue", "white", "darkred"),
  #                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
  #                      limits = c(min(t_data$t_diff), max(t_data$t_diff))) +
  scale_fill_stepsn(colors=c("darkblue", "white", "darkred"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks=9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="black"),
    legend.text = element_text(color="white")
  )

ggsave("figures/temperature_bar_plot.png", width=7, height=4)
