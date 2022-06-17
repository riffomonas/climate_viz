library(tidyverse)
library(plotly)
library(glue)
library(htmlwidgets)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na="***") %>%
  select(year = Year, all_of(month.abb)) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na() %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(year, month) %>%
  mutate(month_number = as.numeric(month),
         radius = t_diff + 1.5,
         theta = 2 * pi * (month_number-1)/12,
         x = radius * sin(theta),
         y = radius * cos(theta),
         z = year,
         label = glue("{month} {year}\n{t_diff}\u00B0 C"))

# t_data %>%
#   ggplot(aes(x=x, y=y, color=z)) +
#   geom_path()

axx <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showticklabels = FALSE
)

axy <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showticklabels = FALSE
)

axz <- list(
  title = ""
)

p <- plot_ly(t_data,
              x = ~x, y = ~y, z = ~z, text = ~label,
              hoverinfo = "text",
              type = 'scatter3d',
              mode = 'lines',
              line = list(width = 10, color = ~t_diff,
                          cmid = 0,# cmin=min(t_data$t_diff), cmax=max(t_data$t_diff),
                          colorscale = list(c(0,'#0000FF'),
                                            c(0.5, "#FFFFFF"),
                                            c(1,'#FF0000')))) %>%
        layout(scene = list(xaxis=axx,
                            yaxis=axy,
                            zaxis=axz))

saveWidget(p, "figures/climate_spiral_plotly.html")
