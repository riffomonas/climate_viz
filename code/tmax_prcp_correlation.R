source("code/local_weather.R")

tmax_prcp <- local_weather %>%
  mutate(year = year(date)) %>%
  # drop_na(tmax, prcp) %>%
  filter(year != 1891 & year != year(today())) %>%
  group_by(year) %>%
  summarize(tmax = mean(tmax, na.rm=TRUE),
            prcp = sum(prcp, na.rm=TRUE))

tmax_prcp %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() + 
  facet_wrap(~name, ncol= 1, scales = "free_y") +
  geom_smooth(se = FALSE)
  

scaled_tmax_prcp <- tmax_prcp %>%
  mutate(tmax_tr = (tmax - min(tmax))/ (max(tmax) - min(tmax)),
         tmax_min = min(tmax),
         tmax_max = max(tmax),
        
         prcp_tr = (prcp - min(prcp))/ (max(prcp) - min(prcp)),
         prcp_min = min(prcp),
         prcp_max = max(prcp)
         
         )


tmax_plot <- scaled_tmax_prcp %>%
  ggplot(aes(x = year, y = tmax_tr)) +
  geom_line(color = "blue")

tmax_plot +
  geom_line(aes(y = prcp_tr), color = "red") +
  scale_y_continuous(labels = seq(10, 30, 5),
                     breaks = (seq(10, 30, 5) - 11.7)/17.5,
                     limits = (c(8, 32) - 11.7)/17.5,
                     name = "Average annual temperature",
                     sec.axis = sec_axis(trans = ~.,
                                         labels = seq(300, 1800, 300),
                                         breaks = (seq(300, 1800, 300) - 420)/1298,
                                         name = "Total Precipitation (mm)"
                                         )
                     ) +
  theme(axis.title.y.left = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red"))



tmax_prcp %>%
  ggplot(aes(x = tmax, y = prcp)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(tmax_prcp$tmax, tmax_prcp$prcp)
cor.test(tmax_prcp$tmax, tmax_prcp$prcp, method = "spearman", exact = FALSE)
