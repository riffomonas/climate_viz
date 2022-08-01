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
  scale_y_continuous(labels = seq(11, 18, 2),
                     breaks = (seq(11, 18.5, 2) - 11.7)/(17.5-11.7),
                     limits = (c(11, 18) - 11.7)/(17.5-11.7),
                     name = "Average annual temperature (\u00B0C)",
                     sec.axis = sec_axis(trans = ~.,
                                         labels = seq(400, 1400, 250),
                                         breaks = (seq(400, 1400, 250) - 420)/(1298-420),
                                         name = "Total Precipitation (mm)"
                                         )
                     ) +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red")) +
  labs(x = "Year")
  
ggsave("figures/tmax_prcp_doubley.png", width = 6, height=4.5)


tmax_prcp %>%
  ggplot(aes(x = tmax, y = prcp)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(tmax_prcp$tmax, tmax_prcp$prcp)
cor.test(tmax_prcp$tmax, tmax_prcp$prcp, method = "spearman", exact = FALSE)
