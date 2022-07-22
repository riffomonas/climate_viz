source("code/local_weather.R")

prcp_snow_annual <- local_weather %>%
  drop_na() %>%
  filter(snow > 0) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(prcp = sum(prcp),
            snow = sum(snow)) %>%
  filter(year != 1891 &  year!=2022)

prcp_snow_annual %>%
  pivot_longer(-year) %>%
  ggplot(aes(x =year, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol=1, scales = "free_y")

prcp_snow_annual %>%
  ggplot(aes(x = prcp, y = snow, color = year)) +
  geom_point()

cor.test(prcp_snow_annual$prcp, prcp_snow_annual$snow)




prcp_snow_daily <- local_weather %>%
  drop_na() %>%
  filter(snow > 0 & tmax <= 0) %>%
  mutate(year = year(date)) %>%
  filter(year != 1891 &  year!=2022)

snow_model <- lm(snow~prcp*tmax + 0, data = prcp_snow_daily)
summary(snow_model)

prcp_snow_daily %>%
  mutate(predicted = predict(snow_model, prcp_snow_daily)) %>%
  ggplot(aes(x = prcp, y = snow)) +
  geom_point(color = "lightgray") +
  geom_smooth(aes(color = "simple"), formula = "y~x+0", method = "lm",  se = FALSE) +
  geom_segment(x=0, y=0,
               xend = max(prcp_snow_daily$prcp),
               yend = 10 * max(prcp_snow_daily$prcp), size = 1,
               aes(color ="rule_of_thumb")) +
  geom_smooth(aes(y = predicted, color = "advanced"), se = FALSE)+
  labs(x = "Total daily precipitation (mm)",
       y = "Total daily snowfall (mm)") +
  scale_color_manual(name = NULL,
                     breaks = c("rule_of_thumb", "simple", "advanced"),
                     labels = c("10:1 rule of thumb",
                                "Simple model",
                                "Advanced model"),
                     values = c("black", "blue", "red")) +
  theme_classic()

ggsave("figures/model_snow_ratio.png", width = 6, height = 4)

cor.test(prcp_snow_daily$prcp, prcp_snow_daily$snow)
