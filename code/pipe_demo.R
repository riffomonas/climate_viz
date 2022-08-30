source("code/local_weather.R")
library(magrittr)


no_na_no_zero <- local_weather[(!is.na(local_weather$prcp) &
                                  !is.na(local_weather$snow)) &
                                 local_weather$snow > 0,]

cor.test(~prcp+snow, data = no_na_no_zero)


no_nas <- drop_na(local_weather)
no_nas_no_zero <- filter(no_nas, snow > 0)
cor.test(~prcp+snow, data = no_na_no_zero)


bench_mark_pipe <- bench::mark(
base_nopipe = cor.test(~prcp+snow, data = local_weather[(!is.na(local_weather$prcp) &
                                             !is.na(local_weather$snow)) &
                                            local_weather$snow > 0,]),
base_nopipe_s = cor.test(~prcp+snow, data = subset(local_weather, !is.na(prcp) &
                                                           !is.na(snow) &
                                                          snow > 0)),

base_base = local_weather |> 
  subset(!is.na(prcp) & !is.na(snow)) |>
  subset(snow > 0) |>
  cor.test(~prcp + snow, data = _ ),

dplyr_base = local_weather |> 
  drop_na(prcp, snow) |>
  filter(snow > 0) |>
  cor.test(~prcp + snow, data = _ ),

base_magrittr = local_weather  %>%  
  subset(!is.na(prcp) & !is.na(snow))  %>% 
  subset(snow > 0)  %>% 
  cor.test(~prcp + snow, data = . ),

base_magrittr_s = local_weather  %>%  
  subset(!is.na(prcp) & !is.na(snow) & snow > 0)  %>% 
  cor.test(~prcp + snow, data = . ),

dplyr_magrittr = local_weather %>%
  drop_na(prcp, snow) %>%
  filter(snow > 0) %>%
  cor.test(~prcp + snow, data = .)
)

autoplot(bench_mark_pipe, type= "jitter")

local_weather %>%
  drop_na() %>%
  filter(snow > 0) %$% #exposition pipe
  cor.test(prcp, snow, data = .)


local_weather %<>%
  drop_na() %>%
  filter(snow > 0)


local_weather %>%
  drop_na() %T>%
  print() %>%
  filter(snow > 0) %T>%
  print() %>%
  summarize(total_prcp = sum(prcp))


1:96 %>%
  matrix(ncol = 12) %>%
  set_colnames(LETTERS[1:12]) %>%
  set_rownames(1:8) %>%
  divide_by(10) %>%
  as.data.frame() %>%
  extract2("D")
