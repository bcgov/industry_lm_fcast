library(tidyverse)
library(readxl)
library(janitor)
library(fpp3)
library(conflicted)
conflicts_prefer(dplyr::filter)
tbbl <- read_excel("Employment for 64 LMO Industries 2000-2024.xlsx", skip = 3)|>
  filter(str_detect(`Lmo Ind Code`, "ind"))|>
  pivot_longer(cols = starts_with("2"), names_to = "year")|>
  clean_names()|>
  group_by(year)|>
  mutate(prop=value/sum(value))|>
  unite(industry, lmo_ind_code, lmo_detailed_industry, sep=": ")|>
  select(-value)|>
  mutate(year=as.numeric(year),
         thing="historic")|>
  tsibble(key=industry, index = year)

fit <- tbbl |>
  model(linear=TSLM(prop ~ trend()),
        ets=ETS(prop)
        )

forecast <- forecast(fit, h = 11)

#take the mean of the two forecasts-------------------------

composite <- forecast|>
  as_tibble()|>
  group_by(industry, year)|>
  summarize(mean_fcast=mean(.mean))|>
  group_by(year)|>
  mutate(mean_fcast=mean_fcast/sum(mean_fcast))

write_csv(composite, "composite_fcast.csv")

#look at it--------------------------------

composite|>
  rename(prop=mean_fcast)|>
  mutate(thing="forecast")|>
  bind_rows(tbbl)|>
  ggplot(aes(year, prop, colour = thing))+
  geom_line()+
  facet_wrap(~fct_reorder(industry, prop, .desc = TRUE), scales="free")



