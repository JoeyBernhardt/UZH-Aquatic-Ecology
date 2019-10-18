

library(tidyverse)
library(lubridate)

abs <- read_csv("data-processed/absorbances.csv")
abs_gr <- read_csv("data-processed/absorbance-growth-rates.csv")
rfu <- read_csv("data-processed/rfu-data.csv") %>% 
	mutate(acclimation_conditions = str_replace(acclimation_conditions, "_", " ")) %>% 
	mutate(experiment_conditions = str_replace(experiment_conditions, "_", " ")) 



### change treatment names

absb <- abs %>% 
	mutate(acclimation_conditions = str_replace(acclimation_conditions, "_", " ")) %>% 
	mutate(experiment_conditions = str_replace(experiment_conditions, "_", " ")) 



late_rfu <- rfu %>% 
	filter(date_time > ymd_hms("2019-10-10 10:02:16"))


late_rfu %>% 
	rename(`Acclimation conditions` = acclimation_conditions) %>% 
	ggplot(aes(x = date_time, y = RFU, color = `Acclimation conditions`)) + geom_point() 

late_rfu2 <- late_rfu %>% 
	mutate(day = day(date_time)) %>% 
	mutate(hour = hour(date_time)) %>% 
	mutate(time_of_day = ifelse(hour < 12, "morning", "evening")) %>%
	mutate(unique_time = paste(day, time_of_day, sep = "_"))

abs2 <- abs %>%
	mutate(day = day(date_time)) %>% 
	mutate(hour = hour(date_time)) %>% 
	mutate(time_of_day = ifelse(hour < 12, "morning", "evening")) %>%
	mutate(unique_time = paste(day, time_of_day, sep = "_"))


all_meas <- left_join(late_rfu2, abs2, by = c("well_plate", "unique_time")) 

all_meas$absorbance <- scale(all_meas$absorbance)
all_meas$RFU <- scale(all_meas$RFU)

all_meas %>% 
	ggplot(aes(x = RFU, y = absorbance, color = acclimation_conditions.x)) + geom_point() +
	geom_smooth(method = "lm") +
	facet_wrap( ~ experiment_conditions.x) +
	geom_abline(intercept = 0, slope = 1)
