library(tidyverse)
library(cowplot)
library(readxl)
library(janitor)
library(lubridate)

theme_set(theme_cowplot())

plate_layout <- read_csv("data-general/randomization-key.csv") 

plate_key <- read_excel("data-general/platenumber-platekey.xlsx") %>% 
	clean_names() 

plate_info <- left_join(plate_layout, plate_key, by = c("platekey")) %>% 
	unite(row, column, col = "well", remove = FALSE, sep = "") %>%
	mutate(column = formatC(column, width = 2, flag = 0)) %>% 
	mutate(column = str_replace(column, " ", "0")) %>% 
	unite(col = well, row, column, sep = "") %>% 
	rename(plate = platenumber) 

write_csv(plate_info, "data-processed/plate-info.csv")


RFU_files <- c(list.files("data-raw/chlorophyll", full.names = TRUE))
RFU_files <- RFU_files[grepl(".xls", RFU_files)]

names(RFU_files) <- RFU_files %>% 
	gsub(pattern = ".xlsx$", replacement = "") %>% 
	gsub(pattern = ".xls$", replacement = "")

# RFU_files[grepl("104", RFU_files)]

# RFU_files <- RFU_files[!grepl("acc", RFU_files)]

all_plates <- map_df(RFU_files, read_excel, range = "B56:N64", .id = "file_name") %>% 
	rename(row = ...1) %>% 
	mutate(file_name = str_replace(file_name, " ", ""))


all_times <- map_df(RFU_files, read_excel, range = "A6:B8", .id = "file_name") %>% 
	clean_names() %>% 
	filter(!is.na(plate_1)) %>% 
	# filter(!grepl("dilution", file_name)) %>% 
	spread(key = plate_number, value = plate_1) %>% 
	separate(Time, into = c("crap", "time"), sep = " ") %>% 
	select(-crap) %>% 
	mutate(file_name = str_replace(file_name, " ", "")) %>% 
	separate(file_name, into = c("path", "plate"), sep = "plate", remove = FALSE) %>% 
	separate(plate, into = c("plate", "other"), sep = "-") %>% 
	select(-other) 


all_plates2 <- dplyr::left_join(all_plates, all_times, by = "file_name")

all_temp_RFU <- all_plates2 %>% 
	gather(key = column, value = RFU, 3:14) %>% 
	unite(row, column, col = "well", remove = FALSE, sep = "") %>%
	mutate(column = formatC(column, width = 2, flag = 0)) %>% 
	mutate(column = str_replace(column, " ", "0")) %>% 
	unite(col = well, row, column, sep = "") %>% 
	filter(!is.na(RFU)) %>% 
	mutate(plate = as.numeric(plate)) %>% 
	unite(col = date_time, Date, time, sep = " ") %>%
	mutate(date_time = ymd_hms(date_time))
	

all_temp_RFU %>% 
	ggplot(aes(x = date_time, y = RFU)) + geom_point() +
	facet_wrap( ~ plate)


View(all_temp_RFU)
View(plate_info)

all_rfus_raw <- left_join(all_temp_RFU, plate_info, by = c("well", "plate")) %>% 
	mutate(plate = as.numeric(plate)) %>% 
	filter(!is.na(platekey)) %>% 
	mutate(population = as.character(population)) %>% 
	separate(population, into = c("treatment", "replicate"), sep = 1, remove = FALSE) %>% 
	mutate(replicate = str_replace(replicate, ".", ""))



# all_rfus2 <- all_rfus_raw %>% 
# 	unite(col = date_time, Date, time, sep = " ") %>%
# 	mutate(date_time = ymd_hms(date_time)) 


all_rfus3 <- all_rfus_raw %>% 
	group_by(treatment, replicate) %>% 
	mutate(start_time = min(date_time)) %>% 
	mutate(days = interval(start_time, date_time)/ddays(1)) %>% 
	unite(col = well_plate, well, plate, remove =  FALSE) %>% 
	mutate(acclimation_conditions = case_when(treatment == "1" ~ "light_acclimated",
								   treatment == "2" ~ "dark_acclimated", 
								   treatment == "3" ~ "cycle_acclimated")) %>% 
	mutate(experiment_conditions = case_when(plate %in% c("1", "2") ~ "grown_in_light",
											 plate %in% c("3", "4") ~ "grown_in_dark",
											 plate %in% c("5", "6") ~ "grown_in_cycle"))


all_rfus3 %>% 
	ggplot(aes(x = days, y = RFU, color = acclimation_conditions, group = well_plate)) + geom_point() +
	geom_line() +
	facet_grid(population ~ experiment_conditions, scales = "free")
ggsave("figures/rfus-time.pdf", width = 15, height = 20)

write_csv(all_rfus3, "data-processed/rfu-data.csv")


library(broom)
library(plotrix)
summaries <- all_rfus3 %>% 
	group_by(well_plate, acclimation_conditions, experiment_conditions, replicate, population) %>% 
	do(tidy(lm(log(RFU) ~ days, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	group_by(population, acclimation_conditions, experiment_conditions) %>% 
	summarise_each(funs(mean), estimate) %>% 
	group_by(acclimation_conditions, experiment_conditions) %>% 
	summarise_each(funs(mean, std.error), estimate) 
write_csv(summaries, "data-processed/mean_growth_rates.csv")


growth_rates <- all_rfus3 %>% 
	group_by(well_plate, acclimation_conditions, experiment_conditions, replicate, population) %>% 
	do(tidy(lm(log(RFU) ~ days, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	group_by(population, acclimation_conditions, experiment_conditions) %>% 
	summarise_each(funs(mean), estimate) 

write_csv(growth_rates, "data-processed/growth-rates.csv")


summaries %>% 
	ggplot(aes(x = experiment_conditions, y = mean, color = acclimation_conditions)) + geom_point() +
	geom_errorbar(aes(x = experiment_conditions, ymin = mean - std.error, ymax = mean + std.error), width = 0.1) +
	ylab("Population growth rate") + xlab("Experiment conditions")


