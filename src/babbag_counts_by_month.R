# Build babbage counts by month by center
# Thu Mar 03 08:43:14 2022 ------------------------------

library(tidyverse)

# ----------
centerLk <- readxl::read_excel(here::here("input", "center_lkup.xlsx"), "center_lkup") %>% 
  select(CENTER_ID, BldgGroupID, BldgGroupName, Group, Region, BabbageName) %>%
  ungroup()

# --------------
babb <- readxl::read_excel(path= here::here("input", "Tanger - Total Device Counts for 2019 through 2021 v4 (02-22-2022).xlsx")) %>% 
  mutate(SignalDate = lubridate::make_date(year, month, day),
         Month = lubridate::floor_date(SignalDate, "month")) %>% 
  group_by(location_name, Month) %>% 
  summarise(TotalDevices = sum(devices)) %>% 
  ungroup() %>% 
  left_join(., centerLk,
            by = c("location_name"= "BabbageName"))


babb %>% filter(is.na(CENTER_ID))


babb %>% 
  write_csv(., file = here::here("data","Babbage_MonthlySignalCounts_2019_2021.csv"))
