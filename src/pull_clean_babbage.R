# Clean and organize the babbage data
# Tue Feb 15 10:30:21 2022 ------------------------------


library(tidyverse)
library(geomtextpath)

theme_set(theme_bw())
cb_palette <- c("#003399", "#ff2b4f", "#3686d3", "#fcab27", "#88298a", "#000000", "#969696", "#008f05", "#FF5733", "#E5F828")

# ----------
centerLk <- readxl::read_excel(here::here("input", "center_lkup.xlsx"), "center_lkup") %>% 
  select(CENTER_ID, BldgGroupID, BldgGroupName, Group, Region, BabbageName) %>%
  ungroup()

# ----------
hoursDay <- tibble(
  hours = 0:23
)  

hoursDay <- c(0:23)
# ----------


babb <- readxl::read_excel(path= here::here("input", "Tanger - Total Device Counts for 2019 through 2021 v4 (02-22-2022).xlsx")) %>% 
  mutate(SignalDate = lubridate::make_date(year, month, day)) %>% 
  select(location_name, SignalDate, hour, TotalDevices = devices, Visits) %>% 
  arrange(location_name, SignalDate, hour) 
    

bab_all <- babb %>% 
  expand(location_name,SignalDate, hour) %>% 
  left_join(., babb) %>% 
  mutate(TotalDevices = ifelse(is.na(TotalDevices),0, TotalDevices)) %>% 
  group_by(location_name, SignalDate) %>% 
  mutate(PerOfDay = TotalDevices/sum(TotalDevices)) %>% 
  ungroup() %>% 
  mutate(DOW = lubridate::wday(SignalDate, abbr = T, label = T),
         DOW_num = lubridate::wday(SignalDate),
         hourSeg = cut(hour, c(-Inf,8, 9, 10, 12,  16,18, 20, Inf)))
  

save(bab_all, file = here::here("data", "bab_all.rds"))

#----Check against the center table------

test <- babb %>% 
  count(location_name) %>% 
  right_join(.,
             centerLk %>% distinct(BabbageName) %>% mutate(CenterLk = 1),
             by = c("location_name" = "BabbageName"))



#-----------


  
test <-
  bab_all %>% 
  filter(hour > 9, hour < 18) %>% 
  arrange(location_name, desc(SignalDate), hour) %>% 
  mutate(CountZeros = ifelse(TotalDevices == 0, 1, 0)) %>% 
  group_by(location_name, SignalDate) %>% 
  mutate(TotalZero = sum(CountZeros),
         TotalDevDay = sum(TotalDevices)) %>% 
  ungroup() %>% 
  filter(TotalZero > 0,
         TotalDevDay > 50) %>% 
  arrange(desc(TotalDevDay), desc(SignalDate),location_name, hour)


unique(bab_all$location_name)
  

#----- Daily Counts

DailyCounts <-
  bab_all %>% 
  mutate(SignalMonth = lubridate::floor_date(SignalDate, "week"),
         loc = str_remove_all(location_name, "Tanger Outlets ")) %>% 
  group_by(loc, location_name, SignalDate = SignalMonth) %>% 
  summarise(TotalDaily = sum(TotalDevices)) %>% 
  ungroup() %>% 
  left_join(., centerLk,
            by = c("location_name" = "BabbageName")) 


unqCnt <- unique(DailyCounts$loc)
# Loop

for (i in unqCnt) {
  
  temp_plot = 
    DailyCounts %>% 
    filter(loc %in% i) %>% 
    ggplot(aes(SignalDate, TotalDaily))+
    geom_point()+
    geom_line()+
    # geom_textpath(aes(label = loc))+
    facet_wrap(Region + Group ~ ., ncol = 2, scales = "free_y")+
    theme(legend.position = "none")+
    labs(title = glue::glue("Weekly Signals for: {i}"),
         y = "Week Counts", x= "Signal Date")
  
  ggsave(temp_plot, file=here::here("output",paste0("plot_", i,".png")), width = 14, height = 10, units = "cm")
}


# library(plotly)
# 
# p1 <- DailyCounts %>% 
#   filter(loc %in% c("Hershey")) %>% 
#   ggplot(aes(SignalDate, TotalDaily))+
#   geom_point()+
#   geom_line()+
#   # geom_textpath(aes(label = loc))+
#   facet_wrap(Region + Group ~ ., ncol = 2, scales = "free_y")+
#   theme(legend.position = "none")+
#   labs(title = glue::glue("Weekly Signals for: {i}"),
#        y = "Week Counts", x= "Signal Date")
# 
# ggplotly(p1)





