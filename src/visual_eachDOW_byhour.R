# By day for each day of week by hour
# Fri Feb 25 12:05:12 2022 ------------------------------

library(tidyverse)

load(file = here::here("data", "bab_all.rds"))
load(file = here::here("data", "trafficClean.rds"))

theme_set(theme_bw())
cb_palette <- c("#003399", "#ff2b4f", "#3686d3", "#fcab27", "#88298a", "#000000", "#969696", "#008f05", "#FF5733", "#E5F828")

# bab_all
# trafficClean, 

babbClean <-
  bab_all %>% 
  mutate(DOW = lubridate::wday(SignalDate, abbr = T, label = T),
         hourSeg = cut(hour, c(-Inf,6,8,10,12,14,16,18,20,Inf))) %>% 
  group_by(DOW, SignalDate,location_name, hourSeg) %>% 
  summarise(TotalD = sum(TotalDevices)) %>% 
  ungroup() %>% 
  group_by(DOW, SignalDate,location_name) %>% 
  mutate(PerDay = TotalD/sum(TotalD),
         loc = str_remove_all(location_name, "Tanger Outlets ")) %>% 
  ungroup() %>% 
  mutate(PerDay = as.numeric(PerDay))




babbClean %>% 

  filter(loc == "Foxwoods",
         DOW == "Fri") %>% 
  ggplot(aes(SignalDate, hourSeg, fill=PerDay))+
  geom_tile(color = "white")+
  scale_fill_gradient(low="grey80", high=cb_palette[c(2)], labels = scales::percent) +  
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(expand=c(0,0), date_breaks = "2 months")+
  labs(title = "Average Mobile Signals for: Foxwoods",
       subtitle = "For Fridays",
       y = NULL, x=NULL,
       fill = "% First Signal Seen")+
  theme(axis.text.x = element_text(size = 8),
  )

babbClean %>% 
  filter(loc == "Foxwoods",
         DOW == "Fri") %>% 
  ggplot(aes(SignalDate, hourSeg, fill=TotalD))+
  geom_tile(color = "white")+
  scale_fill_gradient(low="grey80", high=cb_palette[c(2)]) + #, labels = scales::percent  
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(expand=c(0,0), date_breaks = "2 months")+
  labs(title = "Average Mobile Signals for: Foxwoods",
       subtitle = "For Fridays",
       y = NULL, x=NULL,
       fill = "% First Signal Seen")+
  theme(axis.text.x = element_text(size = 8),
  )




library(plotly)

(p1 <- babbClean %>% 
  filter(loc == "Phoenix Glendale", #"Phoenix Glendale",
         DOW == "Thu") %>% 
  ggplot(aes(SignalDate, hourSeg, fill=TotalD))+
  geom_tile(color = "white")+
  scale_fill_gradient(low="grey80", high=cb_palette[c(2)]) + #, labels = scales::percent  
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(expand=c(0,0), date_breaks = "2 months")+
  labs(title = "Average Mobile Signals for: Westgate || Fridays",
       # subtitle = "For Fridays",
       y = NULL, x=NULL,
       fill = "% First Signal Seen")+
  theme(axis.text.x = element_text(size = 8),
  )
)


ggplotly(p1)

#-------------

dat <- 
  bab_all %>% 
  mutate(loc = str_remove_all(location_name, "Tanger Outlets ")) %>% 
  filter(loc == "Blowing Rock") %>% 
  mutate(DOW = lubridate::wday(SignalDate, abbr = T, label = T),
         TraffYear = lubridate::year(SignalDate)) %>% 
  group_by(DOW, loc, hour, TraffYear) %>% 
  summarise(TotalD = sum(TotalDevices)) %>% 
  ungroup() %>% 
  group_by(loc, DOW, TraffYear) %>% 
  mutate(PerDay = TotalD/sum(TotalD)) %>% 
  ungroup() %>% 
  mutate(PerDay = as.numeric(PerDay))


dat %>%
  filter(hour >6,
         hour < 21) %>% 
  ggplot(aes(DOW, hour, fill=PerDay))+
  geom_tile(color = "white")+
  geom_text(aes(label = scales::percent(PerDay, accuracy = .1)), size = 3)+
  scale_fill_gradient(low="grey90", high=cb_palette[c(3)], labels = scales::percent) +  
  scale_y_continuous(expand=c(0,0), breaks = seq(6,21,1))+
  scale_x_discrete(expand=c(0,0))+
  labs(title = "Average Mobile Signals for: Blowing Rock",
       # subtitle = "For Mondays",
       y = "Hour of Day", x=NULL,
       fill = "% First Signal Seen")+
  theme(axis.text.x = element_text(size = 8),
  )+
  facet_wrap(TraffYear ~., ncol =3)




