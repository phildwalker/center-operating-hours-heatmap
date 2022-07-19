# pull traffic / closed hours -- map back to signals
# Tue Feb 22 12:59:42 2022 ------------------------------

library(odbc)
library(DBI)
library(glue)
library(tidyverse)

# ----------
centerLk <- readxl::read_excel(here::here("input", "center_lkup.xlsx"), "center_lkup") %>% 
  select(CENTER_ID, BldgGroupID, BldgGroupName, Group, Region) %>%
  ungroup()

# ---------------
OpsStats <- DBI::dbConnect(odbc::odbc(), dsn = "OpsStats") #"OpsStats-UAT"

pull_sql <- glue_sql(
  "
with traff as (
select countdate, trafficcount as traffic, trafficcount as vehicleTraffic, 0 as peopletraffic, centerid, conditions, modifiedon, closurestart, closureend, closurereason
FROM [OpsStats].[dbo].[tblTraffic] 
where year(countDate) >= '2018'
and year(countDate) <= '2022'
--and countDate < GetDate() -1
and ModifiedOn is not null
and CenterID not in (104) -- removed foxwoods data because shouldn't be captured in this table
UNION ALL

select countdate, trafficcount/3 as traffic, 0 as vehicleTraffic, trafficCount as peopletraffic, centerid, conditions, modifiedon, NULL as closurestart, NULL as closureend, '' as closurereason
FROM [OpsStats].[dbo].[tblPeopleTraffic] 
where year(countDate) >= '2018'
and year(countDate) <= '2021'
--and countDate < GetDate() -1
and centerid = 104 --Only pull foxwoods people traffic
)

select t.*, c.entityid, c.description as CenterLocation, c.closed, c.traffictype
FROM traff t left join
   OpsStats.dbo.tblCenter C on t.CenterID = C.CenterID
where c.active = 1
--and t.centerid in (6, 104) --6: Gonzales, 104: Foxwoods
  --and TrafficCount > 0
order by t.centerid, countdate desc

  ",
.con = OpsStats)

parm_sql <- dbSendQuery(OpsStats, pull_sql)
traffic <- dbFetch(parm_sql)

dbClearResult(parm_sql)

#-----------


trafficClean <- 
  traffic %>% 
  left_join(., 
            centerLk, by = c("entityid" = "CENTER_ID")) %>% 
  filter(!is.na(BldgGroupName)) %>% 
  mutate(Date = as.Date(countdate),
         month = lubridate::ceiling_date(Date, "month")-1) %>% 
  mutate(closureHapp = ifelse(is.na(closurestart), F, T)) %>% 
  # mutate(closeHrSt = lubridate::hour(closureend) + lubridate::minute(closureend)/60) %>% 
  mutate(CloseHr = as.numeric((lubridate::hour(closureend) + lubridate::minute(closureend)/60) - (lubridate::hour(closurestart) + lubridate::minute(closurestart)/60)),
         CloseHr = ifelse(CloseHr < 0, CloseHr*-1, CloseHr),
         CloseFlg = ifelse(is.na(closurestart), "Open", "Partial Close")) %>% 
  filter(!BldgGroupName %in% c("Atlantic City Outlet Center")) %>% 
  ungroup() %>% 
  mutate(traffic = as.numeric(traffic)) %>% 
  ungroup() %>% 
  select(BldgGroupID, BldgGroupName, Group, Region, Date, traffic, closurestart, closureend, CloseHr, CloseFlg) %>% 
  mutate(CLOstart = lubridate::hour(closurestart),
         CLOend = lubridate::hour(closureend))



# unique(trafficClean$BldgGroupID)

save(trafficClean, file = here::here("data", "trafficClean.rds"))
