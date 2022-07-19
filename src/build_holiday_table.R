# pull holidays to be able to join against
# Fri Feb 18 16:06:35 2022 ------------------------------
library(DBI)
library(glue)
library(tidyverse)
library(lubridate)

holi <- prophet::generated_holidays %>% 
  filter(year >= 2019,
         year <= 2021,
         country == "US")


#-------------

server <- "appsql-prod.database.windows.net"
database = "retailerhub"
con <- DBI::dbConnect(odbc::odbc(), 
                      UID = "pdwalker@tangeroutlet.com", # rstudioapi::askForPassword("pdwalker"),
                      Driver="ODBC Driver 17 for SQL Server",
                      Server = server, Database = database,
                      Authentication = "ActiveDirectoryInteractive")


pull_sql <- glue_sql(
  "
SELECT a.centerid, a.storeid, c.centername, c.BldgID, StartDay, EndDay, openhour, CloseHour, OrderNum
  FROM [dbo].[Center_RegularHours] a
  left join dbo.Center_RegularHoursDetail b on a.CenterRegularHoursID = b.[CenterRegularHoursID]
  left join dbo.centers c on a.CenterID=c.CenterID
  where c.StatusID =1
  and c.WebVisible =1 
  --and a.centerid = 15
  and a.StoreID is null
  order by a.CenterID, b.OrderNum
  ",
.con = con)

parm_sql <- dbSendQuery(con, pull_sql)
centerhours <- dbFetch(parm_sql)

dbClearResult(parm_sql)

save(centerhours, file = here::here("data", "centerhours.rds"))

#--------

DOW <- 1:7

library(fuzzyjoin)

hrs_expnd <- centerhours %>% 
  expand(BldgID, DOW) %>% 
  mutate(DOW = as.numeric(DOW))

cntrClean <- centerhours %>% 
  mutate(StartDay = as.numeric(StartDay), EndDay = as.numeric(EndDay)) %>% 
  as_tibble()



hrs_all <- 
  sqldf::sqldf("SELECT a.BldgID, a.DOW, b.StartDay, b.EndDay, b.Openhour, b.closehour
      FROM hrs_expnd a
      LEFT JOIN cntrClean b 
        on a.BldgID= b.BldgID 
        AND a.DOW >= b.StartDay AND a.DOW <= b.EndDay") %>%
  as_tibble() %>% 
  mutate(Open = as.numeric(substr(openhour,1,2)),
         Close = as.numeric(substr(CloseHour,1,2)))



save(hrs_all, file = here::here("data", "hrs_all.rds"))






  

