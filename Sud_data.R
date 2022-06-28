library(tidyverse) 
library(gsheet)
library(lubridate)
library(data.table)
library(readxl)
library(ggpubr)
library(broom)
library(AICcmodavg)

url<-"https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true"
sud <-gsheet2tbl(url)

sud<-sud%>%
  rename(Date = `Date Values Reflect`)

sud<-sud %>%
  filter(Date != "MISSING DATA" | Date != "NO POWER - AERATOR INSTALL")

library(lubridate)
sud2 <- sud
sud2$Date <- mdy(sud$Date)

sud<-sud2%>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])%>%
  mutate(year = year(Date))


sud3 <- sud2 %>% 
  select(Date, `Timestamp*`, `Air temp Avg (C)`, `Air Temp Max (C)`,`Air Temp Min`, `Solar Total (MJ/m²)` )

sud3 <- sud3 %>% 
  mutate(year = year(Dates))
sud3 <- sud3 %>% 
  mutate(months = month(Dates))

avgC <- sud3 %>% 
  filter(year == 2021) %>% 
  filter(air_temp_avg_C >= 0) %>% 
  group_by(months) %>% 
  summarise(avgC = mean(air_temp_avg_C)) %>% 
  mutate(months = recode_factor(.x = months,
                        "1" = 'January', 
                        "2" = 'February', 
                        "3" = 'March', 
                        "4" = 'April', 
                        "5" = 'May', 
                        "6" = 'June', 
                        "7" = 'July',
                        "8" = 'August', 
                        "9" = 'September', 
                        "10" = 'October', 
                        "11" = 'November', 
                        "12" = 'December'))
ggplot(data = avgC, aes(x = months, y = avgC))+
  geom_point()+
  geom_line(group=1)+
  theme(axis.text.x  = element_text(angle = 90))







